mod builder;

use std::collections::HashMap;

use ignis_hir::{HIR, HIRId, HIRKind, statement::LoopKind};
use ignis_type::{
  definition::{DefinitionId, DefinitionKind, DefinitionStore},
  span::Span,
  symbol::SymbolTable,
  types::{Type, TypeId, TypeStore},
  value::IgnisLiteralValue,
};

use crate::{BlockId, ConstValue, Instr, LirProgram, LocalData, LocalId, Operand, TempId, Terminator};

pub use builder::FunctionBuilder;

/// Context for a loop (for break/continue targets).
#[derive(Debug, Clone)]
struct LoopContext {
  /// Block to jump to on `continue`.
  continue_block: BlockId,
  /// Block to jump to on `break`.
  break_block: BlockId,
}

/// Context for lowering HIR to LIR.
pub struct LoweringContext<'a> {
  /// The HIR being lowered.
  hir: &'a HIR,
  /// Type store from analysis (mutable for pointer type creation).
  types: &'a mut TypeStore,
  /// Definition store from analysis.
  defs: &'a DefinitionStore,
  /// Symbol table for debug names.
  symbols: &'a SymbolTable,

  /// The LIR program being built.
  program: LirProgram,

  /// Current function being lowered.
  current_fn: Option<FunctionBuilder>,

  /// Mapping from HIR definitions to LIR locals.
  def_to_local: HashMap<DefinitionId, LocalId>,

  /// Loop context stack for break/continue.
  loop_stack: Vec<LoopContext>,
}

impl<'a> LoweringContext<'a> {
  pub fn new(
    hir: &'a HIR,
    types: &'a mut TypeStore,
    defs: &'a DefinitionStore,
    symbols: &'a SymbolTable,
  ) -> Self {
    Self {
      hir,
      types,
      defs,
      symbols,
      program: LirProgram::new(),
      current_fn: None,
      def_to_local: HashMap::new(),
      loop_stack: Vec::new(),
    }
  }

  /// Lower the entire HIR to LIR.
  pub fn lower(mut self) -> LirProgram {
    self.program.entry_point = self.hir.entry_point;

    for &def_id in &self.hir.items {
      let def = self.defs.get(&def_id);
      if let DefinitionKind::Function(func_def) = &def.kind {
        if let Some(&body_id) = self.hir.function_bodies.get(&def_id) {
          self.lower_function(def_id, body_id);
        } else if func_def.is_extern {
          self.create_extern_function(def_id);
        }
      }
    }

    self.program
  }

  fn lower_function(
    &mut self,
    def_id: DefinitionId,
    body_id: HIRId,
  ) {
    let def = self.defs.get(&def_id);
    let func_def = match &def.kind {
      DefinitionKind::Function(f) => f.clone(),
      _ => return,
    };

    let builder = FunctionBuilder::new(
      def_id,
      func_def.params.clone(),
      func_def.return_type,
      func_def.is_extern,
      func_def.is_variadic,
      def.span.clone(),
    );

    self.current_fn = Some(builder);
    self.def_to_local.clear();
    self.loop_stack.clear();

    // Allocate locals for parameters and store initial values
    for (idx, &param_id) in func_def.params.iter().enumerate() {
      let param_def = self.defs.get(&param_id);
      let param_ty = *self.defs.type_of(&param_id);
      let param_name = self.symbols.get(&param_def.name).to_string();

      let is_mutable = match &param_def.kind {
        DefinitionKind::Parameter(p) => p.mutable,
        _ => false,
      };

      let local_id = self.fn_builder().alloc_local(LocalData {
        def_id: Some(param_id),
        ty: param_ty,
        mutable: is_mutable,
        name: Some(param_name),
      });

      self.def_to_local.insert(param_id, local_id);

      // Parameters are passed as implicit temps t0, t1, ... that we store into locals
      let param_temp = TempId::new(idx as u32);
      // Register the param temp in the function
      self.fn_builder().register_param_temp(param_ty, def.span.clone());

      self.fn_builder().emit(Instr::Store {
        dest: local_id,
        value: Operand::Temp(param_temp),
      });
    }

    // Lower the body
    self.lower_hir_node(body_id);

    // Ensure function ends with a return if needed
    self.ensure_return();

    // Finalize and store
    let func = self.current_fn.take().unwrap().finish();
    self.program.functions.insert(def_id, func);
  }

  fn create_extern_function(
    &mut self,
    def_id: DefinitionId,
  ) {
    let def = self.defs.get(&def_id);
    let func_def = match &def.kind {
      DefinitionKind::Function(f) => f.clone(),
      _ => return,
    };

    let builder = FunctionBuilder::new(
      def_id,
      func_def.params.clone(),
      func_def.return_type,
      true, // is_extern
      func_def.is_variadic,
      def.span.clone(),
    );

    let func = builder.finish();
    self.program.functions.insert(def_id, func);
  }

  fn fn_builder(&mut self) -> &mut FunctionBuilder {
    self.current_fn.as_mut().expect("no current function")
  }

  /// Lower an HIR node and return the resulting operand (if expression).
  pub fn lower_hir_node(
    &mut self,
    hir_id: HIRId,
  ) -> Option<Operand> {
    let node = self.hir.get(hir_id).clone();

    match &node.kind {
      // Expressions
      HIRKind::Literal(lit) => self.lower_literal(lit, node.type_id),
      HIRKind::Variable(def_id) => self.lower_variable(*def_id, node.type_id),
      HIRKind::Binary { operation, left, right } => {
        self.lower_binary(operation.clone(), *left, *right, node.type_id, node.span)
      },
      HIRKind::Unary { operation, operand } => self.lower_unary(operation.clone(), *operand, node.type_id, node.span),
      HIRKind::Call { callee, args } => self.lower_call(*callee, args, node.type_id, node.span),
      HIRKind::Cast { expression, target } => self.lower_cast(*expression, *target, node.span),
      HIRKind::Reference { expression, mutable } => {
        self.lower_reference(*expression, *mutable, node.type_id, node.span)
      },
      HIRKind::Dereference(expr) => self.lower_dereference(*expr, node.type_id, node.span),
      HIRKind::Index { base, index } => self.lower_index(*base, *index, node.type_id, node.span),
      HIRKind::VectorLiteral { elements } => self.lower_vector_literal(elements, node.type_id, node.span),

      // Statements
      HIRKind::Let { name, value } => {
        self.lower_let(*name, value.as_ref().copied());
        None
      },
      HIRKind::Assign {
        target,
        value,
        operation,
      } => {
        self.lower_assign(*target, *value, operation.clone());
        None
      },
      HIRKind::Block { statements, expression } => self.lower_block(statements, expression.as_ref().copied()),
      HIRKind::If {
        condition,
        then_branch,
        else_branch,
      } => self.lower_if(*condition, *then_branch, else_branch.as_ref().copied(), node.type_id, node.span),
      HIRKind::Loop { condition, body } => {
        self.lower_loop(condition, *body);
        None
      },
      HIRKind::Break => {
        self.lower_break();
        None
      },
      HIRKind::Continue => {
        self.lower_continue();
        None
      },
      HIRKind::Return(value) => {
        self.lower_return(value.as_ref().copied());
        None
      },
      HIRKind::ExpressionStatement(expr) => {
        self.lower_hir_node(*expr);
        None
      },
      HIRKind::Error => None,
    }
  }

  // === Expression lowering ===

  fn lower_literal(
    &mut self,
    lit: &IgnisLiteralValue,
    ty: TypeId,
  ) -> Option<Operand> {
    let const_val = match lit {
      IgnisLiteralValue::Int8(v) => ConstValue::Int(*v as i64, ty),
      IgnisLiteralValue::Int16(v) => ConstValue::Int(*v as i64, ty),
      IgnisLiteralValue::Int32(v) => ConstValue::Int(*v as i64, ty),
      IgnisLiteralValue::Int64(v) => ConstValue::Int(*v, ty),
      IgnisLiteralValue::UnsignedInt8(v) => ConstValue::UInt(*v as u64, ty),
      IgnisLiteralValue::UnsignedInt16(v) => ConstValue::UInt(*v as u64, ty),
      IgnisLiteralValue::UnsignedInt32(v) => ConstValue::UInt(*v as u64, ty),
      IgnisLiteralValue::UnsignedInt64(v) => ConstValue::UInt(*v, ty),
      IgnisLiteralValue::Float32(v) => ConstValue::Float(ordered_float::OrderedFloat(v.0 as f64), ty),
      IgnisLiteralValue::Float64(v) => ConstValue::Float(*v, ty),
      IgnisLiteralValue::Boolean(v) => ConstValue::Bool(*v, ty),
      IgnisLiteralValue::Char(v) => ConstValue::Char(*v, ty),
      IgnisLiteralValue::String(v) => ConstValue::String(v.clone(), ty),
      IgnisLiteralValue::Hex(v) => {
        let val = u64::from_str_radix(v, 16).unwrap_or(0);
        ConstValue::UInt(val, ty)
      },
      IgnisLiteralValue::Binary(v) => {
        let val = u64::from_str_radix(v, 2).unwrap_or(0);
        ConstValue::UInt(val, ty)
      },
      IgnisLiteralValue::Null => ConstValue::Null(ty),
    };

    Some(Operand::Const(const_val))
  }

  fn lower_variable(
    &mut self,
    def_id: DefinitionId,
    ty: TypeId,
  ) -> Option<Operand> {
    // Check if it's a local variable
    if let Some(&local_id) = self.def_to_local.get(&def_id) {
      let temp = self.fn_builder().alloc_temp(ty, Span::default());
      self.fn_builder().emit(Instr::Load {
        dest: temp,
        source: local_id,
      });
      Some(Operand::Temp(temp))
    } else {
      // It's a global reference (constant or function)
      let def = self.defs.get(&def_id);
      match &def.kind {
        DefinitionKind::Function(_) => Some(Operand::FuncRef(def_id)),
        DefinitionKind::Constant(_) => Some(Operand::GlobalRef(def_id)),
        _ => None,
      }
    }
  }

  fn lower_binary(
    &mut self,
    op: ignis_hir::operation::BinaryOperation,
    left: HIRId,
    right: HIRId,
    result_ty: TypeId,
    span: Span,
  ) -> Option<Operand> {
    use ignis_hir::operation::BinaryOperation;

    // Short-circuit evaluation for && and ||
    if matches!(op, BinaryOperation::And | BinaryOperation::Or) {
      return self.lower_short_circuit(op, left, right, result_ty, span);
    }

    let left_op = self.lower_hir_node(left)?;
    let right_op = self.lower_hir_node(right)?;

    let dest = self.fn_builder().alloc_temp(result_ty, span);

    self.fn_builder().emit(Instr::BinOp {
      dest,
      op,
      left: left_op,
      right: right_op,
    });

    Some(Operand::Temp(dest))
  }

  fn lower_short_circuit(
    &mut self,
    op: ignis_hir::operation::BinaryOperation,
    left: HIRId,
    right: HIRId,
    result_ty: TypeId,
    span: Span,
  ) -> Option<Operand> {
    use ignis_hir::operation::BinaryOperation;

    let left_op = self.lower_hir_node(left)?;

    let eval_right_block = self.fn_builder().create_block("sc_right");
    let merge_block = self.fn_builder().create_block("sc_merge");

    // Result local to store the final value
    let result_local = self.fn_builder().alloc_local(LocalData {
      def_id: None,
      ty: result_ty,
      mutable: true,
      name: None,
    });

    // Store left result
    self.fn_builder().emit(Instr::Store {
      dest: result_local,
      value: left_op.clone(),
    });

    // Branch based on operation
    match op {
      BinaryOperation::And => {
        // If left is false, skip right; else evaluate right
        self.fn_builder().terminate(Terminator::Branch {
          condition: left_op,
          then_block: eval_right_block,
          else_block: merge_block,
        });
      },
      BinaryOperation::Or => {
        // If left is true, skip right; else evaluate right
        self.fn_builder().terminate(Terminator::Branch {
          condition: left_op,
          then_block: merge_block,
          else_block: eval_right_block,
        });
      },
      _ => unreachable!(),
    }

    // Evaluate right
    self.fn_builder().switch_to_block(eval_right_block);
    if let Some(right_op) = self.lower_hir_node(right) {
      self.fn_builder().emit(Instr::Store {
        dest: result_local,
        value: right_op,
      });
    }
    if !self.fn_builder().is_terminated() {
      self.fn_builder().terminate(Terminator::Goto(merge_block));
    }

    // Merge block: load result
    self.fn_builder().switch_to_block(merge_block);
    let result_temp = self.fn_builder().alloc_temp(result_ty, span);
    self.fn_builder().emit(Instr::Load {
      dest: result_temp,
      source: result_local,
    });

    Some(Operand::Temp(result_temp))
  }

  fn lower_unary(
    &mut self,
    op: ignis_hir::operation::UnaryOperation,
    operand: HIRId,
    result_ty: TypeId,
    span: Span,
  ) -> Option<Operand> {
    let operand_val = self.lower_hir_node(operand)?;
    let dest = self.fn_builder().alloc_temp(result_ty, span);

    self.fn_builder().emit(Instr::UnaryOp {
      dest,
      op,
      operand: operand_val,
    });

    Some(Operand::Temp(dest))
  }

  fn lower_call(
    &mut self,
    callee: DefinitionId,
    args: &[HIRId],
    result_ty: TypeId,
    span: Span,
  ) -> Option<Operand> {
    let arg_ops: Vec<_> = args.iter().filter_map(|&arg| self.lower_hir_node(arg)).collect();

    let is_void = matches!(self.types.get(&result_ty), Type::Void);

    let dest = if is_void {
      None
    } else {
      Some(self.fn_builder().alloc_temp(result_ty, span))
    };

    self.fn_builder().emit(Instr::Call {
      dest,
      callee,
      args: arg_ops,
    });

    dest.map(Operand::Temp)
  }

  fn lower_cast(
    &mut self,
    expr: HIRId,
    target: TypeId,
    span: Span,
  ) -> Option<Operand> {
    let source = self.lower_hir_node(expr)?;
    let dest = self.fn_builder().alloc_temp(target, span);

    self.fn_builder().emit(Instr::Cast {
      dest,
      source,
      target_type: target,
    });

    Some(Operand::Temp(dest))
  }

  fn lower_reference(
    &mut self,
    expr: HIRId,
    mutable: bool,
    ref_ty: TypeId,
    span: Span,
  ) -> Option<Operand> {
    let node = self.hir.get(expr);

    // Handle different expression kinds that can be referenced
    match &node.kind {
      // Simple variable: &var → AddrOfLocal
      HIRKind::Variable(def_id) => {
        if let Some(&local_id) = self.def_to_local.get(def_id) {
          let dest = self.fn_builder().alloc_temp(ref_ty, span);
          self.fn_builder().emit(Instr::AddrOfLocal {
            dest,
            local: local_id,
            mutable,
          });
          Some(Operand::Temp(dest))
        } else {
          None
        }
      },

      // Index expression: &arr[i] → GEP
      HIRKind::Index { base, index } => {
        let base = *base;
        let index = *index;

        let base_op = self.lower_hir_node(base)?;
        let index_op = self.lower_hir_node(index)?;

        // Get element type from the reference type (ref_ty is &elem_ty)
        let elem_ty = match self.types.get(&ref_ty) {
          Type::Reference { inner, .. } | Type::Pointer(inner) => *inner,
          _ => return None,
        };

        let ptr_ty = self.types.pointer(elem_ty);
        let dest = self.fn_builder().alloc_temp(ptr_ty, span);

        self.fn_builder().emit(Instr::GetElementPtr {
          dest,
          base: base_op,
          index: index_op,
          element_type: elem_ty,
        });

        Some(Operand::Temp(dest))
      },

      // Dereference: &(*ptr) → ptr (the pointer itself is the address)
      HIRKind::Dereference(inner) => {
        let inner = *inner;
        self.lower_hir_node(inner)
      },

      // Other expressions not supported as lvalues
      _ => None,
    }
  }

  fn lower_dereference(
    &mut self,
    expr: HIRId,
    result_ty: TypeId,
    span: Span,
  ) -> Option<Operand> {
    let ptr = self.lower_hir_node(expr)?;
    let dest = self.fn_builder().alloc_temp(result_ty, span);

    self.fn_builder().emit(Instr::LoadPtr { dest, ptr });

    Some(Operand::Temp(dest))
  }

  fn lower_index(
    &mut self,
    base: HIRId,
    index: HIRId,
    elem_ty: TypeId,
    span: Span,
  ) -> Option<Operand> {
    let base_op = self.lower_hir_node(base)?;
    let index_op = self.lower_hir_node(index)?;

    // Get element pointer
    let ptr_ty = self.types.pointer(elem_ty);
    let ptr = self.fn_builder().alloc_temp(ptr_ty, span.clone());

    self.fn_builder().emit(Instr::GetElementPtr {
      dest: ptr,
      base: base_op,
      index: index_op,
      element_type: elem_ty,
    });

    // Load from pointer
    let dest = self.fn_builder().alloc_temp(elem_ty, span);
    self.fn_builder().emit(Instr::LoadPtr {
      dest,
      ptr: Operand::Temp(ptr),
    });

    Some(Operand::Temp(dest))
  }

  fn lower_vector_literal(
    &mut self,
    elements: &[HIRId],
    vec_ty: TypeId,
    span: Span,
  ) -> Option<Operand> {
    // Allocate space for the vector
    let local = self.fn_builder().alloc_local(LocalData {
      def_id: None,
      ty: vec_ty,
      mutable: true,
      name: None,
    });

    // Get element type
    let elem_ty = match self.types.get(&vec_ty) {
      Type::Vector { element, .. } => *element,
      _ => return None,
    };

    // Lower all elements
    let elem_ops: Vec<_> = elements.iter().filter_map(|&e| self.lower_hir_node(e)).collect();

    // Get address of local
    let ptr_ty = self.types.pointer(vec_ty);
    let ptr = self.fn_builder().alloc_temp(ptr_ty, span.clone());
    self.fn_builder().emit(Instr::AddrOfLocal {
      dest: ptr,
      local,
      mutable: true,
    });

    // Initialize the vector
    self.fn_builder().emit(Instr::InitVector {
      dest_ptr: Operand::Temp(ptr),
      elements: elem_ops,
      element_type: elem_ty,
    });

    // Load the vector value
    let dest = self.fn_builder().alloc_temp(vec_ty, span);
    self.fn_builder().emit(Instr::Load { dest, source: local });

    Some(Operand::Temp(dest))
  }

  // === Statement lowering ===

  fn lower_let(
    &mut self,
    name: DefinitionId,
    value: Option<HIRId>,
  ) {
    let def = self.defs.get(&name);
    let ty = *self.defs.type_of(&name);

    let is_mutable = match &def.kind {
      DefinitionKind::Variable(v) => v.mutable,
      DefinitionKind::Constant(_) => false,
      _ => false,
    };

    let local_name = self.symbols.get(&def.name).to_string();

    // Allocate local slot
    let local = self.fn_builder().alloc_local(LocalData {
      def_id: Some(name),
      ty,
      mutable: is_mutable,
      name: Some(local_name),
    });

    self.def_to_local.insert(name, local);

    // Initialize if there's a value
    if let Some(value_id) = value {
      if let Some(val) = self.lower_hir_node(value_id) {
        self.fn_builder().emit(Instr::Store {
          dest: local,
          value: val,
        });
      }
    }
  }

  fn lower_assign(
    &mut self,
    target: HIRId,
    value: HIRId,
    operation: Option<ignis_hir::operation::BinaryOperation>,
  ) {
    // Try to get the local for direct assignment
    if let Some(local) = self.lower_to_local(target) {
      if let Some(op) = operation {
        // Compound assignment: target op= value
        let ty = self.fn_builder().local_type(local);

        // Load current value
        let current = self.fn_builder().alloc_temp(ty, Span::default());
        self.fn_builder().emit(Instr::Load {
          dest: current,
          source: local,
        });

        // Compute new value
        if let Some(rhs_op) = self.lower_hir_node(value) {
          let result = self.fn_builder().alloc_temp(ty, Span::default());
          self.fn_builder().emit(Instr::BinOp {
            dest: result,
            op,
            left: Operand::Temp(current),
            right: rhs_op,
          });

          self.fn_builder().emit(Instr::Store {
            dest: local,
            value: Operand::Temp(result),
          });
        }
      } else {
        // Simple assignment
        if let Some(val) = self.lower_hir_node(value) {
          self.fn_builder().emit(Instr::Store {
            dest: local,
            value: val,
          });
        }
      }
    } else {
      // Complex assignment (dereference, index)
      self.lower_complex_assign(target, value, operation);
    }
  }

  fn lower_complex_assign(
    &mut self,
    target: HIRId,
    value: HIRId,
    operation: Option<ignis_hir::operation::BinaryOperation>,
  ) {
    let target_node = self.hir.get(target).clone();

    match &target_node.kind {
      HIRKind::Dereference(inner) => {
        let ptr = self.lower_hir_node(*inner);
        if let Some(ptr_op) = ptr {
          if let Some(op) = operation {
            // Compound: load current, compute, store
            let elem_ty = target_node.type_id;
            let current = self.fn_builder().alloc_temp(elem_ty, Span::default());
            self.fn_builder().emit(Instr::LoadPtr {
              dest: current,
              ptr: ptr_op.clone(),
            });

            if let Some(rhs) = self.lower_hir_node(value) {
              let result = self.fn_builder().alloc_temp(elem_ty, Span::default());
              self.fn_builder().emit(Instr::BinOp {
                dest: result,
                op,
                left: Operand::Temp(current),
                right: rhs,
              });
              self.fn_builder().emit(Instr::StorePtr {
                ptr: ptr_op,
                value: Operand::Temp(result),
              });
            }
          } else if let Some(val) = self.lower_hir_node(value) {
            self.fn_builder().emit(Instr::StorePtr {
              ptr: ptr_op,
              value: val,
            });
          }
        }
      },
      HIRKind::Index { base, index } => {
        let base_op = self.lower_hir_node(*base);
        let index_op = self.lower_hir_node(*index);

        if let (Some(base_val), Some(idx_val)) = (base_op, index_op) {
          let elem_ty = target_node.type_id;
          let ptr_ty = self.types.pointer(elem_ty);
          let ptr = self.fn_builder().alloc_temp(ptr_ty, Span::default());

          self.fn_builder().emit(Instr::GetElementPtr {
            dest: ptr,
            base: base_val,
            index: idx_val,
            element_type: elem_ty,
          });

          if let Some(op) = operation {
            // Compound: load current, compute, store
            let current = self.fn_builder().alloc_temp(elem_ty, Span::default());
            self.fn_builder().emit(Instr::LoadPtr {
              dest: current,
              ptr: Operand::Temp(ptr),
            });

            if let Some(rhs) = self.lower_hir_node(value) {
              let result = self.fn_builder().alloc_temp(elem_ty, Span::default());
              self.fn_builder().emit(Instr::BinOp {
                dest: result,
                op,
                left: Operand::Temp(current),
                right: rhs,
              });
              self.fn_builder().emit(Instr::StorePtr {
                ptr: Operand::Temp(ptr),
                value: Operand::Temp(result),
              });
            }
          } else if let Some(val) = self.lower_hir_node(value) {
            self.fn_builder().emit(Instr::StorePtr {
              ptr: Operand::Temp(ptr),
              value: val,
            });
          }
        }
      },
      _ => {
        // Fallback: just evaluate value for side effects
        self.lower_hir_node(value);
      },
    }
  }

  fn lower_block(
    &mut self,
    statements: &[HIRId],
    expression: Option<HIRId>,
  ) -> Option<Operand> {
    for &stmt in statements {
      self.lower_hir_node(stmt);

      // If block is terminated (e.g., by return), stop
      if self.fn_builder().is_terminated() {
        return None;
      }
    }

    expression.and_then(|expr| self.lower_hir_node(expr))
  }

  fn lower_if(
    &mut self,
    condition: HIRId,
    then_branch: HIRId,
    else_branch: Option<HIRId>,
    result_ty: TypeId,
    span: Span,
  ) -> Option<Operand> {
    let cond = self.lower_hir_node(condition)?;

    let then_block = self.fn_builder().create_block("then");
    let else_block = self.fn_builder().create_block("else");
    let merge_block = self.fn_builder().create_block("merge");

    let is_void = matches!(self.types.get(&result_ty), Type::Void);

    // Result local for non-void if-expressions
    let result_local = if !is_void {
      Some(self.fn_builder().alloc_local(LocalData {
        def_id: None,
        ty: result_ty,
        mutable: true,
        name: None,
      }))
    } else {
      None
    };

    // Branch
    self.fn_builder().terminate(Terminator::Branch {
      condition: cond,
      then_block,
      else_block,
    });

    // Then branch
    self.fn_builder().switch_to_block(then_block);
    let then_val = self.lower_hir_node(then_branch);

    if !self.fn_builder().is_terminated() {
      if let (Some(local), Some(val)) = (result_local, then_val) {
        self.fn_builder().emit(Instr::Store {
          dest: local,
          value: val,
        });
      }
      self.fn_builder().terminate(Terminator::Goto(merge_block));
    }

    // Else branch
    self.fn_builder().switch_to_block(else_block);
    if let Some(else_id) = else_branch {
      let else_val = self.lower_hir_node(else_id);

      if !self.fn_builder().is_terminated() {
        if let (Some(local), Some(val)) = (result_local, else_val) {
          self.fn_builder().emit(Instr::Store {
            dest: local,
            value: val,
          });
        }
        self.fn_builder().terminate(Terminator::Goto(merge_block));
      }
    } else {
      self.fn_builder().terminate(Terminator::Goto(merge_block));
    }

    // Merge block
    self.fn_builder().switch_to_block(merge_block);

    // Load result if non-void
    result_local.map(|local| {
      let temp = self.fn_builder().alloc_temp(result_ty, span);
      self.fn_builder().emit(Instr::Load {
        dest: temp,
        source: local,
      });
      Operand::Temp(temp)
    })
  }

  fn lower_loop(
    &mut self,
    condition: &LoopKind,
    body: HIRId,
  ) {
    let exit_block = self.fn_builder().create_block("loop_exit");

    match condition {
      LoopKind::Infinite => {
        // For infinite loops, body_block serves as both body and continue target
        let body_block = self.fn_builder().create_block("loop_body");

        // Push loop context: continue goes back to body start
        self.loop_stack.push(LoopContext {
          continue_block: body_block,
          break_block: exit_block,
        });

        // Jump to body
        self.fn_builder().terminate(Terminator::Goto(body_block));

        // Body
        self.fn_builder().switch_to_block(body_block);
        self.lower_hir_node(body);
        if !self.fn_builder().is_terminated() {
          // Loop back to body start (same as continue target)
          self.fn_builder().terminate(Terminator::Goto(body_block));
        }
      },
      LoopKind::While { condition: cond_id } => {
        let header_block = self.fn_builder().create_block("loop_header");
        let body_block = self.fn_builder().create_block("loop_body");
        let continue_block = self.fn_builder().create_block("loop_continue");

        // Push loop context
        self.loop_stack.push(LoopContext {
          continue_block,
          break_block: exit_block,
        });

        // Go to header
        self.fn_builder().terminate(Terminator::Goto(header_block));

        // Header: evaluate condition
        self.fn_builder().switch_to_block(header_block);
        if let Some(cond) = self.lower_hir_node(*cond_id) {
          self.fn_builder().terminate(Terminator::Branch {
            condition: cond,
            then_block: body_block,
            else_block: exit_block,
          });
        } else {
          // Condition failed to lower, just exit
          self.fn_builder().terminate(Terminator::Goto(exit_block));
        }

        // Body
        self.fn_builder().switch_to_block(body_block);
        self.lower_hir_node(body);
        if !self.fn_builder().is_terminated() {
          self.fn_builder().terminate(Terminator::Goto(continue_block));
        }

        // Continue block (just goes back to header)
        self.fn_builder().switch_to_block(continue_block);
        self.fn_builder().terminate(Terminator::Goto(header_block));
      },
      LoopKind::For {
        init,
        condition: cond,
        update,
      } => {
        let header_block = self.fn_builder().create_block("loop_header");
        let body_block = self.fn_builder().create_block("loop_body");
        let continue_block = self.fn_builder().create_block("loop_continue");

        // Push loop context
        self.loop_stack.push(LoopContext {
          continue_block,
          break_block: exit_block,
        });

        // Init (if present)
        if let Some(init_id) = init {
          self.lower_hir_node(*init_id);
        }

        // Go to header
        if !self.fn_builder().is_terminated() {
          self.fn_builder().terminate(Terminator::Goto(header_block));
        }

        // Header: evaluate condition
        self.fn_builder().switch_to_block(header_block);
        if let Some(cond_id) = cond {
          if let Some(cond_val) = self.lower_hir_node(*cond_id) {
            self.fn_builder().terminate(Terminator::Branch {
              condition: cond_val,
              then_block: body_block,
              else_block: exit_block,
            });
          } else {
            self.fn_builder().terminate(Terminator::Goto(exit_block));
          }
        } else {
          // No condition means infinite loop
          self.fn_builder().terminate(Terminator::Goto(body_block));
        }

        // Body
        self.fn_builder().switch_to_block(body_block);
        self.lower_hir_node(body);
        if !self.fn_builder().is_terminated() {
          self.fn_builder().terminate(Terminator::Goto(continue_block));
        }

        // Continue block: execute update, then go to header
        self.fn_builder().switch_to_block(continue_block);
        if let Some(update_id) = update {
          self.lower_hir_node(*update_id);
        }
        if !self.fn_builder().is_terminated() {
          self.fn_builder().terminate(Terminator::Goto(header_block));
        }
      },
    }

    // Pop loop context
    self.loop_stack.pop();

    // Continue in exit block
    self.fn_builder().switch_to_block(exit_block);
  }

  fn lower_break(&mut self) {
    let loop_ctx = self
      .loop_stack
      .last()
      .expect("break outside of loop - should have been caught by analyzer");
    let break_block = loop_ctx.break_block;
    self.fn_builder().terminate(Terminator::Goto(break_block));
  }

  fn lower_continue(&mut self) {
    let loop_ctx = self
      .loop_stack
      .last()
      .expect("continue outside of loop - should have been caught by analyzer");
    let continue_block = loop_ctx.continue_block;
    self.fn_builder().terminate(Terminator::Goto(continue_block));
  }

  fn lower_return(
    &mut self,
    value: Option<HIRId>,
  ) {
    let val = value.and_then(|v| self.lower_hir_node(v));
    self.fn_builder().terminate(Terminator::Return(val));
  }

  fn ensure_return(&mut self) {
    if !self.fn_builder().is_terminated() {
      let ret_ty = self.current_fn.as_ref().unwrap().return_type();
      if matches!(self.types.get(&ret_ty), Type::Void) {
        self.fn_builder().terminate(Terminator::Return(None));
      } else {
        // Missing return - should have been caught by type checking
        self.fn_builder().terminate(Terminator::Unreachable);
      }
    }
  }

  // === Helpers ===

  /// Try to get the local ID for a simple variable expression.
  fn lower_to_local(
    &self,
    hir_id: HIRId,
  ) -> Option<LocalId> {
    let node = self.hir.get(hir_id);
    match &node.kind {
      HIRKind::Variable(def_id) => self.def_to_local.get(def_id).copied(),
      _ => None,
    }
  }
}

/// Lower HIR to LIR.
pub fn lower_hir(
  hir: &HIR,
  types: &mut TypeStore,
  defs: &DefinitionStore,
  symbols: &SymbolTable,
) -> LirProgram {
  LoweringContext::new(hir, types, defs, symbols).lower()
}

/// Lower HIR to LIR and verify the result.
/// Returns the program and any verification errors.
pub fn lower_and_verify(
  hir: &HIR,
  types: &mut TypeStore,
  defs: &DefinitionStore,
  symbols: &SymbolTable,
) -> (LirProgram, Result<(), Vec<crate::VerifyError>>) {
  let program = lower_hir(hir, types, defs, symbols);
  let verify_result = crate::verify::verify_lir(&program, types);
  (program, verify_result)
}
