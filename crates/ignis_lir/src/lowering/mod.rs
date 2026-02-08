mod builder;

use std::collections::{HashMap, HashSet};

use ignis_hir::{DropSchedules, ExitKey, HIR, HIRId, HIRKind, statement::LoopKind};
use ignis_type::{
  definition::{DefinitionId, DefinitionKind, DefinitionStore, InlineMode},
  module::ModuleId,
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
  /// Synthetic stack depth when entering the loop.
  /// Used to drop temps created inside the loop on break/continue.
  synthetic_stack_depth: usize,
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

  /// Current function definition ID (for FnEnd drops).
  current_fn_def_id: Option<DefinitionId>,

  /// Mapping from HIR definitions to LIR locals.
  def_to_local: HashMap<DefinitionId, LocalId>,

  /// Loop context stack for break/continue.
  loop_stack: Vec<LoopContext>,

  /// Drop schedules from ownership analysis.
  drop_schedules: &'a DropSchedules,

  /// Optional set of modules to emit. If None, emit all.
  /// If Some, only emit functions from modules in the set;
  /// functions from other modules are created as extern declarations.
  emit_modules: Option<&'a HashSet<ModuleId>>,

  /// Stack of synthetic owned temps per block.
  /// Each entry is (block_hir_id, Vec<LocalId>).
  /// Used to emit drops for owned temporaries that are not tracked by DropSchedules.
  synthetic_owned_stack: Vec<(HIRId, Vec<LocalId>)>,

  /// Temps loaded from locals (via `Instr::Load`). These are aliases and should
  /// not be spilled for synthetic drops - their source local has its own schedule.
  load_alias_temps: HashSet<TempId>,
}

impl<'a> LoweringContext<'a> {
  pub fn new(
    hir: &'a HIR,
    types: &'a mut TypeStore,
    defs: &'a DefinitionStore,
    symbols: &'a SymbolTable,
    drop_schedules: &'a DropSchedules,
    emit_modules: Option<&'a HashSet<ModuleId>>,
  ) -> Self {
    Self {
      hir,
      types,
      defs,
      symbols,
      program: LirProgram::new(),
      current_fn: None,
      current_fn_def_id: None,
      def_to_local: HashMap::new(),
      loop_stack: Vec::new(),
      drop_schedules,
      emit_modules,
      synthetic_owned_stack: Vec::new(),
      load_alias_temps: HashSet::new(),
    }
  }

  /// Lower the entire HIR to LIR.
  pub fn lower(mut self) -> LirProgram {
    self.program.entry_point = self.hir.entry_point;

    for &def_id in &self.hir.items {
      let def = self.defs.get(&def_id);

      // Filter by module if emit_modules is specified
      if let Some(emit_set) = &self.emit_modules
        && !emit_set.contains(&def.owner_module)
      {
        // Not in emit set: register as extern if it's a function or method
        match &def.kind {
          DefinitionKind::Function(_) | DefinitionKind::Method(_) => {
            self.create_extern_function(def_id);
          },
          _ => {},
        }
        continue;
      }

      match &def.kind {
        DefinitionKind::Function(func_def) => {
          if let Some(&body_id) = self.hir.function_bodies.get(&def_id) {
            self.lower_function(def_id, body_id);
          } else if func_def.is_extern {
            self.create_extern_function(def_id);
          }
        },
        DefinitionKind::Method(_) => {
          if let Some(&body_id) = self.hir.function_bodies.get(&def_id) {
            self.lower_method(def_id, body_id);
          }
        },
        _ => {},
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
      func_def.inline_mode,
      def.span.clone(),
    );

    self.current_fn = Some(builder);
    self.current_fn_def_id = Some(def_id);
    self.def_to_local.clear();
    self.loop_stack.clear();
    self.synthetic_owned_stack.clear();
    self.load_alias_temps.clear();

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

  fn lower_method(
    &mut self,
    def_id: DefinitionId,
    body_id: HIRId,
  ) {
    let def = self.defs.get(&def_id);
    let method_def = match &def.kind {
      DefinitionKind::Method(m) => m.clone(),
      _ => return,
    };

    let builder = FunctionBuilder::new(
      def_id,
      method_def.params.clone(),
      method_def.return_type,
      false, // is_extern - methods are never extern
      false, // is_variadic - methods are never variadic
      method_def.inline_mode,
      def.span.clone(),
    );

    self.current_fn = Some(builder);
    self.current_fn_def_id = Some(def_id);
    self.def_to_local.clear();
    self.loop_stack.clear();
    self.synthetic_owned_stack.clear();
    self.load_alias_temps.clear();

    // Allocate locals for parameters and store initial values
    for (idx, &param_id) in method_def.params.iter().enumerate() {
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

    let (params, return_type, is_variadic) = match &def.kind {
      DefinitionKind::Function(f) => (f.params.clone(), f.return_type, f.is_variadic),
      DefinitionKind::Method(m) => (m.params.clone(), m.return_type, false),
      _ => return,
    };

    let builder = FunctionBuilder::new(
      def_id,
      params,
      return_type,
      true, // is_extern
      is_variadic,
      InlineMode::None, // extern functions are never inline
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
      HIRKind::Call {
        callee,
        args,
        type_args: _,
      } => self.lower_call(*callee, args, node.type_id, node.span),
      HIRKind::Cast { expression, target } => self.lower_cast(*expression, *target, node.span),
      HIRKind::BitCast { expression, target } => self.lower_bitcast(*expression, *target, node.span),
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
        self.lower_assign(hir_id, *target, *value, operation.clone());
        None
      },
      HIRKind::Block { statements, expression } => self.lower_block(hir_id, statements, expression.as_ref().copied()),
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
        self.lower_break(hir_id);
        None
      },
      HIRKind::Continue => {
        self.lower_continue(hir_id);
        None
      },
      HIRKind::Return(value) => {
        self.lower_return(hir_id, value.as_ref().copied());
        None
      },
      HIRKind::ExpressionStatement(expr) => {
        let expr_node = self.hir.get(*expr);
        let expr_ty = expr_node.type_id;
        if let Some(op) = self.lower_hir_node(*expr) {
          // Spill owned temps so they get dropped at block end
          self.spill_if_owned_temp(op, expr_ty);
        }
        None
      },
      HIRKind::Error => None,
      HIRKind::TypeOf(operand_hir) => self.lower_typeof(*operand_hir, node.type_id, node.span),
      HIRKind::SizeOf(ty) => self.lower_sizeof(*ty, node.type_id, node.span),
      HIRKind::AlignOf(ty) => self.lower_alignof(*ty, node.type_id, node.span),
      HIRKind::MaxOf(ty) => self.lower_maxof(*ty, node.type_id, node.span),
      HIRKind::MinOf(ty) => self.lower_minof(*ty, node.type_id, node.span),
      HIRKind::BuiltinLoad { ty, ptr } => self.lower_builtin_load(*ty, *ptr, node.type_id, node.span),
      HIRKind::BuiltinStore { ty, ptr, value } => {
        self.lower_builtin_store(*ty, *ptr, *value, node.span);
        None
      },

      // Records and enums
      HIRKind::FieldAccess { base, field_index } => {
        self.lower_field_access(*base, *field_index, node.type_id, node.span)
      },
      HIRKind::RecordInit {
        record_def,
        fields,
        type_args: _,
      } => self.lower_record_init(*record_def, fields, node.type_id, node.span),
      HIRKind::MethodCall {
        receiver,
        method,
        args,
        type_args: _,
      } => self.lower_method_call(receiver.as_ref().copied(), *method, args, node.type_id, node.span),
      HIRKind::EnumVariant {
        enum_def,
        variant_tag,
        payload,
        type_args: _,
      } => self.lower_enum_variant(*enum_def, *variant_tag, payload, node.type_id, node.span),
      HIRKind::StaticAccess { def } => self.lower_static_access(*def, node.type_id, node.span),

      HIRKind::Panic(msg) => {
        let msg_node = self.hir.get(*msg);
        let message = match &msg_node.kind {
          HIRKind::Literal(IgnisLiteralValue::String(s)) => s.clone(),
          _ => "<non-literal panic>".into(),
        };

        self.fn_builder().emit(Instr::PanicMessage {
          message,
          span: node.span,
        });
        self.fn_builder().terminate(Terminator::Unreachable);
        None
      },
      HIRKind::Trap => {
        self.fn_builder().emit(Instr::Trap { span: node.span });
        self.fn_builder().terminate(Terminator::Unreachable);
        None
      },
      HIRKind::BuiltinUnreachable => {
        self.fn_builder().terminate(Terminator::Unreachable);
        None
      },
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
      self.load_alias_temps.insert(temp);
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
    let is_extern = self.is_callee_extern(callee);

    // Extern calls don't consume ownership - spill owned temps so caller drops them.
    // Non-extern calls transfer ownership to callee.
    let arg_ops: Vec<_> = args
      .iter()
      .filter_map(|&arg| {
        let arg_node = self.hir.get(arg);
        let arg_ty = arg_node.type_id;
        let op = self.lower_hir_node(arg)?;

        if is_extern {
          Some(self.spill_if_owned_temp(op, arg_ty))
        } else {
          Some(op)
        }
      })
      .collect();

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

  fn lower_bitcast(
    &mut self,
    expr: HIRId,
    target: TypeId,
    span: Span,
  ) -> Option<Operand> {
    let source = self.lower_hir_node(expr)?;
    let dest = self.fn_builder().alloc_temp(target, span);

    self.fn_builder().emit(Instr::BitCast {
      dest,
      source,
      target_type: target,
    });

    Some(Operand::Temp(dest))
  }

  fn lower_typeof(
    &mut self,
    operand_hir: HIRId,
    result_ty: TypeId,
    span: Span,
  ) -> Option<Operand> {
    let operand_node = self.hir.get(operand_hir);
    let base_type = self.unwrap_reference_type(operand_node.type_id);
    let dest = self.fn_builder().alloc_temp(result_ty, span);

    if matches!(self.types.get(&base_type), Type::Infer) {
      panic!("ICE: typeOf on inferred type reached LIR lowering");
    }

    let type_id = self.type_to_runtime_id(base_type);
    self.fn_builder().emit(Instr::Copy {
      dest,
      source: Operand::Const(ConstValue::UInt(type_id, result_ty)),
    });

    Some(Operand::Temp(dest))
  }

  fn lower_sizeof(
    &mut self,
    ty: TypeId,
    result_ty: TypeId,
    span: Span,
  ) -> Option<Operand> {
    let dest = self.fn_builder().alloc_temp(result_ty, span);

    self.fn_builder().emit(Instr::SizeOf { dest, ty });

    Some(Operand::Temp(dest))
  }

  fn lower_alignof(
    &mut self,
    ty: TypeId,
    result_ty: TypeId,
    span: Span,
  ) -> Option<Operand> {
    let dest = self.fn_builder().alloc_temp(result_ty, span);

    self.fn_builder().emit(Instr::AlignOf { dest, ty });

    Some(Operand::Temp(dest))
  }

  fn lower_maxof(
    &mut self,
    ty: TypeId,
    result_ty: TypeId,
    span: Span,
  ) -> Option<Operand> {
    let dest = self.fn_builder().alloc_temp(result_ty, span);

    self.fn_builder().emit(Instr::MaxOf { dest, ty });

    Some(Operand::Temp(dest))
  }

  fn lower_minof(
    &mut self,
    ty: TypeId,
    result_ty: TypeId,
    span: Span,
  ) -> Option<Operand> {
    let dest = self.fn_builder().alloc_temp(result_ty, span);

    self.fn_builder().emit(Instr::MinOf { dest, ty });

    Some(Operand::Temp(dest))
  }

  fn lower_builtin_load(
    &mut self,
    ty: TypeId,
    ptr: HIRId,
    result_ty: TypeId,
    span: Span,
  ) -> Option<Operand> {
    let ptr_op = self.lower_hir_node(ptr)?;
    let dest = self.fn_builder().alloc_temp(result_ty, span);

    self.fn_builder().emit(Instr::BuiltinLoad { dest, ptr: ptr_op, ty });

    Some(Operand::Temp(dest))
  }

  fn lower_builtin_store(
    &mut self,
    ty: TypeId,
    ptr: HIRId,
    value: HIRId,
    span: Span,
  ) {
    let ptr_op = match self.lower_hir_node(ptr) {
      Some(op) => op,
      None => return,
    };

    let value_op = match self.lower_hir_node(value) {
      Some(op) => op,
      None => return,
    };

    let _ = span;

    self.fn_builder().emit(Instr::BuiltinStore {
      ptr: ptr_op,
      value: value_op,
      ty,
    });
  }

  fn unwrap_reference_type(
    &self,
    ty: TypeId,
  ) -> TypeId {
    match self.types.get(&ty) {
      Type::Reference { inner, .. } => self.unwrap_reference_type(*inner),
      _ => ty,
    }
  }

  /// Map compile-time type to runtime type ID (matches IGNIS_TYPE_*_ID in ignis_rt.h).
  fn type_to_runtime_id(
    &self,
    ty: TypeId,
  ) -> u64 {
    match self.types.get(&ty) {
      Type::I8 => 0,
      Type::I16 => 1,
      Type::I32 => 2,
      Type::I64 => 3,
      Type::U8 => 4,
      Type::U16 => 5,
      Type::U32 => 6,
      Type::U64 => 7,
      Type::F32 => 8,
      Type::F64 => 9,
      Type::Boolean => 10,
      Type::Char => 11,
      Type::String => 12,
      Type::Vector { size: None, .. } => 100,
      Type::Pointer { .. } => 200,
      _ => 0xFFFFFFFF,
    }
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
          Type::Reference { inner, .. } | Type::Pointer { inner, .. } => *inner,
          _ => return None,
        };

        let ptr_ty = self.types.pointer(elem_ty, mutable);
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

    // Get element pointer (mutable for internal use)
    let ptr_ty = self.types.pointer(elem_ty, true);
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

    let elem_ops: Vec<_> = elements.iter().filter_map(|&e| self.lower_hir_node(e)).collect();

    // C array decay: array name becomes pointer to first element
    let ptr_ty = self.types.pointer(elem_ty, true);
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
      // Check for init-in-place opportunity
      let value_node = self.hir.get(value_id);
      if let HIRKind::RecordInit {
        record_def: _,
        fields,
        type_args: _,
      } = &value_node.kind
      {
        // Init record directly into the local
        let field_ops: Vec<(u32, Operand)> = fields
          .iter()
          .filter_map(|(idx, hir_id)| {
            let op = self.lower_hir_node(*hir_id)?;
            Some((*idx, op))
          })
          .collect();

        let ptr_ty = self.types.pointer(ty, true);
        let ptr = self.fn_builder().alloc_temp(ptr_ty, value_node.span.clone());
        self.fn_builder().emit(Instr::AddrOfLocal {
          dest: ptr,
          local,
          mutable: true,
        });

        self.fn_builder().emit(Instr::InitRecord {
          dest_ptr: Operand::Temp(ptr),
          fields: field_ops,
          record_type: ty,
        });
        return;
      }

      // General case
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
    hir_id: HIRId,
    target: HIRId,
    value: HIRId,
    operation: Option<ignis_hir::operation::BinaryOperation>,
  ) {
    self.emit_overwrite_drops(hir_id);

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
          let ptr_ty = self.types.pointer(elem_ty, true);
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

      HIRKind::FieldAccess { base, field_index } => {
        let base_node = self.hir.get(*base).clone();
        let span = target_node.span.clone();

        // Track mutability of the base to propagate to field pointer
        let mut base_is_mutable = true; // Default for value types (spilled to local)

        // Check if base is a Dereference of a pointer/reference.
        // This happens with auto-deref: `self.field` where `self: &mut T` becomes
        // `FieldAccess { base: Dereference(Variable(self)), ... }`.
        // In that case, we should use the inner pointer directly to avoid copying.
        let base_ptr = if let HIRKind::Dereference(inner) = &base_node.kind {
          let inner_node = self.hir.get(*inner);
          let inner_ty = inner_node.type_id;
          if matches!(self.types.get(&inner_ty), Type::Pointer { .. } | Type::Reference { .. }) {
            // Extract mutability from the pointer/reference type
            base_is_mutable = match self.types.get(&inner_ty) {
              Type::Pointer { mutable, .. } | Type::Reference { mutable, .. } => *mutable,
              _ => true,
            };
            // Inner is a pointer - use it directly without dereferencing
            match self.lower_hir_node(*inner) {
              Some(op) => op,
              None => return,
            }
          } else {
            // Inner is not a pointer - evaluate the dereference and spill
            match self.spill_to_local_for_field_assign(*base, span.clone()) {
              Some(op) => op,
              None => return,
            }
          }
        } else {
          // Check if base is already a pointer/reference type
          let base_ty = base_node.type_id;
          let base_is_ptr = matches!(self.types.get(&base_ty), Type::Pointer { .. } | Type::Reference { .. });

          if base_is_ptr {
            // Extract mutability from the pointer/reference type
            base_is_mutable = match self.types.get(&base_ty) {
              Type::Pointer { mutable, .. } | Type::Reference { mutable, .. } => *mutable,
              _ => true,
            };
            // Base is already a pointer - lower and use directly
            match self.lower_hir_node(*base) {
              Some(op) => op,
              None => return,
            }
          } else {
            // Base is a value - spill to local to get addressable storage
            match self.spill_to_local_for_field_assign(*base, span.clone()) {
              Some(op) => op,
              None => return,
            }
          }
        };

        // Get pointer to field with mutability derived from base
        let field_ty = target_node.type_id;
        let field_ptr_ty = self.types.pointer(field_ty, base_is_mutable);
        let field_ptr = self.fn_builder().alloc_temp(field_ptr_ty, span.clone());

        self.fn_builder().emit(Instr::GetFieldPtr {
          dest: field_ptr,
          base: base_ptr,
          field_index: *field_index,
          field_type: field_ty,
        });

        if let Some(op) = operation {
          // Compound assignment: load current, compute, store
          let current = self.fn_builder().alloc_temp(field_ty, span.clone());
          self.fn_builder().emit(Instr::LoadPtr {
            dest: current,
            ptr: Operand::Temp(field_ptr),
          });

          if let Some(rhs) = self.lower_hir_node(value) {
            let result = self.fn_builder().alloc_temp(field_ty, span);
            self.fn_builder().emit(Instr::BinOp {
              dest: result,
              op,
              left: Operand::Temp(current),
              right: rhs,
            });
            self.fn_builder().emit(Instr::StorePtr {
              ptr: Operand::Temp(field_ptr),
              value: Operand::Temp(result),
            });
          }
        } else if let Some(val) = self.lower_hir_node(value) {
          self.fn_builder().emit(Instr::StorePtr {
            ptr: Operand::Temp(field_ptr),
            value: val,
          });
        }
      },

      _ => {
        // Fallback: just evaluate value for side effects
        self.lower_hir_node(value);
      },
    }
  }

  /// Spill a value to a local and return a pointer to that local.
  /// Used for field assignment when the base is a value (not a pointer).
  fn spill_to_local_for_field_assign(
    &mut self,
    base_hir_id: HIRId,
    span: Span,
  ) -> Option<Operand> {
    let base_ty = self.hir.get(base_hir_id).type_id;
    let base_val = self.lower_hir_node(base_hir_id)?;
    let temp_local = self.alloc_synthetic_local(base_ty, true);

    self.fn_builder().emit(Instr::Store {
      dest: temp_local,
      value: base_val,
    });

    let base_ptr_ty = self.types.pointer(base_ty, true);
    let base_ptr_temp = self.fn_builder().alloc_temp(base_ptr_ty, span);
    self.fn_builder().emit(Instr::AddrOfLocal {
      dest: base_ptr_temp,
      local: temp_local,
      mutable: true,
    });

    Some(Operand::Temp(base_ptr_temp))
  }

  fn lower_block(
    &mut self,
    block_hir_id: HIRId,
    statements: &[HIRId],
    expression: Option<HIRId>,
  ) -> Option<Operand> {
    self.synthetic_owned_stack.push((block_hir_id, Vec::new()));

    for &stmt in statements {
      self.lower_hir_node(stmt);

      // Early exit (return/break/continue) already emitted drops
      if self.fn_builder().is_terminated() {
        self.synthetic_owned_stack.pop();
        return None;
      }
    }

    let result = expression.and_then(|expr| self.lower_hir_node(expr));

    if !self.fn_builder().is_terminated() {
      self.emit_scope_end_drops(block_hir_id);
      self.emit_current_synthetic_drops();
    }

    self.synthetic_owned_stack.pop();
    result
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
          synthetic_stack_depth: self.synthetic_owned_stack.len(),
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
          synthetic_stack_depth: self.synthetic_owned_stack.len(),
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
          synthetic_stack_depth: self.synthetic_owned_stack.len(),
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

  fn lower_break(
    &mut self,
    hir_id: HIRId,
  ) {
    let loop_ctx = self.loop_stack.last().expect("break outside of loop").clone();
    self.emit_exit_drops(ExitKey::Break(hir_id));
    self.emit_synthetic_drops_from_depth(loop_ctx.synthetic_stack_depth);
    self.fn_builder().terminate(Terminator::Goto(loop_ctx.break_block));
  }

  fn lower_continue(
    &mut self,
    hir_id: HIRId,
  ) {
    let loop_ctx = self.loop_stack.last().expect("continue outside of loop").clone();
    self.emit_exit_drops(ExitKey::Continue(hir_id));
    self.emit_synthetic_drops_from_depth(loop_ctx.synthetic_stack_depth);
    self.fn_builder().terminate(Terminator::Goto(loop_ctx.continue_block));
  }

  fn lower_return(
    &mut self,
    hir_id: HIRId,
    value: Option<HIRId>,
  ) {
    let val = value.and_then(|v| self.lower_hir_node(v)); // Don't spill - ownership transfers to caller
    self.emit_exit_drops(ExitKey::Return(hir_id));
    self.emit_all_synthetic_drops();
    self.fn_builder().terminate(Terminator::Return(val));
  }

  fn ensure_return(&mut self) {
    if !self.fn_builder().is_terminated() {
      if let Some(fn_def_id) = self.current_fn_def_id {
        self.emit_exit_drops(ExitKey::FnEnd(fn_def_id));
      }

      let ret_ty = self.current_fn.as_ref().unwrap().return_type();
      if matches!(self.types.get(&ret_ty), Type::Void) {
        self.fn_builder().terminate(Terminator::Return(None));
      } else {
        self.fn_builder().terminate(Terminator::Unreachable);
      }
    }
  }

  fn emit_scope_end_drops(
    &mut self,
    block_hir_id: HIRId,
  ) {
    if let Some(drops) = self.drop_schedules.on_scope_end.get(&block_hir_id) {
      for def_id in drops {
        self.emit_drop_for_def(*def_id);
      }
    }
  }

  fn emit_exit_drops(
    &mut self,
    key: ExitKey,
  ) {
    if let Some(drops) = self.drop_schedules.on_exit.get(&key) {
      for def_id in drops {
        self.emit_drop_for_def(*def_id);
      }
    }
  }

  fn emit_overwrite_drops(
    &mut self,
    assign_hir_id: HIRId,
  ) {
    if let Some(drops) = self.drop_schedules.on_overwrite.get(&assign_hir_id) {
      for def_id in drops {
        self.emit_drop_for_def(*def_id);
      }
    }
  }

  fn emit_drop_for_def(
    &mut self,
    def_id: DefinitionId,
  ) {
    if let Some(&local) = self.def_to_local.get(&def_id) {
      self.fn_builder().emit(Instr::Drop { local });
    }
  }

  /// Check if a callee (function or method) is extern.
  /// Extern functions don't consume ownership of their arguments.
  fn is_callee_extern(
    &self,
    callee: DefinitionId,
  ) -> bool {
    let def = self.defs.get(&callee);
    match &def.kind {
      DefinitionKind::Function(f) => f.is_extern,
      DefinitionKind::Method(_) => false, // Methods are never extern
      _ => false,
    }
  }

  // === Synthetic temp spilling for owned temporaries ===

  /// If the operand is a Temp of an owned type, spill it to a synthetic Local.
  /// Returns the operand unchanged if it's not a Temp, doesn't need dropping,
  /// or is a "load alias" (loaded from a local that has its own drop schedule).
  /// The local is registered for dropping at block end.
  fn spill_if_owned_temp(
    &mut self,
    operand: Operand,
    ty: TypeId,
  ) -> Operand {
    let Operand::Temp(temp_id) = operand else {
      return operand;
    };

    if !self.types.needs_drop_with_defs(&ty, self.defs) {
      return operand;
    }

    if self.load_alias_temps.contains(&temp_id) {
      return operand;
    }

    let local = self.fn_builder().alloc_local(LocalData {
      def_id: None,
      ty,
      mutable: false,
      name: None,
    });

    self.fn_builder().emit(Instr::Store {
      dest: local,
      value: operand,
    });

    if let Some((_, locals)) = self.synthetic_owned_stack.last_mut() {
      locals.push(local);
    }

    Operand::Local(local)
  }

  /// Allocate a synthetic local, registering for drop if owned.
  fn alloc_synthetic_local(
    &mut self,
    ty: TypeId,
    mutable: bool,
  ) -> LocalId {
    let local = self.fn_builder().alloc_local(LocalData {
      def_id: None,
      ty,
      mutable,
      name: None,
    });

    if self.types.needs_drop_with_defs(&ty, self.defs)
      && let Some((_, locals)) = self.synthetic_owned_stack.last_mut()
    {
      locals.push(local);
    }

    local
  }

  /// Emit drops for synthetic temps in the current block (for normal block exit).
  fn emit_current_synthetic_drops(&mut self) {
    // Collect locals first to avoid borrow conflict with fn_builder()
    let locals_to_drop: Vec<LocalId> = self
      .synthetic_owned_stack
      .last()
      .map(|(_, locals)| locals.iter().rev().copied().collect())
      .unwrap_or_default();

    for local in locals_to_drop {
      self.fn_builder().emit(Instr::Drop { local });
    }
  }

  /// Emit drops for ALL synthetic temps on the stack (for return).
  fn emit_all_synthetic_drops(&mut self) {
    // Collect locals first to avoid borrow conflict with fn_builder()
    let locals_to_drop: Vec<LocalId> = self
      .synthetic_owned_stack
      .iter()
      .rev()
      .flat_map(|(_, locals)| locals.iter().rev().copied())
      .collect();

    for local in locals_to_drop {
      self.fn_builder().emit(Instr::Drop { local });
    }
  }

  /// Emit drops for synthetic temps from a given stack depth (for break/continue).
  fn emit_synthetic_drops_from_depth(
    &mut self,
    depth: usize,
  ) {
    let locals_to_drop: Vec<LocalId> = self.synthetic_owned_stack[depth..]
      .iter()
      .rev()
      .flat_map(|(_, locals)| locals.iter().rev().copied())
      .collect();

    for local in locals_to_drop {
      self.fn_builder().emit(Instr::Drop { local });
    }
  }

  // === Records and enums lowering ===

  fn lower_field_access(
    &mut self,
    base: HIRId,
    field_index: u32,
    field_ty: TypeId,
    span: Span,
  ) -> Option<Operand> {
    let base_node = self.hir.get(base);
    let base_ty = base_node.type_id;

    // Check if base is already a pointer/reference type
    let base_is_ptr = matches!(self.types.get(&base_ty), Type::Pointer { .. } | Type::Reference { .. });

    let base_ptr = if base_is_ptr {
      // Base is already a pointer - lower and use directly
      self.lower_hir_node(base)?
    } else {
      // Base is a value - spill to local to get addressable storage
      let base_val = self.lower_hir_node(base)?;
      let temp_local = self.alloc_synthetic_local(base_ty, false);

      self.fn_builder().emit(Instr::Store {
        dest: temp_local,
        value: base_val,
      });

      let base_ptr_ty = self.types.pointer(base_ty, false);
      let base_ptr_temp = self.fn_builder().alloc_temp(base_ptr_ty, span.clone());
      self.fn_builder().emit(Instr::AddrOfLocal {
        dest: base_ptr_temp,
        local: temp_local,
        mutable: false,
      });

      Operand::Temp(base_ptr_temp)
    };

    // Get pointer to field
    let field_ptr_ty = self.types.pointer(field_ty, false);
    let field_ptr = self.fn_builder().alloc_temp(field_ptr_ty, span.clone());

    self.fn_builder().emit(Instr::GetFieldPtr {
      dest: field_ptr,
      base: base_ptr,
      field_index,
      field_type: field_ty,
    });

    // Load the field value
    let dest = self.fn_builder().alloc_temp(field_ty, span);
    self.fn_builder().emit(Instr::LoadPtr {
      dest,
      ptr: Operand::Temp(field_ptr),
    });

    Some(Operand::Temp(dest))
  }

  fn lower_record_init(
    &mut self,
    _record_def: DefinitionId,
    fields: &[(u32, HIRId)],
    record_ty: TypeId,
    span: Span,
  ) -> Option<Operand> {
    // Allocate local for the record
    let local = self.fn_builder().alloc_local(LocalData {
      def_id: None,
      ty: record_ty,
      mutable: true,
      name: None,
    });

    // Lower each field value
    let field_ops: Vec<(u32, Operand)> = fields
      .iter()
      .filter_map(|(idx, hir_id)| {
        let op = self.lower_hir_node(*hir_id)?;
        Some((*idx, op))
      })
      .collect();

    // Get pointer to local
    let ptr_ty = self.types.pointer(record_ty, true);
    let ptr = self.fn_builder().alloc_temp(ptr_ty, span.clone());
    self.fn_builder().emit(Instr::AddrOfLocal {
      dest: ptr,
      local,
      mutable: true,
    });

    // Initialize the record
    self.fn_builder().emit(Instr::InitRecord {
      dest_ptr: Operand::Temp(ptr),
      fields: field_ops,
      record_type: record_ty,
    });

    // Load the record value
    let dest = self.fn_builder().alloc_temp(record_ty, span);
    self.fn_builder().emit(Instr::Load { dest, source: local });

    Some(Operand::Temp(dest))
  }

  fn lower_method_call(
    &mut self,
    receiver: Option<HIRId>,
    method: DefinitionId,
    args: &[HIRId],
    result_ty: TypeId,
    span: Span,
  ) -> Option<Operand> {
    // Build argument list: receiver (if any) + args
    // Don't spill arguments - ownership transfers to callee
    let mut call_args = Vec::new();

    if let Some(recv) = receiver
      && let Some(recv_op) = self.lower_hir_node(recv)
    {
      call_args.push(recv_op);
    }

    for &arg in args {
      if let Some(arg_op) = self.lower_hir_node(arg) {
        call_args.push(arg_op);
      }
    }

    // Emit the call
    let is_void = matches!(self.types.get(&result_ty), Type::Void);
    let dest = if is_void {
      None
    } else {
      Some(self.fn_builder().alloc_temp(result_ty, span))
    };

    self.fn_builder().emit(Instr::Call {
      dest,
      callee: method,
      args: call_args,
    });

    dest.map(Operand::Temp)
  }

  fn lower_enum_variant(
    &mut self,
    _enum_def: DefinitionId,
    variant_tag: u32,
    payload: &[HIRId],
    enum_ty: TypeId,
    span: Span,
  ) -> Option<Operand> {
    // Allocate local for the enum
    let local = self.fn_builder().alloc_local(LocalData {
      def_id: None,
      ty: enum_ty,
      mutable: true,
      name: None,
    });

    // Lower payload values
    let payload_ops: Vec<Operand> = payload
      .iter()
      .filter_map(|&hir_id| self.lower_hir_node(hir_id))
      .collect();

    // Get pointer to local
    let ptr_ty = self.types.pointer(enum_ty, true);
    let ptr = self.fn_builder().alloc_temp(ptr_ty, span.clone());
    self.fn_builder().emit(Instr::AddrOfLocal {
      dest: ptr,
      local,
      mutable: true,
    });

    // Initialize the enum variant
    self.fn_builder().emit(Instr::InitEnumVariant {
      dest_ptr: Operand::Temp(ptr),
      enum_type: enum_ty,
      variant_tag,
      payload: payload_ops,
    });

    // Load the enum value
    let dest = self.fn_builder().alloc_temp(enum_ty, span);
    self.fn_builder().emit(Instr::Load { dest, source: local });

    Some(Operand::Temp(dest))
  }

  fn lower_static_access(
    &mut self,
    def: DefinitionId,
    result_ty: TypeId,
    span: Span,
  ) -> Option<Operand> {
    // Static access is like a variable lookup but for static members
    // For now, treat it as a variable load if we have a local mapping
    if let Some(&local) = self.def_to_local.get(&def) {
      let dest = self.fn_builder().alloc_temp(result_ty, span);
      self.fn_builder().emit(Instr::Load { dest, source: local });
      Some(Operand::Temp(dest))
    } else {
      // Check if this is a constant with a known value
      let def_data = self.defs.get(&def);
      if let DefinitionKind::Constant(const_def) = &def_data.kind
        && let Some(value) = &const_def.value
      {
        // Convert ConstValue from definition to LIR ConstValue
        if let Some(lir_const) = self.definition_const_to_lir_const(value, result_ty) {
          return Some(Operand::Const(lir_const));
        }
      }
      // Static field/method - for functions, just return None (call will handle it)
      None
    }
  }

  fn definition_const_to_lir_const(
    &self,
    value: &ignis_type::definition::ConstValue,
    ty: TypeId,
  ) -> Option<ConstValue> {
    use ignis_type::definition::ConstValue as DefConstValue;
    match value {
      DefConstValue::Int(i) => Some(ConstValue::Int(*i, ty)),
      DefConstValue::Float(f) => Some(ConstValue::Float(*f, ty)),
      DefConstValue::Bool(b) => Some(ConstValue::Bool(*b, ty)),
      DefConstValue::Char(c) => Some(ConstValue::Char(*c, ty)),
      DefConstValue::String(s) => Some(ConstValue::String(s.clone(), ty)),
      DefConstValue::Null => Some(ConstValue::Null(ty)),
      // Arrays and tuples not yet supported in LIR constants
      DefConstValue::Array(_) | DefConstValue::Tuple(_) => None,
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
///
/// If `emit_modules` is Some, only emit function bodies for those modules;
/// other functions become extern declarations.
pub fn lower_hir(
  hir: &HIR,
  types: &mut TypeStore,
  defs: &DefinitionStore,
  symbols: &SymbolTable,
  drop_schedules: &DropSchedules,
  emit_modules: Option<&HashSet<ModuleId>>,
) -> LirProgram {
  LoweringContext::new(hir, types, defs, symbols, drop_schedules, emit_modules).lower()
}

/// Lower HIR to LIR and verify the result.
pub fn lower_and_verify(
  hir: &HIR,
  types: &mut TypeStore,
  defs: &DefinitionStore,
  symbols: &SymbolTable,
  drop_schedules: &DropSchedules,
  emit_modules: Option<&HashSet<ModuleId>>,
) -> (LirProgram, Result<(), Vec<crate::VerifyError>>) {
  let program = lower_hir(hir, types, defs, symbols, drop_schedules, emit_modules);
  let verify_result = crate::verify::verify_lir(&program, types, defs);
  (program, verify_result)
}
