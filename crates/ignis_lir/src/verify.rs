use std::collections::HashSet;

use ignis_type::types::{Type, TypeId, TypeStore};

use crate::{BlockId, FunctionLir, Instr, LirProgram, LocalId, Operand, TempId, Terminator};

/// Errors found during LIR verification.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VerifyError {
  /// A block has no terminator set (still Unreachable when it shouldn't be).
  MissingTerminator { function: String, block: String },

  /// Reference to a non-existent block.
  InvalidBlockRef {
    function: String,
    block: String,
    target: BlockId,
  },

  /// Reference to a non-existent local.
  InvalidLocalRef {
    function: String,
    block: String,
    local: LocalId,
  },

  /// Reference to a non-existent temp.
  InvalidTempRef {
    function: String,
    block: String,
    temp: TempId,
  },

  /// Temp used before it was defined.
  TempUsedBeforeDef {
    function: String,
    block: String,
    temp: TempId,
  },

  /// Branch condition is not a boolean type.
  NonBoolBranchCondition {
    function: String,
    block: String,
    actual_type: TypeId,
  },

  /// Return value type doesn't match function signature.
  ReturnTypeMismatch {
    function: String,
    block: String,
    expected: TypeId,
    actual: TypeId,
  },

  /// Missing return value when function expects one.
  MissingReturnValue {
    function: String,
    block: String,
    expected: TypeId,
  },

  /// Unexpected return value when function returns void.
  UnexpectedReturnValue { function: String, block: String },

  /// Drop instruction on a non-droppable type.
  DropNonDroppable {
    function: String,
    block: String,
    local: LocalId,
    actual_type: TypeId,
  },
}

/// LIR verification result.
pub type VerifyResult = Result<(), Vec<VerifyError>>;

/// Verifier for LIR programs.
pub struct LirVerifier<'a> {
  program: &'a LirProgram,
  types: &'a TypeStore,
  errors: Vec<VerifyError>,
}

impl<'a> LirVerifier<'a> {
  pub fn new(
    program: &'a LirProgram,
    types: &'a TypeStore,
  ) -> Self {
    Self {
      program,
      types,
      errors: Vec::new(),
    }
  }

  /// Verify the entire LIR program.
  pub fn verify(mut self) -> VerifyResult {
    for func in self.program.functions.values() {
      if !func.is_extern {
        self.verify_function(func);
      }
    }

    if self.errors.is_empty() {
      Ok(())
    } else {
      Err(self.errors)
    }
  }

  fn verify_function(
    &mut self,
    func: &FunctionLir,
  ) {
    let func_name = format!("fn_{}", func.def_id.index());

    // Track defined temps for def-before-use analysis
    let mut defined_temps: HashSet<TempId> = HashSet::new();

    // Parameters are implicitly defined as temps t0, t1, ...
    for i in 0..func.params.len() {
      defined_temps.insert(TempId::new(i as u32));
    }

    // Verify each block
    for block in func.blocks.get_all() {
      self.verify_block(func, &func_name, block, &mut defined_temps);
    }
  }

  fn verify_block(
    &mut self,
    func: &FunctionLir,
    func_name: &str,
    block: &crate::Block,
    defined_temps: &mut HashSet<TempId>,
  ) {
    let block_name = block.label.clone();

    // Check each instruction
    for instr in &block.instructions {
      self.verify_instr(func, func_name, &block_name, instr, defined_temps);
    }

    // Check terminator
    self.verify_terminator(func, func_name, &block_name, &block.terminator, defined_temps);
  }

  fn verify_instr(
    &mut self,
    func: &FunctionLir,
    func_name: &str,
    block_name: &str,
    instr: &Instr,
    defined_temps: &mut HashSet<TempId>,
  ) {
    match instr {
      Instr::Load { dest, source } => {
        self.check_local_exists(func, func_name, block_name, *source);
        defined_temps.insert(*dest);
      },
      Instr::Store { dest, value } => {
        self.check_local_exists(func, func_name, block_name, *dest);
        self.check_operand(func, func_name, block_name, value, defined_temps);
      },
      Instr::LoadPtr { dest, ptr } => {
        self.check_operand(func, func_name, block_name, ptr, defined_temps);
        defined_temps.insert(*dest);
      },
      Instr::StorePtr { ptr, value } => {
        self.check_operand(func, func_name, block_name, ptr, defined_temps);
        self.check_operand(func, func_name, block_name, value, defined_temps);
      },
      Instr::BuiltinLoad { dest, ptr, .. } => {
        self.check_operand(func, func_name, block_name, ptr, defined_temps);
        defined_temps.insert(*dest);
      },
      Instr::BuiltinStore { ptr, value, .. } => {
        self.check_operand(func, func_name, block_name, ptr, defined_temps);
        self.check_operand(func, func_name, block_name, value, defined_temps);
      },
      Instr::Copy { dest, source } => {
        self.check_operand(func, func_name, block_name, source, defined_temps);
        defined_temps.insert(*dest);
      },
      Instr::BinOp { dest, left, right, .. } => {
        self.check_operand(func, func_name, block_name, left, defined_temps);
        self.check_operand(func, func_name, block_name, right, defined_temps);
        defined_temps.insert(*dest);
      },
      Instr::UnaryOp { dest, operand, .. } => {
        self.check_operand(func, func_name, block_name, operand, defined_temps);
        defined_temps.insert(*dest);
      },
      Instr::Call { dest, args, .. } => {
        for arg in args {
          self.check_operand(func, func_name, block_name, arg, defined_temps);
        }
        if let Some(d) = dest {
          defined_temps.insert(*d);
        }
      },
      Instr::Cast { dest, source, .. } => {
        self.check_operand(func, func_name, block_name, source, defined_temps);
        defined_temps.insert(*dest);
      },
      Instr::AddrOfLocal { dest, local, .. } => {
        self.check_local_exists(func, func_name, block_name, *local);
        defined_temps.insert(*dest);
      },
      Instr::GetElementPtr { dest, base, index, .. } => {
        self.check_operand(func, func_name, block_name, base, defined_temps);
        self.check_operand(func, func_name, block_name, index, defined_temps);
        defined_temps.insert(*dest);
      },
      Instr::InitVector { dest_ptr, elements, .. } => {
        self.check_operand(func, func_name, block_name, dest_ptr, defined_temps);
        for elem in elements {
          self.check_operand(func, func_name, block_name, elem, defined_temps);
        }
      },
      Instr::Nop => {},
      Instr::RuntimeCall { args, .. } => {
        for arg in args {
          self.check_operand(func, func_name, block_name, arg, defined_temps);
        }
      },
      Instr::TypeIdOf { dest, source } => {
        self.check_operand(func, func_name, block_name, source, defined_temps);
        defined_temps.insert(*dest);
      },
      Instr::SizeOf { dest, .. } => {
        // ty is a TypeId, no operand to check
        defined_temps.insert(*dest);
      },
      Instr::AlignOf { dest, .. } => {
        // ty is a TypeId, no operand to check
        defined_temps.insert(*dest);
      },
      Instr::MaxOf { dest, .. } => {
        // ty is a TypeId, no operand to check
        defined_temps.insert(*dest);
      },
      Instr::MinOf { dest, .. } => {
        // ty is a TypeId, no operand to check
        defined_temps.insert(*dest);
      },
      Instr::Drop { local } => {
        self.check_local_exists(func, func_name, block_name, *local);

        // Verify the local has a droppable type
        if local.index() < func.locals.get_all().len() as u32 {
          let local_data = func.locals.get(local);
          let ty = local_data.ty;
          if !is_droppable(self.types, ty) {
            self.errors.push(VerifyError::DropNonDroppable {
              function: func_name.to_string(),
              block: block_name.to_string(),
              local: *local,
              actual_type: ty,
            });
          }
        }
      },
      Instr::GetFieldPtr { dest, base, .. } => {
        self.check_operand(func, func_name, block_name, base, defined_temps);
        defined_temps.insert(*dest);
      },
      Instr::InitRecord { dest_ptr, fields, .. } => {
        self.check_operand(func, func_name, block_name, dest_ptr, defined_temps);
        for (_, field_value) in fields {
          self.check_operand(func, func_name, block_name, field_value, defined_temps);
        }
      },
      Instr::InitEnumVariant { dest_ptr, payload, .. } => {
        self.check_operand(func, func_name, block_name, dest_ptr, defined_temps);
        for p in payload {
          self.check_operand(func, func_name, block_name, p, defined_temps);
        }
      },
      Instr::Trap { .. } => {},
      Instr::PanicMessage { .. } => {},
    }
  }

  fn verify_terminator(
    &mut self,
    func: &FunctionLir,
    func_name: &str,
    block_name: &str,
    term: &Terminator,
    defined_temps: &HashSet<TempId>,
  ) {
    match term {
      Terminator::Goto(target) => {
        self.check_block_exists(func, func_name, block_name, *target);
      },
      Terminator::Branch {
        condition,
        then_block,
        else_block,
      } => {
        self.check_operand(func, func_name, block_name, condition, defined_temps);
        self.check_block_exists(func, func_name, block_name, *then_block);
        self.check_block_exists(func, func_name, block_name, *else_block);

        // Check that condition is boolean
        if let Some(cond_ty) = self.operand_type(func, condition) {
          let bool_ty = self.types.boolean();
          if cond_ty != bool_ty {
            self.errors.push(VerifyError::NonBoolBranchCondition {
              function: func_name.to_string(),
              block: block_name.to_string(),
              actual_type: cond_ty,
            });
          }
        }
      },
      Terminator::Return(value) => {
        let ret_ty = func.return_type;
        let is_void = matches!(self.types.get(&ret_ty), Type::Void);

        match value {
          Some(val) => {
            self.check_operand(func, func_name, block_name, val, defined_temps);

            if is_void {
              self.errors.push(VerifyError::UnexpectedReturnValue {
                function: func_name.to_string(),
                block: block_name.to_string(),
              });
            } else if let Some(val_ty) = self.operand_type(func, val)
              && val_ty != ret_ty
            {
              if std::env::var("IGNIS_VERBOSE").is_ok() {
                eprintln!(
                  "[LIR_VERIFY] ReturnTypeMismatch in {}: expected {:?} = {:?}, actual {:?} = {:?}",
                  func_name,
                  ret_ty,
                  self.types.get(&ret_ty),
                  val_ty,
                  self.types.get(&val_ty)
                );
              }
              self.errors.push(VerifyError::ReturnTypeMismatch {
                function: func_name.to_string(),
                block: block_name.to_string(),
                expected: ret_ty,
                actual: val_ty,
              });
            }
          },
          None => {
            if !is_void {
              self.errors.push(VerifyError::MissingReturnValue {
                function: func_name.to_string(),
                block: block_name.to_string(),
                expected: ret_ty,
              });
            }
          },
        }
      },
      Terminator::Unreachable => {
        // Unreachable is valid for diverging code paths
      },
    }
  }

  fn check_operand(
    &mut self,
    func: &FunctionLir,
    func_name: &str,
    block_name: &str,
    operand: &Operand,
    defined_temps: &HashSet<TempId>,
  ) {
    match operand {
      Operand::Temp(temp) => {
        self.check_temp_exists(func, func_name, block_name, *temp);
        if !defined_temps.contains(temp) {
          self.errors.push(VerifyError::TempUsedBeforeDef {
            function: func_name.to_string(),
            block: block_name.to_string(),
            temp: *temp,
          });
        }
      },
      Operand::Local(local) => {
        // Verify local exists
        if local.index() >= func.locals.len() as u32 {
          self.errors.push(VerifyError::InvalidLocalRef {
            function: func_name.to_string(),
            block: block_name.to_string(),
            local: *local,
          });
        }
      },
      Operand::Const(_) | Operand::FuncRef(_) | Operand::GlobalRef(_) => {
        // These are always valid
      },
    }
  }

  fn check_block_exists(
    &mut self,
    func: &FunctionLir,
    func_name: &str,
    block_name: &str,
    block_id: BlockId,
  ) {
    if block_id.index() >= func.blocks.get_all().len() as u32 {
      self.errors.push(VerifyError::InvalidBlockRef {
        function: func_name.to_string(),
        block: block_name.to_string(),
        target: block_id,
      });
    }
  }

  fn check_local_exists(
    &mut self,
    func: &FunctionLir,
    func_name: &str,
    block_name: &str,
    local_id: LocalId,
  ) {
    if local_id.index() >= func.locals.get_all().len() as u32 {
      self.errors.push(VerifyError::InvalidLocalRef {
        function: func_name.to_string(),
        block: block_name.to_string(),
        local: local_id,
      });
    }
  }

  fn check_temp_exists(
    &mut self,
    func: &FunctionLir,
    func_name: &str,
    block_name: &str,
    temp_id: TempId,
  ) {
    if temp_id.index() >= func.temps.get_all().len() as u32 {
      self.errors.push(VerifyError::InvalidTempRef {
        function: func_name.to_string(),
        block: block_name.to_string(),
        temp: temp_id,
      });
    }
  }

  fn operand_type(
    &self,
    func: &FunctionLir,
    operand: &Operand,
  ) -> Option<TypeId> {
    match operand {
      Operand::Temp(temp) => {
        if temp.index() < func.temps.get_all().len() as u32 {
          Some(func.temp_type(*temp))
        } else {
          None
        }
      },
      Operand::Local(local) => {
        if local.index() < func.locals.len() as u32 {
          Some(func.locals.get(local).ty)
        } else {
          None
        }
      },
      Operand::Const(c) => Some(self.const_type(c)),
      Operand::FuncRef(_) | Operand::GlobalRef(_) => {
        // Would need definition store to get types; skip for now
        None
      },
    }
  }

  fn const_type(
    &self,
    c: &crate::ConstValue,
  ) -> TypeId {
    match c {
      crate::ConstValue::Int(_, ty) => *ty,
      crate::ConstValue::UInt(_, ty) => *ty,
      crate::ConstValue::Float(_, ty) => *ty,
      crate::ConstValue::Bool(_, ty) => *ty,
      crate::ConstValue::Char(_, ty) => *ty,
      crate::ConstValue::String(_, ty) => *ty,
      crate::ConstValue::Null(ty) => *ty,
      crate::ConstValue::Undef(ty) => *ty,
    }
  }
}

/// Verify a LIR program.
pub fn verify_lir(
  program: &LirProgram,
  types: &TypeStore,
) -> VerifyResult {
  LirVerifier::new(program, types).verify()
}

/// Check if a type requires a drop call.
/// Droppable types: string, dynamic vector (size: None), infer.
pub fn is_droppable(
  types: &TypeStore,
  ty: TypeId,
) -> bool {
  matches!(types.get(&ty), Type::String | Type::Vector { size: None, .. } | Type::Infer)
}
