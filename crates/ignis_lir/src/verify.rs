use std::collections::{HashMap, HashSet};

use ignis_type::{
  definition::{DefinitionId, DefinitionStore},
  span::Span,
  types::{Type, TypeId, TypeStore},
};

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

  /// A local is dropped twice on one straight-line path with nothing that re-initializes
  /// it in between, so the second drop frees storage the first already released.
  DoubleDrop {
    function: String,
    block: String,
    local: LocalId,
  },

  /// An analysis error placeholder was reached while lowering.
  ///
  /// Lowering runs only after analysis reported no errors, so this means a phase produced
  /// the placeholder without a diagnostic to go with it. It lowers to nothing, which
  /// deletes whatever the user wrote at that span from the emitted program.
  ///
  /// Constructed by `lowering::lower_and_verify` rather than by the verifier itself, since
  /// only lowering knows which placeholders it actually reached.
  ErrorNodeReachedLowering { span: Span, function: Option<DefinitionId> },
}

/// LIR verification result.
pub type VerifyResult = Result<(), Vec<VerifyError>>;

/// Verifier for LIR programs.
pub struct LirVerifier<'a> {
  program: &'a LirProgram,
  types: &'a TypeStore,
  defs: &'a DefinitionStore,
  errors: Vec<VerifyError>,
}

impl<'a> LirVerifier<'a> {
  pub fn new(
    program: &'a LirProgram,
    types: &'a TypeStore,
    defs: &'a DefinitionStore,
  ) -> Self {
    Self {
      program,
      types,
      defs,
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

    self.verify_no_double_drops(func, &func_name);
  }

  /// Rejects a local freed twice inside one block with nothing between the two drops that
  /// gives it a new value.
  ///
  /// This is what a duplicated drop schedule reaches the program as: ownership lists the
  /// same value twice for one site, and lowering emits both frees back to back. Scope-end
  /// and early-exit drops of one value land in different blocks, so a repeat that matters
  /// shows up inside a block.
  ///
  /// # Limit
  ///
  /// The check is intra-block: it never follows an edge, so two drops in different blocks
  /// are not compared even when one dominates the other. A cross-block rule needs a
  /// liveness dataflow over the whole function, and the shape this exists to catch —
  /// `record_overwrite` or `record_match_arm_end` holding one value twice — always emits
  /// both frees into one block.
  ///
  /// # What counts as re-initialization
  ///
  /// A `Store` to the local, which is how every new value reaches a slot. Reads do not:
  /// a closure is freed as `Load` then `DropClosure`, so treating the `Load` as a write
  /// would hide exactly the double free PR #226 found.
  ///
  /// A local whose address is ever taken — `AddrOfLocal`, or an `Operand::Local` used as a
  /// pointer — is skipped entirely. Anything reachable through that pointer can write the
  /// slot, in this block or another one, and the verifier cannot see it.
  fn verify_no_double_drops(
    &mut self,
    func: &FunctionLir,
    func_name: &str,
  ) {
    let addressed = addressed_locals(func);

    for block in func.blocks.get_all() {
      // Cleared per block: the check never follows an edge, so nothing carries over.
      let mut dropped: HashSet<LocalId> = HashSet::new();
      let mut loaded_from: HashMap<TempId, LocalId> = HashMap::new();

      for instr in &block.instructions {
        match instr {
          Instr::Load { dest, source } => {
            loaded_from.insert(*dest, *source);
          },
          Instr::Store { dest, .. } => {
            dropped.remove(dest);
            loaded_from.retain(|_, local| local != dest);
          },
          Instr::MarkMoved { ptr, .. } => {
            if let Some(local) = local_behind(ptr, &loaded_from) {
              dropped.remove(&local);
            }
          },
          Instr::Drop { local } => {
            self.record_drop(*local, &addressed, &mut dropped, func_name, &block.label);
          },
          Instr::DropClosure { closure, .. } => {
            if let Some(local) = local_behind(closure, &loaded_from) {
              self.record_drop(local, &addressed, &mut dropped, func_name, &block.label);
            }
          },
          _ => {},
        }
      }
    }
  }

  fn record_drop(
    &mut self,
    local: LocalId,
    addressed: &HashSet<LocalId>,
    dropped: &mut HashSet<LocalId>,
    func_name: &str,
    block_name: &str,
  ) {
    if addressed.contains(&local) {
      return;
    }

    if !dropped.insert(local) {
      self.errors.push(VerifyError::DoubleDrop {
        function: func_name.to_string(),
        block: block_name.to_string(),
        local,
      });
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
      Instr::BuiltinHash { value, hasher, .. } => {
        self.check_operand(func, func_name, block_name, value, defined_temps);
        self.check_operand(func, func_name, block_name, hasher, defined_temps);
      },
      Instr::BuiltinEq { dest, left, right, .. } => {
        self.check_operand(func, func_name, block_name, left, defined_temps);
        self.check_operand(func, func_name, block_name, right, defined_temps);
        defined_temps.insert(*dest);
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
      Instr::BitCast { dest, source, .. } => {
        self.check_operand(func, func_name, block_name, source, defined_temps);
        defined_temps.insert(*dest);
      },
      Instr::AddrOfLocal { dest, local, .. } => {
        self.check_local_exists(func, func_name, block_name, *local);
        defined_temps.insert(*dest);
      },
      Instr::AddrOfGlobal { dest, .. } => {
        defined_temps.insert(*dest);
      },
      Instr::GetElementPtr { dest, base, index, .. } => {
        self.check_operand(func, func_name, block_name, base, defined_temps);
        self.check_operand(func, func_name, block_name, index, defined_temps);
        defined_temps.insert(*dest);
      },
      Instr::MakeSlice { dest, data, len, .. } => {
        self.check_operand(func, func_name, block_name, data, defined_temps);
        self.check_operand(func, func_name, block_name, len, defined_temps);
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
          if !is_droppable(self.types, self.defs, ty) {
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
      Instr::EnumGetTag { dest, source } => {
        self.check_operand(func, func_name, block_name, source, defined_temps);
        defined_temps.insert(*dest);
      },
      Instr::EnumGetPayloadField { dest, source, .. } => {
        self.check_operand(func, func_name, block_name, source, defined_temps);
        defined_temps.insert(*dest);
      },
      Instr::EnumGetPayloadFieldPtr { dest, source, .. } => {
        self.check_operand(func, func_name, block_name, source, defined_temps);
        defined_temps.insert(*dest);
      },
      Instr::Trap { .. } => {},
      Instr::PanicMessage { .. } => {},
      Instr::DropInPlace { ptr, .. } => {
        self.check_operand(func, func_name, block_name, ptr, defined_temps);
      },
      Instr::MarkMoved { ptr, .. } => {
        self.check_operand(func, func_name, block_name, ptr, defined_temps);
      },
      Instr::DropGlue { dest, .. } => {
        defined_temps.insert(*dest);
      },
      Instr::MakeClosure { dest, captures, .. } => {
        for cap in captures {
          self.check_operand(func, func_name, block_name, cap, defined_temps);
        }
        defined_temps.insert(*dest);
      },
      Instr::CallClosure {
        dest, closure, args, ..
      } => {
        self.check_operand(func, func_name, block_name, closure, defined_temps);
        for arg in args {
          self.check_operand(func, func_name, block_name, arg, defined_temps);
        }
        if let Some(d) = dest {
          defined_temps.insert(*d);
        }
      },
      Instr::DropClosure { closure, .. } => {
        self.check_operand(func, func_name, block_name, closure, defined_temps);
      },
      Instr::FreeEnv { env } => {
        self.check_operand(func, func_name, block_name, env, defined_temps);
      },
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
      crate::ConstValue::Atom(_, ty) => *ty,
      crate::ConstValue::Null(ty) => *ty,
      crate::ConstValue::Undef(ty) => *ty,
    }
  }
}

/// Every local whose slot could be written through a pointer.
///
/// `AddrOfLocal` hands out the address explicitly; an `Operand::Local` is a slot used
/// where a pointer is expected, which is the same thing. Either way the verifier cannot
/// tell what writes through it or where, so the double-drop check leaves those locals
/// alone rather than guess.
fn addressed_locals(func: &FunctionLir) -> HashSet<LocalId> {
  let mut addressed = HashSet::new();

  for block in func.blocks.get_all() {
    for instr in &block.instructions {
      if let Instr::AddrOfLocal { local, .. } = instr {
        addressed.insert(*local);
      }

      each_operand(instr, &mut |operand| {
        if let Operand::Local(local) = operand {
          addressed.insert(*local);
        }
      });
    }

    match &block.terminator {
      Terminator::Branch { condition, .. } => {
        if let Operand::Local(local) = condition {
          addressed.insert(*local);
        }
      },
      Terminator::Return(Some(Operand::Local(local))) => {
        addressed.insert(*local);
      },
      Terminator::Goto(_) | Terminator::Return(_) | Terminator::Unreachable => {},
    }
  }

  addressed
}

/// The local a drop names when the value reached the instruction through a temp.
///
/// A closure is freed as a `Load` into a temp followed by `DropClosure` on that temp, so
/// without this the pair reads as a drop of nothing.
fn local_behind(
  operand: &Operand,
  loaded_from: &HashMap<TempId, LocalId>,
) -> Option<LocalId> {
  match operand {
    Operand::Local(local) => Some(*local),
    Operand::Temp(temp) => loaded_from.get(temp).copied(),
    Operand::Const(_) | Operand::FuncRef(_) | Operand::GlobalRef(_) => None,
  }
}

/// Calls `visit` on every operand an instruction reads or writes through.
///
/// Written out rather than matched with a catch-all so that a new instruction carrying an
/// operand has to be listed here instead of silently going unseen.
fn each_operand(
  instr: &Instr,
  visit: &mut impl FnMut(&Operand),
) {
  match instr {
    Instr::Store { value, .. } => visit(value),
    Instr::LoadPtr { ptr, .. } => visit(ptr),
    Instr::StorePtr { ptr, value } => {
      visit(ptr);
      visit(value);
    },
    Instr::BuiltinLoad { ptr, .. } => visit(ptr),
    Instr::BuiltinStore { ptr, value, .. } => {
      visit(ptr);
      visit(value);
    },
    Instr::BuiltinHash { value, hasher, .. } => {
      visit(value);
      visit(hasher);
    },
    Instr::BuiltinEq { left, right, .. } => {
      visit(left);
      visit(right);
    },
    Instr::Copy { source, .. } => visit(source),
    Instr::BinOp { left, right, .. } => {
      visit(left);
      visit(right);
    },
    Instr::UnaryOp { operand, .. } => visit(operand),
    Instr::Call { args, .. } | Instr::RuntimeCall { args, .. } => args.iter().for_each(visit),
    Instr::Cast { source, .. } | Instr::BitCast { source, .. } => visit(source),
    Instr::GetElementPtr { base, index, .. } => {
      visit(base);
      visit(index);
    },
    Instr::MakeSlice { data, len, .. } => {
      visit(data);
      visit(len);
    },
    Instr::InitVector { dest_ptr, elements, .. } => {
      visit(dest_ptr);
      elements.iter().for_each(visit);
    },
    Instr::TypeIdOf { source, .. } => visit(source),
    Instr::GetFieldPtr { base, .. } => visit(base),
    Instr::InitRecord { dest_ptr, fields, .. } => {
      visit(dest_ptr);
      fields.iter().for_each(|(_, operand)| visit(operand));
    },
    Instr::InitEnumVariant { dest_ptr, payload, .. } => {
      visit(dest_ptr);
      payload.iter().for_each(visit);
    },
    Instr::EnumGetTag { source, .. }
    | Instr::EnumGetPayloadField { source, .. }
    | Instr::EnumGetPayloadFieldPtr { source, .. } => visit(source),
    Instr::DropInPlace { ptr, .. } | Instr::MarkMoved { ptr, .. } => visit(ptr),
    Instr::MakeClosure { captures, .. } => captures.iter().for_each(visit),
    Instr::CallClosure { closure, args, .. } => {
      visit(closure);
      args.iter().for_each(visit);
    },
    Instr::DropClosure { closure, .. } => visit(closure),
    Instr::FreeEnv { env } => visit(env),
    Instr::Load { .. }
    | Instr::AddrOfLocal { .. }
    | Instr::AddrOfGlobal { .. }
    | Instr::Nop
    | Instr::SizeOf { .. }
    | Instr::AlignOf { .. }
    | Instr::MaxOf { .. }
    | Instr::MinOf { .. }
    | Instr::Trap { .. }
    | Instr::PanicMessage { .. }
    | Instr::Drop { .. }
    | Instr::DropGlue { .. } => {},
  }
}

/// Verify a LIR program.
pub fn verify_lir(
  program: &LirProgram,
  types: &TypeStore,
  defs: &DefinitionStore,
) -> VerifyResult {
  LirVerifier::new(program, types, defs).verify()
}

/// Check if a type requires a drop call. Delegates to `TypeStore::needs_drop_with_defs`.
pub fn is_droppable(
  types: &TypeStore,
  defs: &DefinitionStore,
  ty: TypeId,
) -> bool {
  types.needs_drop_with_defs(&ty, defs)
}
