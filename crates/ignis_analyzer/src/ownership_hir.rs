//! Ownership checking over HIR.
//!
//! This module implements ownership analysis that runs on the HIR representation,
//! producing `DropSchedules` that tell LIR lowering when to emit drop instructions.

use std::collections::HashMap;

use ignis_diagnostics::{diagnostic_report::Diagnostic, message::DiagnosticMessage};
use ignis_hir::{DropSchedules, ExitKey, HIR, HIRId, HIRKind, statement::LoopKind};
use ignis_type::{
  definition::{DefinitionId, DefinitionKind, DefinitionStore},
  file::SourceMap,
  span::Span,
  symbol::SymbolTable,
  types::TypeStore,
};

/// Ownership state for a variable.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OwnershipState {
  /// Variable is live and usable
  Valid,
  /// Ownership was transferred (moved)
  Moved,
  /// Explicitly freed via `deallocate`
  Freed,
  /// Returned from function (ownership transferred to caller)
  Returned,
  /// Explicitly dropped via manual `.drop()` call
  Dropped,
}

/// Entry in the scope stack.
#[derive(Debug, Clone)]
struct ScopeEntry {
  /// HIRId of the Block that created this scope (None for function root scope)
  block_hir_id: Option<HIRId>,

  /// Owned variables declared in this scope, in declaration order
  owned_vars: Vec<DefinitionId>,

  /// Is this a loop scope? (for break/continue target calculation)
  is_loop: bool,
}

/// Ownership checker that operates on HIR and produces drop schedules.
pub struct HirOwnershipChecker<'a> {
  hir: &'a HIR,
  types: &'a TypeStore,
  defs: &'a DefinitionStore,
  symbols: &'a SymbolTable,
  source_map: Option<&'a SourceMap>,

  /// Current ownership state per variable
  states: HashMap<DefinitionId, OwnershipState>,

  /// Scope stack for tracking variable declarations and loop boundaries
  scope_stack: Vec<ScopeEntry>,

  /// Stack of state snapshots for branch handling
  branch_snapshots: Vec<HashMap<DefinitionId, OwnershipState>>,

  /// Output: drop schedules
  schedules: DropSchedules,

  /// Current function being analyzed
  current_fn: Option<DefinitionId>,

  /// Collected diagnostics
  diagnostics: Vec<Diagnostic>,
}

impl<'a> HirOwnershipChecker<'a> {
  pub fn new(
    hir: &'a HIR,
    types: &'a TypeStore,
    defs: &'a DefinitionStore,
    symbols: &'a SymbolTable,
  ) -> Self {
    Self {
      hir,
      types,
      defs,
      symbols,
      source_map: None,
      states: HashMap::new(),
      scope_stack: Vec::new(),
      branch_snapshots: Vec::new(),
      schedules: DropSchedules::new(),
      current_fn: None,
      diagnostics: Vec::new(),
    }
  }

  pub fn with_source_map(
    mut self,
    source_map: &'a SourceMap,
  ) -> Self {
    self.source_map = Some(source_map);
    self
  }

  /// Run ownership check on all functions and methods, returns schedules and diagnostics.
  pub fn check(mut self) -> (DropSchedules, Vec<Diagnostic>) {
    for &fn_def_id in &self.hir.items {
      let def = self.defs.get(&fn_def_id);

      match &def.kind {
        DefinitionKind::Function(func_def) => {
          if func_def.is_extern {
            continue;
          }

          if let Some(&body_id) = self.hir.function_bodies.get(&fn_def_id) {
            self.check_function(fn_def_id, body_id, &func_def.params.clone());
          }
        },

        DefinitionKind::Method(method_def) => {
          if let Some(&body_id) = self.hir.function_bodies.get(&fn_def_id) {
            self.check_function(fn_def_id, body_id, &method_def.params.clone());
          }
        },

        _ => {},
      }
    }

    (self.schedules, self.diagnostics)
  }

  fn check_function(
    &mut self,
    fn_def_id: DefinitionId,
    body_id: HIRId,
    params: &[DefinitionId],
  ) {
    // Reset state for this function
    self.states.clear();
    self.scope_stack.clear();
    self.branch_snapshots.clear();
    self.current_fn = Some(fn_def_id);

    // Push function root scope (no block_hir_id)
    self.scope_stack.push(ScopeEntry {
      block_hir_id: None,
      owned_vars: Vec::new(),
      is_loop: false,
    });

    // Register owned parameters
    for &param_id in params {
      let param_ty = self.defs.type_of(&param_id);
      if self.types.needs_drop_with_defs(param_ty, self.defs) {
        self.declare_owned(param_id);
      }
    }

    // Check the function body
    self.check_node(body_id);

    // Calculate drops for implicit return at function end (FnEnd)
    let fn_end_drops = self.calculate_exit_drops(0);
    if !fn_end_drops.is_empty() {
      self.schedules.on_exit.insert(ExitKey::FnEnd(fn_def_id), fn_end_drops);
    }

    // Pop function root scope (should be the only one left)
    self.scope_stack.pop();
    self.current_fn = None;
  }

  fn check_node(
    &mut self,
    hir_id: HIRId,
  ) {
    let node = self.hir.get(hir_id);
    let kind = node.kind.clone();
    let span = node.span.clone();

    match kind {
      HIRKind::Block { statements, expression } => {
        self.check_block(hir_id, &statements, expression);
      },

      HIRKind::Let { name, value } => {
        // Check initializer first (may consume variables)
        if let Some(val_id) = value {
          self.check_node(val_id);

          // If initializing from an owned variable, mark source as moved
          if let Some(source_def) = self.get_moved_var(val_id) {
            self.try_consume(source_def, span.clone());
          }
        }

        // Register the new variable if it's owned
        let var_ty = self.defs.type_of(&name);
        if self.types.needs_drop_with_defs(var_ty, self.defs) {
          self.declare_owned(name);
        }
      },

      HIRKind::Assign { target, value, .. } => {
        self.check_assign(hir_id, target, value, span);
      },

      HIRKind::Return(value) => {
        self.check_return(hir_id, value, span);
      },

      HIRKind::Break => {
        self.check_break(hir_id);
      },

      HIRKind::Continue => {
        self.check_continue(hir_id);
      },

      HIRKind::If {
        condition,
        then_branch,
        else_branch,
      } => {
        self.check_if(condition, then_branch, else_branch, span);
      },

      HIRKind::Loop { condition, body } => {
        self.check_loop(condition, body);
      },

      HIRKind::Call { callee, args, .. } => {
        self.check_call(callee, &args, span);
      },

      HIRKind::BuiltinLoad { ptr, .. } => {
        self.check_node(ptr);
      },

      HIRKind::BuiltinStore { ptr, value, .. } => {
        self.check_node(ptr);
        self.check_node(value);
      },

      HIRKind::Variable(def_id) => {
        // Check if variable is still valid
        self.check_use(def_id, span);
      },

      HIRKind::ExpressionStatement(expr) => {
        self.check_node(expr);
      },

      // Expressions that don't affect ownership directly
      HIRKind::Literal(_)
      | HIRKind::SizeOf(_)
      | HIRKind::AlignOf(_)
      | HIRKind::MaxOf(_)
      | HIRKind::MinOf(_)
      | HIRKind::Error => {},

      HIRKind::Binary { left, right, .. } => {
        self.check_node(left);
        self.check_node(right);
      },

      HIRKind::Unary { operand, .. } => {
        self.check_node(operand);
      },

      HIRKind::Cast { expression, .. } => {
        self.check_node(expression);
      },

      HIRKind::BitCast { expression, .. } => {
        self.check_node(expression);
      },

      HIRKind::Reference { expression, .. } => {
        self.check_node(expression);
      },

      HIRKind::Dereference(inner) => {
        self.check_node(inner);
        self.check_use_after_free(inner, span);
      },

      HIRKind::Index { base, index } => {
        self.check_node(base);
        self.check_node(index);

        let base_node = self.hir.get(base);
        if matches!(self.types.get(&base_node.type_id), ignis_type::types::Type::Pointer { .. }) {
          self.check_use_after_free(base, span);
        }
      },

      HIRKind::VectorLiteral { elements } => {
        for elem in elements {
          self.check_node(elem);
        }
      },

      HIRKind::TypeOf(inner) => {
        self.check_node(inner);
      },

      // Records and enums
      HIRKind::FieldAccess { base, .. } => {
        self.check_node(base);
      },

      HIRKind::RecordInit { fields, .. } => {
        for (_, field_value) in fields {
          self.check_node(field_value);
        }
      },

      HIRKind::MethodCall {
        receiver, args, method, ..
      } => {
        let is_drop = receiver.is_some() && self.is_drop_method(method);

        if let Some(recv) = receiver {
          if is_drop {
            // Skip receiver check here: handle_manual_drop reports DoubleDrop directly.
            if let Some(recv_def) = self.extract_receiver_variable(recv) {
              self.handle_manual_drop(recv_def, span.clone());
            }
          } else {
            self.check_node(recv);
          }
        }

        for &arg in &args {
          self.check_node(arg);
        }
      },

      HIRKind::EnumVariant { payload, .. } => {
        for &p in &payload {
          self.check_node(p);
        }
      },

      HIRKind::StaticAccess { def } => {
        // Check if static variable is still valid
        self.check_use(def, span);
      },

      HIRKind::Panic(msg) => {
        self.check_node(msg);
      },
      HIRKind::Trap | HIRKind::BuiltinUnreachable => {},
    }
  }

  fn check_block(
    &mut self,
    block_hir_id: HIRId,
    statements: &[HIRId],
    expression: Option<HIRId>,
  ) {
    // Push block scope
    self.scope_stack.push(ScopeEntry {
      block_hir_id: Some(block_hir_id),
      owned_vars: Vec::new(),
      is_loop: false,
    });

    // Check statements
    for &stmt_id in statements {
      self.check_node(stmt_id);
    }

    // Check tail expression if present
    if let Some(expr_id) = expression {
      self.check_node(expr_id);
    }

    // Calculate drops for this scope
    self.exit_block_scope(block_hir_id);
  }

  fn check_assign(
    &mut self,
    hir_id: HIRId,
    target: HIRId,
    value: HIRId,
    span: Span,
  ) {
    // Check value first (may move source)
    self.check_node(value);

    // Check use-after-free on assign target (e.g., *freed_ptr = value)
    self.check_assign_target_use_after_free(target, span.clone());

    // Check if target is a simple owned variable that needs drop-before-overwrite
    if let Some(target_def) = self.get_assign_target_def(target) {
      let target_ty = self.defs.type_of(&target_def);

      if self.types.needs_drop_with_defs(target_ty, self.defs) {
        if self.is_valid(&target_def) {
          // Need to drop old value before overwriting
          self.schedules.on_overwrite.entry(hir_id).or_default().push(target_def);
        }

        // Re-initialization: assigning to a dropped/moved variable makes it valid again.
        // This is the only way to "revive" a consumed variable.
        let current_state = self.states.get(&target_def).copied();
        if matches!(current_state, Some(OwnershipState::Dropped) | Some(OwnershipState::Moved)) {
          self.states.insert(target_def, OwnershipState::Valid);
        }
      }
    }

    // If assigning from an owned variable, mark source as moved
    if let Some(source_def) = self.get_moved_var(value) {
      self.try_consume(source_def, span);
    }
  }

  fn check_assign_target_use_after_free(
    &mut self,
    target: HIRId,
    span: Span,
  ) {
    let node = self.hir.get(target);
    match &node.kind {
      HIRKind::Dereference(inner) => {
        self.check_use_after_free(*inner, span);
      },
      HIRKind::Index { base, .. } => {
        let base_node = self.hir.get(*base);
        if matches!(self.types.get(&base_node.type_id), ignis_type::types::Type::Pointer { .. }) {
          self.check_use_after_free(*base, span);
        }
      },
      _ => {},
    }
  }

  fn check_return(
    &mut self,
    hir_id: HIRId,
    value: Option<HIRId>,
    _span: Span,
  ) {
    // Check return value expression
    if let Some(val_id) = value {
      self.check_node(val_id);

      // If returning an owned variable, mark as Returned (not dropped)
      if let Some(def_id) = self.get_moved_var(val_id) {
        let ty = self.defs.type_of(&def_id);
        if self.types.needs_drop_with_defs(ty, self.defs) {
          self.states.insert(def_id, OwnershipState::Returned);
        }
      }
    }

    // Calculate drops for all scopes we're exiting (to function root)
    let drops = self.calculate_exit_drops(0);
    if !drops.is_empty() {
      self.schedules.on_exit.insert(ExitKey::Return(hir_id), drops);
    }
  }

  fn check_break(
    &mut self,
    hir_id: HIRId,
  ) {
    // Find loop scope
    if let Some(loop_idx) = self.find_loop_scope_idx() {
      // Calculate drops for scopes being exited (not including loop scope)
      let drops = self.calculate_exit_drops(loop_idx + 1);
      if !drops.is_empty() {
        self.schedules.on_exit.insert(ExitKey::Break(hir_id), drops);
      }
    }
    // If no loop found, semantic error should have been caught earlier
  }

  fn check_continue(
    &mut self,
    hir_id: HIRId,
  ) {
    // Find loop scope
    if let Some(loop_idx) = self.find_loop_scope_idx() {
      // Calculate drops for scopes being exited (not including loop scope)
      let drops = self.calculate_exit_drops(loop_idx + 1);
      if !drops.is_empty() {
        self.schedules.on_exit.insert(ExitKey::Continue(hir_id), drops);
      }
    }
  }

  fn check_if(
    &mut self,
    condition: HIRId,
    then_branch: HIRId,
    else_branch: Option<HIRId>,
    span: Span,
  ) {
    // Check condition
    self.check_node(condition);

    // Snapshot state before then branch
    self.branch_snapshots.push(self.states.clone());

    // Check then branch
    self.check_node(then_branch);
    let then_state = self.states.clone();

    if let Some(else_id) = else_branch {
      // Restore state for else branch
      self.states = self.branch_snapshots.pop().unwrap();
      self.branch_snapshots.push(self.states.clone());

      // Check else branch
      self.check_node(else_id);
      let else_state = self.states.clone();

      // Pop snapshot
      self.branch_snapshots.pop();

      // Merge branch states
      self.merge_branch_states(vec![then_state, else_state], span);
    } else {
      // No explicit else: merge then-state with the pre-if snapshot.
      let pre_if_state = self.branch_snapshots.pop().unwrap();
      self.merge_branch_states(vec![then_state, pre_if_state], span);
    }
  }

  fn check_loop(
    &mut self,
    condition: LoopKind,
    body: HIRId,
  ) {
    // Check loop condition/init/update based on loop kind
    match &condition {
      LoopKind::Infinite => {},
      LoopKind::While { condition: cond_id } => {
        self.check_node(*cond_id);
      },
      LoopKind::For {
        init,
        condition: cond,
        update,
      } => {
        if let Some(init_id) = init {
          self.check_node(*init_id);
        }
        if let Some(cond_id) = cond {
          self.check_node(*cond_id);
        }
        if let Some(update_id) = update {
          self.check_node(*update_id);
        }
      },
    }

    // Push a loop marker scope for break/continue target resolution
    self.scope_stack.push(ScopeEntry {
      block_hir_id: None,
      owned_vars: Vec::new(),
      is_loop: true,
    });

    self.check_node(body);
    self.scope_stack.pop();
  }

  fn check_call(
    &mut self,
    callee: DefinitionId,
    args: &[HIRId],
    span: Span,
  ) {
    let callee_def = self.defs.get(&callee);
    let callee_name = self.symbols.get(&callee_def.name);
    let is_extern = matches!(&callee_def.kind, DefinitionKind::Function(f) if f.is_extern);

    // Track deallocate arg to mark as Freed after processing
    let deallocate_ptr = if callee_name == "deallocate" && !args.is_empty() {
      self.extract_pointer_variable(args[0])
    } else {
      None
    };

    for &arg_id in args {
      self.check_node(arg_id);

      if let Some(arg_def) = self.get_moved_var(arg_id) {
        let arg_ty = self.defs.type_of(&arg_def);

        if self.types.needs_drop_with_defs(arg_ty, self.defs) && !self.types.is_copy_with_defs(arg_ty, self.defs) {
          // FFI Ownership Semantics:
          // - Ignis functions: consume ownership (caller must not use value after call)
          // - Extern functions: do NOT consume ownership (caller retains responsibility)
          //
          // This means when passing owned values to extern functions:
          // 1. The Ignis caller must ensure proper cleanup if needed
          // 2. The extern function should not free/deallocate the value
          // 3. Standard library functions (std::*) typically handle this correctly
          //
          // Note: A future lint could warn about potential leaks when passing owned
          // values to non-std extern functions, but this was removed to reduce noise.
          if !is_extern {
            self.try_consume(arg_def, span.clone());
          }
        }
      }
    }

    if let Some(ptr_def) = deallocate_ptr {
      self.states.insert(ptr_def, OwnershipState::Freed);
    }
  }

  fn extract_pointer_variable(
    &self,
    hir_id: HIRId,
  ) -> Option<DefinitionId> {
    let node = self.hir.get(hir_id);
    match &node.kind {
      HIRKind::Variable(def_id) => {
        let ty = self.defs.type_of(def_id);
        if matches!(self.types.get(ty), ignis_type::types::Type::Pointer { .. }) {
          Some(*def_id)
        } else {
          None
        }
      },
      HIRKind::Cast { expression, .. } => self.extract_pointer_variable(*expression),
      HIRKind::BitCast { expression, .. } => self.extract_pointer_variable(*expression),
      _ => None,
    }
  }

  fn check_use_after_free(
    &mut self,
    ptr_hir: HIRId,
    span: Span,
  ) {
    if let Some(ptr_def) = self.extract_pointer_variable(ptr_hir) {
      if self.states.get(&ptr_def) == Some(&OwnershipState::Freed) {
        let var_name = self.get_var_name(&ptr_def);
        self
          .diagnostics
          .push(DiagnosticMessage::UseAfterFree { var_name, span }.report());
      }
    }
  }

  // === Scope management ===

  fn declare_owned(
    &mut self,
    def_id: DefinitionId,
  ) {
    self.states.insert(def_id, OwnershipState::Valid);

    if let Some(scope) = self.scope_stack.last_mut() {
      scope.owned_vars.push(def_id);
    }
  }

  fn exit_block_scope(
    &mut self,
    _block_hir_id: HIRId,
  ) {
    let entry = self.scope_stack.pop().expect("unbalanced scope stack");

    // Calculate drops: valid vars in reverse declaration order
    let drops: Vec<_> = entry
      .owned_vars
      .iter()
      .rev()
      .filter(|def| self.is_valid(def))
      .copied()
      .collect();

    if let Some(hir_id) = entry.block_hir_id {
      if !drops.is_empty() {
        self.schedules.on_scope_end.insert(hir_id, drops);
      }
    }
  }

  /// Calculate drops for exiting from current scope to target scope index.
  /// Returns drops ordered: inner scopes first, outer last.
  /// Within each scope: reverse declaration order.
  fn calculate_exit_drops(
    &self,
    exit_to_scope_idx: usize,
  ) -> Vec<DefinitionId> {
    let mut drops = Vec::new();

    // From innermost to exit_to_scope_idx (exclusive)
    for scope in self.scope_stack[exit_to_scope_idx..].iter().rev() {
      // Within each scope: reverse declaration order
      for def in scope.owned_vars.iter().rev() {
        if self.is_valid(def) {
          drops.push(*def);
        }
      }
    }

    drops
  }

  fn find_loop_scope_idx(&self) -> Option<usize> {
    self.scope_stack.iter().rposition(|s| s.is_loop)
  }

  // === State management ===

  fn is_valid(
    &self,
    def: &DefinitionId,
  ) -> bool {
    matches!(self.states.get(def), Some(OwnershipState::Valid) | None)
  }

  fn check_use(
    &mut self,
    def_id: DefinitionId,
    span: Span,
  ) {
    // Use-after-free is checked at dereference/index, not here
    match self.states.get(&def_id) {
      Some(OwnershipState::Moved) => {
        let var_name = self.get_var_name(&def_id);
        self
          .diagnostics
          .push(DiagnosticMessage::UseAfterMove { var_name, span }.report());
      },
      Some(OwnershipState::Dropped) => {
        let var_name = self.get_var_name(&def_id);
        self
          .diagnostics
          .push(DiagnosticMessage::UseAfterDrop { var_name, span }.report());
      },
      _ => {},
    }
  }

  /// Attempt to move a variable, transitioning Valid -> Moved.
  ///
  /// Invalid states are diagnosed by `check_use`, which runs before this path.
  fn try_consume(
    &mut self,
    def_id: DefinitionId,
    _span: Span,
  ) {
    match self.states.get(&def_id) {
      Some(OwnershipState::Moved | OwnershipState::Dropped | OwnershipState::Freed) => {
        // Already diagnosed by check_use — just leave state as-is.
      },
      _ => {
        self.states.insert(def_id, OwnershipState::Moved);
      },
    }
  }

  fn merge_branch_states(
    &mut self,
    branches: Vec<HashMap<DefinitionId, OwnershipState>>,
    span: Span,
  ) {
    if branches.is_empty() {
      return;
    }

    // Collect all variables that appear in any branch
    let all_vars: std::collections::HashSet<_> = branches.iter().flat_map(|b| b.keys().copied()).collect();

    for var in all_vars {
      let states: Vec<_> = branches.iter().filter_map(|b| b.get(&var).copied()).collect();

      if states.is_empty() {
        continue;
      }

      // Check if all branches have the same state
      let first = states[0];
      let all_same = states.iter().all(|&s| s == first);

      if all_same {
        self.states.insert(var, first);
      } else {
        let has_moved = states.contains(&OwnershipState::Moved);
        let has_dropped = states.contains(&OwnershipState::Dropped);
        let has_freed = states.contains(&OwnershipState::Freed);
        let has_valid = states.contains(&OwnershipState::Valid);

        if has_moved && has_valid {
          let var_name = self.get_var_name(&var);
          self.diagnostics.push(
            DiagnosticMessage::InconsistentMoveInBranches {
              var_name,
              span: span.clone(),
            }
            .report(),
          );
        }

        if has_dropped && has_valid {
          let var_name = self.get_var_name(&var);
          self.diagnostics.push(
            DiagnosticMessage::InconsistentDropInBranches {
              var_name,
              span: span.clone(),
            }
            .report(),
          );
        }

        // Most restrictive state wins: Dropped > Moved > Freed > Valid
        if has_dropped {
          self.states.insert(var, OwnershipState::Dropped);
        } else if has_moved {
          self.states.insert(var, OwnershipState::Moved);
        } else if has_freed {
          self.states.insert(var, OwnershipState::Freed);
        }
      }
    }
  }

  // === Helpers ===

  /// Get the DefinitionId if this node is a simple owned variable.
  fn get_moved_var(
    &self,
    hir_id: HIRId,
  ) -> Option<DefinitionId> {
    let node = self.hir.get(hir_id);
    match &node.kind {
      HIRKind::Variable(def_id) => {
        let ty = self.defs.type_of(def_id);
        if self.types.needs_drop_with_defs(ty, self.defs) && !self.types.is_copy_with_defs(ty, self.defs) {
          Some(*def_id)
        } else {
          None
        }
      },
      _ => None,
    }
  }

  fn get_assign_target_def(
    &self,
    target: HIRId,
  ) -> Option<DefinitionId> {
    let node = self.hir.get(target);
    match &node.kind {
      HIRKind::Variable(def_id) => Some(*def_id),
      _ => None,
    }
  }

  fn get_var_name(
    &self,
    def_id: &DefinitionId,
  ) -> String {
    let def = self.defs.get(def_id);
    self.symbols.get(&def.name).to_string()
  }

  /// Check if a method DefinitionId refers to a drop method on a record with `@implements(Drop)`.
  fn is_drop_method(
    &self,
    method_def_id: DefinitionId,
  ) -> bool {
    let method_def = self.defs.get(&method_def_id);

    let owner_type_def_id = match &method_def.kind {
      DefinitionKind::Method(md) => md.owner_type,
      _ => return false,
    };

    let method_name = self.symbols.get(&method_def.name);
    if method_name != "drop" && !method_name.ends_with("__drop") {
      return false;
    }

    let owner_def = self.defs.get(&owner_type_def_id);
    match &owner_def.kind {
      DefinitionKind::Record(rd) => rd.lang_traits.drop,
      _ => false,
    }
  }

  /// Extract the binding DefinitionId from a method receiver.
  /// Expects the HIR pattern: `Reference { expression: Variable(def_id), mutable: true }`
  /// or a direct `Variable(def_id)`.
  ///
  /// Returns `None` for complex receivers (e.g. `container.inner.drop()`, `getObj().drop()`,
  /// `array[i].drop()`). These cases are not tracked at compile time — the runtime
  /// `__ignis_drop_state` guard in the generated C code handles double-drop prevention
  /// for them instead.
  fn extract_receiver_variable(
    &self,
    recv_hir: HIRId,
  ) -> Option<DefinitionId> {
    let recv_node = self.hir.get(recv_hir);

    match &recv_node.kind {
      HIRKind::Reference { expression, .. } => {
        let inner = self.hir.get(*expression);
        match &inner.kind {
          HIRKind::Variable(def_id) => Some(*def_id),
          _ => None,
        }
      },
      HIRKind::Variable(def_id) => Some(*def_id),
      _ => None,
    }
  }

  /// Handle a manual `.drop()` call on a receiver variable.
  /// Transitions: Valid → Dropped, Dropped → DoubleDrop, Moved → UseAfterMove.
  fn handle_manual_drop(
    &mut self,
    def_id: DefinitionId,
    span: Span,
  ) {
    match self.states.get(&def_id) {
      Some(OwnershipState::Dropped) => {
        let var_name = self.get_var_name(&def_id);
        self
          .diagnostics
          .push(DiagnosticMessage::DoubleDrop { var_name, span }.report());
      },

      Some(OwnershipState::Moved) => {
        let var_name = self.get_var_name(&def_id);
        self
          .diagnostics
          .push(DiagnosticMessage::UseAfterMove { var_name, span }.report());
      },

      Some(OwnershipState::Freed) => {
        let var_name = self.get_var_name(&def_id);
        self
          .diagnostics
          .push(DiagnosticMessage::UseAfterFree { var_name, span }.report());
      },

      Some(OwnershipState::Valid) | None => {
        self.states.insert(def_id, OwnershipState::Dropped);
      },

      Some(OwnershipState::Returned) => {
        // Value already left this scope; model this as use-after-move.
        let var_name = self.get_var_name(&def_id);
        self
          .diagnostics
          .push(DiagnosticMessage::UseAfterMove { var_name, span }.report());
      },
    }
  }
}
