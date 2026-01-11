//! Ownership checking over HIR.
//!
//! This module implements ownership analysis that runs on the HIR representation,
//! producing `DropSchedules` that tell LIR lowering when to emit drop instructions.

use std::collections::HashMap;

use ignis_diagnostics::{diagnostic_report::Diagnostic, message::DiagnosticMessage};
use ignis_hir::{DropSchedules, ExitKey, HIR, HIRId, HIRKind, statement::LoopKind};
use ignis_type::{
  definition::{DefinitionId, DefinitionKind, DefinitionStore},
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
  /// Explicitly freed
  Freed,
  /// Returned from function (ownership transferred to caller)
  Returned,
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
      states: HashMap::new(),
      scope_stack: Vec::new(),
      branch_snapshots: Vec::new(),
      schedules: DropSchedules::new(),
      current_fn: None,
      diagnostics: Vec::new(),
    }
  }

  /// Run ownership check on all functions, returns schedules and diagnostics.
  pub fn check(mut self) -> (DropSchedules, Vec<Diagnostic>) {
    for &fn_def_id in &self.hir.items {
      let def = self.defs.get(&fn_def_id);

      if let DefinitionKind::Function(func_def) = &def.kind {
        // Skip extern functions (no body to analyze)
        if func_def.is_extern {
          continue;
        }

        if let Some(&body_id) = self.hir.function_bodies.get(&fn_def_id) {
          self.check_function(fn_def_id, body_id, &func_def.params.clone());
        }
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
      if self.types.is_owned(param_ty) {
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
        if self.types.is_owned(var_ty) {
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

      HIRKind::Call { callee, args } => {
        self.check_call(callee, &args, span);
      },

      HIRKind::Variable(def_id) => {
        // Check if variable is still valid
        self.check_use(def_id, span);
      },

      HIRKind::ExpressionStatement(expr) => {
        self.check_node(expr);
      },

      // Expressions that don't affect ownership directly
      HIRKind::Literal(_) | HIRKind::SizeOf(_) | HIRKind::Error => {},

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
        if matches!(self.types.get(&base_node.type_id), ignis_type::types::Type::Pointer(_)) {
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

      if self.types.is_owned(target_ty) && self.is_valid(&target_def) {
        // Need to drop old value before overwriting
        self.schedules.on_overwrite.entry(hir_id).or_default().push(target_def);
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
        if matches!(self.types.get(&base_node.type_id), ignis_type::types::Type::Pointer(_)) {
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
        if self.types.is_owned(ty) {
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
      // No else branch - restore state from before if
      self.states = self.branch_snapshots.pop().unwrap();
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

        if self.types.is_owned(arg_ty) && !self.types.is_copy(arg_ty) {
          if is_extern {
            let var_name = self.get_var_name(&arg_def);
            self.diagnostics.push(
              DiagnosticMessage::PossibleLeakToFFI {
                var_name,
                span: span.clone(),
              }
              .report(),
            );
          }

          self.try_consume(arg_def, span.clone());
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
        if matches!(self.types.get(ty), ignis_type::types::Type::Pointer(_)) {
          Some(*def_id)
        } else {
          None
        }
      },
      HIRKind::Cast { expression, .. } => self.extract_pointer_variable(*expression),
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
    if let Some(OwnershipState::Moved) = self.states.get(&def_id) {
      let var_name = self.get_var_name(&def_id);
      self
        .diagnostics
        .push(DiagnosticMessage::UseAfterMove { var_name, span }.report());
    }
  }

  fn try_consume(
    &mut self,
    def_id: DefinitionId,
    span: Span,
  ) {
    match self.states.get(&def_id) {
      Some(OwnershipState::Moved) => {
        let var_name = self.get_var_name(&def_id);
        self
          .diagnostics
          .push(DiagnosticMessage::UseAfterMove { var_name, span }.report());
      },
      Some(OwnershipState::Freed) => {},
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
        // Inconsistent: check if some moved and some didn't
        let has_moved = states.iter().any(|&s| s == OwnershipState::Moved);
        let has_valid = states.iter().any(|&s| s == OwnershipState::Valid);

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

        // Use most restrictive state (Moved > Valid)
        if has_moved {
          self.states.insert(var, OwnershipState::Moved);
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
        if self.types.is_owned(ty) && !self.types.is_copy(ty) {
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
}
