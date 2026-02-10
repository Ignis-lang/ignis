use std::collections::HashMap;

use ignis_diagnostics::{diagnostic_report::Diagnostic, message::DiagnosticMessage};
use ignis_hir::{HIR, HIRId, HIRKind, statement::LoopKind};
use ignis_type::{
  definition::{DefinitionId, DefinitionKind, DefinitionStore},
  file::SourceMap,
  span::Span,
  symbol::SymbolTable,
  types::{Type, TypeStore},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BorrowState {
  None,
  Imm(usize),
  Mut,
}

impl BorrowState {
  fn is_borrowed(self) -> bool {
    !matches!(self, BorrowState::None)
  }

  fn merge(
    self,
    other: BorrowState,
  ) -> BorrowState {
    match (self, other) {
      (BorrowState::Mut, _) | (_, BorrowState::Mut) => BorrowState::Mut,
      (BorrowState::Imm(a), BorrowState::Imm(b)) => BorrowState::Imm(a.max(b)),
      (BorrowState::Imm(n), BorrowState::None) | (BorrowState::None, BorrowState::Imm(n)) => BorrowState::Imm(n),
      (BorrowState::None, BorrowState::None) => BorrowState::None,
    }
  }
}

/// Tracks which variables a named borrow (let-binding) borrows.
#[derive(Debug, Clone)]
struct ActiveBorrow {
  #[allow(dead_code)]
  binder: DefinitionId,
  /// The variable being borrowed
  target: DefinitionId,
  /// Whether the borrow is mutable
  mutable: bool,
}

#[derive(Debug, Clone)]
struct ScopeEntry {
  #[allow(dead_code)]
  block_hir_id: Option<HIRId>,
  borrows: Vec<ActiveBorrow>,
  #[allow(dead_code)]
  is_loop: bool,
}

/// Borrow checker operating on HIR, post-monomorphization.
///
/// Runs alongside HirOwnershipChecker in the pipeline. Detects:
/// - Mutable borrow while immutably borrowed (A0037)
/// - Immutable borrow while mutably borrowed (A0036)
/// - Double mutable borrow (A0038)
/// - Mutation of a borrowed variable (A0047)
/// - Return of reference to local variable (A0043)
pub struct HirBorrowChecker<'a> {
  hir: &'a HIR,
  types: &'a TypeStore,
  defs: &'a DefinitionStore,
  symbols: &'a SymbolTable,
  source_map: Option<&'a SourceMap>,

  borrow_state: HashMap<DefinitionId, BorrowState>,

  scope_stack: Vec<ScopeEntry>,

  branch_snapshots: Vec<(HashMap<DefinitionId, BorrowState>, Vec<ScopeEntry>)>,

  reachable: bool,

  loop_continue_stack: Vec<bool>,

  diagnostics: Vec<Diagnostic>,
}

impl<'a> HirBorrowChecker<'a> {
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
      borrow_state: HashMap::new(),
      scope_stack: Vec::new(),
      branch_snapshots: Vec::new(),
      reachable: true,
      loop_continue_stack: Vec::new(),
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

  pub fn check(mut self) -> Vec<Diagnostic> {
    for &fn_def_id in &self.hir.items {
      let def = self.defs.get(&fn_def_id);

      match &def.kind {
        DefinitionKind::Function(func_def) => {
          if func_def.is_extern {
            continue;
          }
          if let Some(&body_id) = self.hir.function_bodies.get(&fn_def_id) {
            self.check_function(body_id);
          }
        },

        DefinitionKind::Method(_) => {
          if let Some(&body_id) = self.hir.function_bodies.get(&fn_def_id) {
            self.check_function(body_id);
          }
        },

        _ => {},
      }
    }

    self.diagnostics
  }

  fn check_function(
    &mut self,
    body_id: HIRId,
  ) {
    self.borrow_state.clear();
    self.scope_stack.clear();
    self.branch_snapshots.clear();
    self.reachable = true;
    self.loop_continue_stack.clear();

    self.scope_stack.push(ScopeEntry {
      block_hir_id: None,
      borrows: Vec::new(),
      is_loop: false,
    });

    self.check_node(body_id);

    self.scope_stack.pop();
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
        if let Some(val_id) = value {
          self.check_node(val_id);

          // If the value is a reference expression, record a named borrow
          self.try_record_let_borrow(name, val_id, &span);
        }
      },

      HIRKind::Assign { target, value, .. } => {
        self.check_node(value);
        self.check_node(target);

        // Check mutation of borrowed variable
        if let Some(target_def) = self.extract_root_variable(target) {
          if self
            .borrow_state
            .get(&target_def)
            .copied()
            .unwrap_or(BorrowState::None)
            .is_borrowed()
          {
            let var_name = self.def_name(target_def);
            self
              .diagnostics
              .push(DiagnosticMessage::MutatedWhileBorrowed { var_name, span }.report());
          }
        }
      },

      HIRKind::Return(value) => {
        if let Some(val_id) = value {
          self.check_node(val_id);
          self.check_return_local_ref(val_id, &span);
        }
        self.reachable = false;
      },

      HIRKind::Break => {
        self.reachable = false;
      },

      HIRKind::Continue => {
        if let Some(flag) = self.loop_continue_stack.last_mut() {
          *flag = true;
        }
        self.reachable = false;
      },

      HIRKind::If {
        condition,
        then_branch,
        else_branch,
      } => {
        self.check_if(condition, then_branch, else_branch);
      },

      HIRKind::Loop { condition, body } => {
        self.check_loop(condition, body);
      },

      HIRKind::Call { args, .. } => {
        for arg in args {
          self.check_node(arg);

          // Temporary borrows: references passed directly to calls
          self.check_temporary_borrow(arg, &span);
        }
      },

      HIRKind::MethodCall { receiver, args, .. } => {
        if let Some(recv) = receiver {
          self.check_node(recv);
        }
        for &arg in &args {
          self.check_node(arg);
          self.check_temporary_borrow(arg, &span);
        }
      },

      HIRKind::Reference { expression, mutable } => {
        self.check_node(expression);

        // Standalone reference not captured by Let (e.g. in expression statement) —
        // check for conflict but don't persist the borrow.
        if let Some(target_def) = self.extract_root_variable(expression) {
          self.try_borrow(target_def, mutable, &span);
          // Release immediately since it's not bound to a Let
          self.release_borrow(target_def, mutable);
        }
      },

      // Pass-through: recurse into children
      HIRKind::Binary { left, right, .. } => {
        self.check_node(left);
        self.check_node(right);
      },
      HIRKind::Unary { operand, .. } => {
        self.check_node(operand);
      },
      HIRKind::Cast { expression, .. } | HIRKind::BitCast { expression, .. } => {
        self.check_node(expression);
      },
      HIRKind::Dereference(inner) => {
        self.check_node(inner);
      },
      HIRKind::Index { base, index } => {
        self.check_node(base);
        self.check_node(index);
      },
      HIRKind::VectorLiteral { elements } => {
        for elem in elements {
          self.check_node(elem);
        }
      },
      HIRKind::FieldAccess { base, .. } => {
        self.check_node(base);
      },
      HIRKind::RecordInit { fields, .. } => {
        for (_, value) in fields {
          self.check_node(value);
        }
      },
      HIRKind::EnumVariant { payload, .. } => {
        for p in payload {
          self.check_node(p);
        }
      },
      HIRKind::ExpressionStatement(expr) => {
        self.check_node(expr);
      },
      HIRKind::TypeOf(inner) => {
        self.check_node(inner);
      },
      HIRKind::BuiltinLoad { ptr, .. } | HIRKind::BuiltinDropInPlace { ptr, .. } => {
        self.check_node(ptr);
      },
      HIRKind::BuiltinStore { ptr, value, .. } => {
        self.check_node(ptr);
        self.check_node(value);
      },
      HIRKind::RcNew { value } | HIRKind::RcClone { value } => {
        self.check_node(value);
      },
      HIRKind::Panic(msg) => {
        self.check_node(msg);
        self.reachable = false;
      },
      HIRKind::Trap | HIRKind::BuiltinUnreachable => {
        self.reachable = false;
      },

      // Leaf nodes
      HIRKind::Literal(_)
      | HIRKind::Variable(_)
      | HIRKind::SizeOf(_)
      | HIRKind::AlignOf(_)
      | HIRKind::MaxOf(_)
      | HIRKind::MinOf(_)
      | HIRKind::StaticAccess { .. }
      | HIRKind::BuiltinDropGlue { .. }
      | HIRKind::Error => {},
    }
  }

  fn check_block(
    &mut self,
    block_hir_id: HIRId,
    statements: &[HIRId],
    expression: Option<HIRId>,
  ) {
    self.scope_stack.push(ScopeEntry {
      block_hir_id: Some(block_hir_id),
      borrows: Vec::new(),
      is_loop: false,
    });

    for &stmt_id in statements {
      if !self.reachable {
        break;
      }
      self.check_node(stmt_id);
    }

    if let Some(expr_id) = expression {
      if self.reachable {
        self.check_node(expr_id);
      }
    }

    // Release all borrows registered in this scope
    self.exit_block_scope();
  }

  fn check_if(
    &mut self,
    condition: HIRId,
    then_branch: HIRId,
    else_branch: Option<HIRId>,
  ) {
    self.check_node(condition);

    let pre_if_reachable = self.reachable;
    let pre_if_state = self.borrow_state.clone();
    let pre_if_scopes = self.clone_scope_borrows();

    // Then branch
    self.reachable = pre_if_reachable;
    self.check_node(then_branch);
    let then_state = self.borrow_state.clone();
    let then_reachable = self.reachable;

    if let Some(else_id) = else_branch {
      // Restore for else
      self.borrow_state = pre_if_state;
      self.restore_scope_borrows(&pre_if_scopes);

      self.reachable = pre_if_reachable;
      self.check_node(else_id);
      let else_state = self.borrow_state.clone();
      let else_reachable = self.reachable;

      self.borrow_state = self.merge_borrow_states(&then_state, &else_state);
      self.reachable = then_reachable || else_reachable;
    } else {
      self.borrow_state = self.merge_borrow_states(&then_state, &pre_if_state);
      self.reachable = then_reachable || pre_if_reachable;
    }
  }

  fn check_loop(
    &mut self,
    condition: LoopKind,
    body: HIRId,
  ) {
    let for_update = match &condition {
      LoopKind::Infinite => None,
      LoopKind::While { condition: cond_id } => {
        self.check_node(*cond_id);
        None
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
        *update
      },
    };

    let pre_loop_state = self.borrow_state.clone();

    self.scope_stack.push(ScopeEntry {
      block_hir_id: None,
      borrows: Vec::new(),
      is_loop: true,
    });

    self.loop_continue_stack.push(false);

    let pre_body_reachable = self.reachable;
    self.check_node(body);

    let body_falls_through = self.reachable;
    let continue_seen = *self.loop_continue_stack.last().unwrap_or(&false);
    let update_reachable = body_falls_through || continue_seen;

    if let Some(update_id) = for_update {
      if update_reachable {
        self.reachable = true;
        self.check_node(update_id);
      }
    }

    // Second pass for cross-iteration borrow conflicts
    let can_iterate_again = body_falls_through || continue_seen;
    if can_iterate_again {
      let post_first_iter = self.borrow_state.clone();
      let diag_count_before = self.diagnostics.len();

      self.borrow_state = self.merge_borrow_states(&pre_loop_state, &post_first_iter);

      self.reachable = pre_body_reachable;
      self.check_node(body);

      let body_falls_through_2 = self.reachable;
      let continue_seen_2 = *self.loop_continue_stack.last().unwrap_or(&false);
      if let Some(update_id) = for_update {
        if body_falls_through_2 || continue_seen_2 {
          self.reachable = true;
          self.check_node(update_id);
        }
      }

      self.deduplicate_second_pass_diagnostics(diag_count_before);
    }

    self.loop_continue_stack.pop();
    self.scope_stack.pop();

    self.borrow_state = self.merge_borrow_states(&pre_loop_state, &self.borrow_state.clone());
    self.reachable = pre_body_reachable;
  }

  /// When a `let` binding is initialized with a reference expression,
  /// record the borrow so it persists until the scope exits.
  fn try_record_let_borrow(
    &mut self,
    binder: DefinitionId,
    value_id: HIRId,
    span: &Span,
  ) {
    let node = self.hir.get(value_id);

    let (expr_id, mutable) = match &node.kind {
      HIRKind::Reference { expression, mutable } => (*expression, *mutable),
      _ => return,
    };

    let Some(target_def) = self.extract_root_variable(expr_id) else {
      return;
    };

    // Check for conflict before recording
    self.try_borrow(target_def, mutable, span);

    // Record in current scope so it's released when the scope exits
    if let Some(scope) = self.scope_stack.last_mut() {
      scope.borrows.push(ActiveBorrow {
        binder,
        target: target_def,
        mutable,
      });
    }
  }

  /// Check a temporary reference passed as a function argument.
  /// These borrows are transient: conflict-check only, no persistent state.
  fn check_temporary_borrow(
    &mut self,
    arg_id: HIRId,
    span: &Span,
  ) {
    let node = self.hir.get(arg_id);

    let (expr_id, mutable) = match &node.kind {
      HIRKind::Reference { expression, mutable } => (*expression, *mutable),
      _ => return,
    };

    let Some(target_def) = self.extract_root_variable(expr_id) else {
      return;
    };

    // Check for conflict
    self.try_borrow(target_def, mutable, span);

    // Release immediately — the borrow only lives for the call duration
    self.release_borrow(target_def, mutable);
  }

  /// Attempt to create a borrow on a variable. Emits a diagnostic on conflict.
  fn try_borrow(
    &mut self,
    target: DefinitionId,
    mutable: bool,
    span: &Span,
  ) {
    let current = self.borrow_state.get(&target).copied().unwrap_or(BorrowState::None);

    if mutable {
      match current {
        BorrowState::None => {
          self.borrow_state.insert(target, BorrowState::Mut);
        },
        BorrowState::Imm(_) => {
          let var_name = self.def_name(target);
          self.diagnostics.push(
            DiagnosticMessage::BorrowConflictMutWhileImmutable {
              var_name,
              span: span.clone(),
            }
            .report(),
          );
        },
        BorrowState::Mut => {
          let var_name = self.def_name(target);
          self.diagnostics.push(
            DiagnosticMessage::BorrowConflictMutWhileMutable {
              var_name,
              span: span.clone(),
            }
            .report(),
          );
        },
      }
    } else {
      match current {
        BorrowState::None => {
          self.borrow_state.insert(target, BorrowState::Imm(1));
        },
        BorrowState::Imm(n) => {
          self.borrow_state.insert(target, BorrowState::Imm(n + 1));
        },
        BorrowState::Mut => {
          let var_name = self.def_name(target);
          self.diagnostics.push(
            DiagnosticMessage::BorrowConflictImmWhileMutable {
              var_name,
              span: span.clone(),
            }
            .report(),
          );
        },
      }
    }
  }

  /// Release a single borrow (decrement immutable count or clear mutable).
  fn release_borrow(
    &mut self,
    target: DefinitionId,
    mutable: bool,
  ) {
    let current = self.borrow_state.get(&target).copied().unwrap_or(BorrowState::None);

    let new_state = if mutable {
      BorrowState::None
    } else {
      match current {
        BorrowState::Imm(1) => BorrowState::None,
        BorrowState::Imm(n) if n > 1 => BorrowState::Imm(n - 1),
        other => other,
      }
    };

    if new_state == BorrowState::None {
      self.borrow_state.remove(&target);
    } else {
      self.borrow_state.insert(target, new_state);
    }
  }

  /// Pop the current scope and release all borrows registered in it.
  fn exit_block_scope(&mut self) {
    let Some(scope) = self.scope_stack.pop() else {
      return;
    };

    for borrow in &scope.borrows {
      self.release_borrow(borrow.target, borrow.mutable);
    }
  }

  /// Check if a return expression is a reference to a local variable.
  fn check_return_local_ref(
    &mut self,
    value_id: HIRId,
    span: &Span,
  ) {
    let node = self.hir.get(value_id);

    let inner = match &node.kind {
      HIRKind::Reference { expression, .. } => *expression,
      _ => {
        // Also check if the return type is a reference type (value might be a variable holding a ref)
        if matches!(self.types.get(&node.type_id), Type::Reference { .. }) {
          if let HIRKind::Variable(def_id) = &node.kind {
            if matches!(self.defs.get(def_id).kind, DefinitionKind::Variable(_)) {
              // Variable holds a reference — could be a local ref, but we can't easily
              // tell if it was derived from a local. Defer to the specific case below.
            }
          }
        }
        return;
      },
    };

    if let Some(def_id) = self.extract_root_variable(inner) {
      if matches!(self.defs.get(&def_id).kind, DefinitionKind::Variable(_)) {
        self
          .diagnostics
          .push(DiagnosticMessage::CannotReturnLocalReference { span: span.clone() }.report());
      }
    }
  }

  /// Extract the root variable DefinitionId from an HIR expression.
  /// Follows chains of FieldAccess, Dereference, Index back to the base Variable.
  fn extract_root_variable(
    &self,
    hir_id: HIRId,
  ) -> Option<DefinitionId> {
    let node = self.hir.get(hir_id);

    match &node.kind {
      HIRKind::Variable(def_id) => Some(*def_id),
      HIRKind::FieldAccess { base, .. } => self.extract_root_variable(*base),
      HIRKind::Dereference(inner) => self.extract_root_variable(*inner),
      HIRKind::Index { base, .. } => self.extract_root_variable(*base),
      _ => None,
    }
  }

  fn def_name(
    &self,
    def_id: DefinitionId,
  ) -> String {
    let def = self.defs.get(&def_id);
    self.symbols.get(&def.name).to_string()
  }

  fn merge_borrow_states(
    &self,
    a: &HashMap<DefinitionId, BorrowState>,
    b: &HashMap<DefinitionId, BorrowState>,
  ) -> HashMap<DefinitionId, BorrowState> {
    let mut result = HashMap::new();

    let all_keys: std::collections::HashSet<_> = a.keys().chain(b.keys()).collect();

    for &key in &all_keys {
      let sa = a.get(key).copied().unwrap_or(BorrowState::None);
      let sb = b.get(key).copied().unwrap_or(BorrowState::None);
      let merged = sa.merge(sb);
      if merged != BorrowState::None {
        result.insert(*key, merged);
      }
    }

    result
  }

  fn clone_scope_borrows(&self) -> Vec<Vec<ActiveBorrow>> {
    self.scope_stack.iter().map(|s| s.borrows.clone()).collect()
  }

  fn restore_scope_borrows(
    &mut self,
    saved: &[Vec<ActiveBorrow>],
  ) {
    for (scope, borrows) in self.scope_stack.iter_mut().zip(saved.iter()) {
      scope.borrows = borrows.clone();
    }
  }

  fn deduplicate_second_pass_diagnostics(
    &mut self,
    first_pass_end: usize,
  ) {
    let first_pass_messages: std::collections::HashSet<String> = self.diagnostics[..first_pass_end]
      .iter()
      .map(|d| d.message.clone())
      .collect();

    let mut i = first_pass_end;
    while i < self.diagnostics.len() {
      if first_pass_messages.contains(&self.diagnostics[i].message) {
        self.diagnostics.remove(i);
      } else {
        i += 1;
      }
    }
  }
}
