use std::collections::HashMap;

use ignis_ast::{ASTNode, NodeId, expressions::ASTExpression, statements::ASTStatement};
use ignis_diagnostics::message::DiagnosticMessage;
use ignis_type::definition::{DefinitionId, DefinitionKind};

use crate::{Analyzer, ScopeKind};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BorrowState {
  None,
  Imm(usize),
  Mut,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BorrowError {
  ImmWhileMutable,
  MutWhileImmutable,
  MutWhileMutable,
}

/// Tracks borrow state for variables within scopes.
/// Borrows are released automatically when exiting a block scope via the
/// snapshot/restore mechanism in enter_block()/exit_block().
struct BorrowChecker {
  state: HashMap<DefinitionId, BorrowState>,
  scope_stack: Vec<HashMap<DefinitionId, BorrowState>>,
}

impl BorrowChecker {
  fn new() -> Self {
    Self {
      state: HashMap::new(),
      scope_stack: Vec::new(),
    }
  }

  fn enter_block(&mut self) {
    self.scope_stack.push(self.state.clone());
  }

  fn exit_block(&mut self) {
    if let Some(prev_state) = self.scope_stack.pop() {
      self.state = prev_state;
    }
  }

  fn borrow_immutable(
    &mut self,
    def_id: &DefinitionId,
  ) -> Result<(), BorrowError> {
    let current = self.state.get(def_id).copied().unwrap_or(BorrowState::None);

    match current {
      BorrowState::None => {
        self.state.insert(*def_id, BorrowState::Imm(1));
        Ok(())
      },
      BorrowState::Imm(n) => {
        self.state.insert(*def_id, BorrowState::Imm(n + 1));
        Ok(())
      },
      BorrowState::Mut => Err(BorrowError::ImmWhileMutable),
    }
  }

  fn borrow_mutable(
    &mut self,
    def_id: &DefinitionId,
  ) -> Result<(), BorrowError> {
    let current = self.state.get(def_id).copied().unwrap_or(BorrowState::None);

    match current {
      BorrowState::None => {
        self.state.insert(*def_id, BorrowState::Mut);
        Ok(())
      },
      BorrowState::Imm(_) => Err(BorrowError::MutWhileImmutable),
      BorrowState::Mut => Err(BorrowError::MutWhileMutable),
    }
  }

  fn is_borrowed(
    &self,
    def_id: &DefinitionId,
  ) -> bool {
    matches!(self.state.get(def_id), Some(BorrowState::Imm(_)) | Some(BorrowState::Mut))
  }
}

impl<'a> Analyzer<'a> {
  pub fn borrowcheck_phase(
    &mut self,
    roots: &[NodeId],
  ) {
    self.reset_scopes(roots);

    for root in roots {
      let mut checker = BorrowChecker::new();
      self.borrowcheck_node(root, &mut checker, ScopeKind::Global);
    }
  }

  fn borrowcheck_node(
    &mut self,
    node_id: &NodeId,
    checker: &mut BorrowChecker,
    scope_kind: ScopeKind,
  ) {
    let node = self.ast.get(node_id);

    match node {
      ASTNode::Statement(stmt) => self.borrowcheck_statement(node_id, stmt, checker, scope_kind),
      ASTNode::Expression(expr) => self.borrowcheck_expression(node_id, expr, checker, scope_kind),
    }
  }

  fn borrowcheck_statement(
    &mut self,
    node_id: &NodeId,
    stmt: &ASTStatement,
    checker: &mut BorrowChecker,
    scope_kind: ScopeKind,
  ) {
    match stmt {
      ASTStatement::Function(func) => {
        self.scopes.push(ScopeKind::Function);

        if let Some(def_id) = self.lookup_def(node_id).cloned() {
          self.define_function_params_in_scope(&def_id);
        }

        let mut func_checker = BorrowChecker::new();

        if let Some(body_id) = &func.body {
          self.borrowcheck_node(body_id, &mut func_checker, ScopeKind::Function);
        }

        self.scopes.pop();
      },
      ASTStatement::Block(block) => {
        checker.enter_block();
        self.scopes.push(ScopeKind::Block);

        for stmt_id in &block.statements {
          self.borrowcheck_node(stmt_id, checker, ScopeKind::Block);
        }

        self.scopes.pop();
        checker.exit_block();
      },
      ASTStatement::If(if_stmt) => {
        self.borrowcheck_node(&if_stmt.condition, checker, scope_kind);
        self.borrowcheck_node(&if_stmt.then_block, checker, scope_kind);

        if let Some(else_block) = &if_stmt.else_block {
          self.borrowcheck_node(else_block, checker, scope_kind);
        }
      },
      ASTStatement::While(while_stmt) => {
        self.scopes.push(ScopeKind::Loop);

        self.borrowcheck_node(&while_stmt.condition, checker, ScopeKind::Loop);
        self.borrowcheck_node(&while_stmt.body, checker, ScopeKind::Loop);

        self.scopes.pop();
      },
      ASTStatement::For(for_stmt) => {
        self.scopes.push(ScopeKind::Loop);

        self.borrowcheck_node(&for_stmt.initializer, checker, ScopeKind::Loop);
        self.borrowcheck_node(&for_stmt.condition, checker, ScopeKind::Loop);
        self.borrowcheck_node(&for_stmt.body, checker, ScopeKind::Loop);
        self.borrowcheck_node(&for_stmt.increment, checker, ScopeKind::Loop);

        self.scopes.pop();
      },
      ASTStatement::ForOf(for_of) => {
        self.scopes.push(ScopeKind::Loop);
        checker.enter_block();

        if let Some(def_id) = self.for_of_binding_defs.get(node_id).cloned() {
          let _ = self.scopes.define(&for_of.binding.name, &def_id, false);
        }

        self.borrowcheck_node(&for_of.iter, checker, ScopeKind::Loop);

        let is_mut_ref = self.is_for_of_mut_ref(for_of);
        let is_ref = self.is_for_of_ref(for_of);

        if is_ref || is_mut_ref {
          if let Some(iter_def_id) = self.get_iter_def_id(&for_of.iter) {
            let borrow_result = if is_mut_ref {
              checker.borrow_mutable(&iter_def_id)
            } else {
              checker.borrow_immutable(&iter_def_id)
            };

            if let Err(err) = borrow_result {
              self.emit_borrow_error(err, &iter_def_id, &for_of.span);
            }
          }
        }

        self.borrowcheck_node(&for_of.body, checker, ScopeKind::Loop);

        checker.exit_block();
        self.scopes.pop();
      },
      ASTStatement::Return(ret) => {
        if let Some(value) = &ret.expression {
          self.borrowcheck_node(value, checker, scope_kind);

          if let Some(type_id) = self.lookup_type(value) {
            if let ignis_type::types::Type::Reference { .. } = self.types.get(type_id) {
              if self.is_reference_to_local(value) {
                self.add_diagnostic(DiagnosticMessage::CannotReturnLocalReference { span: ret.span.clone() }.report());
              }
            }
          }
        }
      },
      ASTStatement::Variable(var) => {
        if let Some(value_id) = &var.value {
          self.borrowcheck_node(value_id, checker, scope_kind);
        }

        self.define_decl_in_current_scope(node_id);
      },
      ASTStatement::Constant(const_) => {
        if let Some(value_id) = &const_.value {
          self.borrowcheck_node(value_id, checker, scope_kind);
        }

        self.define_decl_in_current_scope(node_id);
      },
      ASTStatement::Expression(expr) => {
        self.borrowcheck_expression(node_id, expr, checker, scope_kind);
      },
      ASTStatement::Extern(extern_stmt) => {
        for item in &extern_stmt.items {
          self.borrowcheck_node(item, checker, scope_kind);
        }
      },
      ASTStatement::Namespace(ns_stmt) => {
        for item in &ns_stmt.items {
          self.borrowcheck_node(item, checker, scope_kind);
        }
      },
      ASTStatement::Export(export_stmt) => {
        if let ignis_ast::statements::ASTExport::Declaration { decl, .. } = export_stmt {
          self.borrowcheck_node(decl, checker, scope_kind);
        }
      },
      _ => {},
    }
  }

  fn borrowcheck_expression(
    &mut self,
    _node_id: &NodeId,
    expr: &ASTExpression,
    checker: &mut BorrowChecker,
    scope_kind: ScopeKind,
  ) {
    match expr {
      ASTExpression::Reference(ref_) => {
        self.borrowcheck_node(&ref_.inner, checker, scope_kind);

        if let Some(def_id) = self.get_def_id_from_expr(&ref_.inner) {
          let result = if ref_.mutable {
            checker.borrow_mutable(&def_id)
          } else {
            checker.borrow_immutable(&def_id)
          };

          if let Err(err) = result {
            let var_name = self.get_var_name_from_def(&def_id);
            let diagnostic = match err {
              BorrowError::ImmWhileMutable => DiagnosticMessage::BorrowConflictImmWhileMutable {
                var_name,
                span: ref_.span.clone(),
              },
              BorrowError::MutWhileImmutable => DiagnosticMessage::BorrowConflictMutWhileImmutable {
                var_name,
                span: ref_.span.clone(),
              },
              BorrowError::MutWhileMutable => DiagnosticMessage::BorrowConflictMutWhileMutable {
                var_name,
                span: ref_.span.clone(),
              },
            };
            self.add_diagnostic(diagnostic.report());
          }
        }
      },
      ASTExpression::Assignment(assign) => {
        self.borrowcheck_node(&assign.target, checker, scope_kind);
        self.borrowcheck_node(&assign.value, checker, scope_kind);

        if let Some(def_id) = self.get_def_id_from_expr(&assign.target) {
          if checker.is_borrowed(&def_id) {
            let var_name = self.get_var_name_from_def(&def_id);
            self.add_diagnostic(
              DiagnosticMessage::MutatedWhileBorrowed {
                var_name,
                span: assign.span.clone(),
              }
              .report(),
            );
          }
        }
      },
      ASTExpression::Binary(binary) => {
        self.borrowcheck_node(&binary.left, checker, scope_kind);
        self.borrowcheck_node(&binary.right, checker, scope_kind);
      },
      ASTExpression::Unary(unary) => {
        self.borrowcheck_node(&unary.operand, checker, scope_kind);
      },
      ASTExpression::Call(call) => {
        self.borrowcheck_node(&call.callee, checker, scope_kind);

        for arg in &call.arguments {
          self.borrowcheck_node(arg, checker, scope_kind);
        }
      },
      ASTExpression::Grouped(grouped) => {
        self.borrowcheck_node(&grouped.expression, checker, scope_kind);
      },
      ASTExpression::Cast(cast) => {
        self.borrowcheck_node(&cast.expression, checker, scope_kind);
      },
      ASTExpression::Dereference(deref) => {
        self.borrowcheck_node(&deref.inner, checker, scope_kind);
      },
      ASTExpression::VectorAccess(access) => {
        self.borrowcheck_node(&access.name, checker, scope_kind);
        self.borrowcheck_node(&access.index, checker, scope_kind);
      },
      ASTExpression::Vector(vector) => {
        for elem in &vector.items {
          self.borrowcheck_node(elem, checker, scope_kind);
        }
      },
      ASTExpression::PostfixIncrement { expr, .. } | ASTExpression::PostfixDecrement { expr, .. } => {
        self.borrowcheck_node(expr, checker, scope_kind);
      },
      _ => {},
    }
  }

  /// Extracts the DefinitionId from an expression if it is a simple variable or path.
  ///
  /// LIMITATION: Does not extract the root variable from compound expressions like
  /// `arr[i]`, `*ptr`, or field access. This means mutations through these expressions
  /// are not detected as borrow conflicts.
  pub(crate) fn get_def_id_from_expr(
    &self,
    node_id: &NodeId,
  ) -> Option<DefinitionId> {
    let node = self.ast.get(node_id);

    match node {
      ASTNode::Expression(ASTExpression::Variable(var)) => self.scopes.lookup_def(&var.name).copied(),
      ASTNode::Expression(ASTExpression::Path(path)) => path
        .segments
        .last()
        .and_then(|seg| self.scopes.lookup_def(&seg.name).copied()),
      _ => None,
    }
  }

  fn is_reference_to_local(
    &self,
    node_id: &NodeId,
  ) -> bool {
    if let Some(def_id) = self.get_def_id_from_expr(node_id) {
      matches!(self.defs.get(&def_id).kind, DefinitionKind::Variable(_))
    } else {
      false
    }
  }

  pub(crate) fn get_var_name_from_def(
    &self,
    def_id: &DefinitionId,
  ) -> String {
    self.get_symbol_name(&self.defs.get(def_id).name)
  }

  fn is_for_of_mut_ref(
    &self,
    for_of: &ignis_ast::statements::ASTForOf,
  ) -> bool {
    matches!(
      &for_of.binding.type_annotation,
      Some(ignis_ast::type_::IgnisTypeSyntax::Reference { mutable: true, .. })
    )
  }

  fn is_for_of_ref(
    &self,
    for_of: &ignis_ast::statements::ASTForOf,
  ) -> bool {
    matches!(
      &for_of.binding.type_annotation,
      Some(ignis_ast::type_::IgnisTypeSyntax::Reference { mutable: false, .. })
    )
  }

  fn get_iter_def_id(
    &self,
    iter_node: &NodeId,
  ) -> Option<DefinitionId> {
    let node = self.ast.get(iter_node);
    if let ASTNode::Expression(ASTExpression::Variable(var)) = node {
      self.scopes.lookup_def(&var.name).cloned()
    } else {
      None
    }
  }

  fn emit_borrow_error(
    &mut self,
    err: BorrowError,
    def_id: &DefinitionId,
    span: &ignis_type::span::Span,
  ) {
    let var_name = self.get_var_name_from_def(def_id);

    let diagnostic = match err {
      BorrowError::ImmWhileMutable => DiagnosticMessage::BorrowConflictImmWhileMutable {
        var_name,
        span: span.clone(),
      },
      BorrowError::MutWhileImmutable => DiagnosticMessage::BorrowConflictMutWhileImmutable {
        var_name,
        span: span.clone(),
      },
      BorrowError::MutWhileMutable => DiagnosticMessage::BorrowConflictMutWhileMutable {
        var_name,
        span: span.clone(),
      },
    };

    self.add_diagnostic(diagnostic.report());
  }
}
