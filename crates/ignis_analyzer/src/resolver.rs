use crate::{Analyzer, ScopeKind};
use ignis_ast::{expressions::ASTExpression, statements::ASTStatement, ASTNode, NodeId};
use ignis_diagnostics::message::DiagnosticMessage;

impl<'a> Analyzer<'a> {
  pub fn resolve_phase(
    &mut self,
    roots: &[NodeId],
  ) {
    self.reset_scopes(roots);

    for root in roots {
      self.resolve_node(root, ScopeKind::Global);
    }
  }

  fn resolve_node(
    &mut self,
    node_id: &NodeId,
    scope_kind: ScopeKind,
  ) {
    let node = self.ast.get(&node_id);

    match node {
      ASTNode::Statement(stmt) => self.resolve_statement(node_id, stmt, scope_kind),
      ASTNode::Expression(expr) => self.resolve_expression(node_id, expr, scope_kind),
    }
  }

  fn resolve_statement(
    &mut self,
    node_id: &NodeId,
    stmt: &ASTStatement,
    scope_kind: ScopeKind,
  ) {
    match stmt {
      ASTStatement::Variable(var) => {
        if let Some(value_id) = &var.value {
          self.resolve_node(value_id, scope_kind);
        }
        self.define_decl_in_current_scope(node_id);
      },
      ASTStatement::Function(func) => {
        let def_id = self.define_decl_in_current_scope(node_id);
        self.scopes.push(ScopeKind::Function);
        if let Some(def_id) = &def_id {
          self.define_function_params_in_scope(def_id);
        }
        if let Some(body_id) = &func.body {
          self.resolve_node(body_id, ScopeKind::Function);
        }
        self.scopes.pop();
      },
      ASTStatement::Constant(const_) => {
        // Only resolve value if it exists (not for extern const)
        if let Some(value_id) = &const_.value {
          self.resolve_node(value_id, scope_kind);
        }
        self.define_decl_in_current_scope(node_id);
      },
      ASTStatement::Block(block) => {
        self.scopes.push(ScopeKind::Block);
        for stmt_id in &block.statements {
          self.resolve_node(&stmt_id, ScopeKind::Block);
        }
        self.scopes.pop();
      },
      ASTStatement::If(if_stmt) => {
        self.resolve_node(&if_stmt.condition, scope_kind);
        self.resolve_node(&if_stmt.then_block, ScopeKind::Block);
        if let Some(else_branch) = &if_stmt.else_block {
          self.resolve_node(else_branch, ScopeKind::Block);
        }
      },
      ASTStatement::While(while_stmt) => {
        self.scopes.push(ScopeKind::Loop);

        self.resolve_node(&while_stmt.condition, ScopeKind::Loop);
        self.resolve_node(&while_stmt.body, ScopeKind::Loop);

        self.scopes.pop();
      },
      ASTStatement::For(for_stmt) => {
        self.scopes.push(ScopeKind::Loop);

        self.resolve_node(&for_stmt.initializer, ScopeKind::Loop);
        self.resolve_node(&for_stmt.condition, ScopeKind::Loop);
        self.resolve_node(&for_stmt.increment, ScopeKind::Loop);
        self.resolve_node(&for_stmt.body, ScopeKind::Loop);

        self.scopes.pop();
      },
      ASTStatement::Return(ret) => {
        if let Some(value) = &ret.expression {
          self.resolve_node(value, scope_kind);
        }
      },
      ASTStatement::Expression(expr) => {
        self.resolve_expression_inline(expr, scope_kind);
      },
      ASTStatement::Extern(extern_stmt) => {
        self.resolve_node(&extern_stmt.item, scope_kind);
      },
      ASTStatement::Export(export_stmt) => match export_stmt {
        ignis_ast::statements::ASTExport::Declaration { decl, .. } => {
          self.resolve_node(&decl, scope_kind);
        },
        ignis_ast::statements::ASTExport::Name { name, span } => {
          if self.scopes.lookup(&name).is_none() {
            let symbol = self.get_symbol_name(name);
            self.add_diagnostic(
              DiagnosticMessage::UndeclaredIdentifier {
                name: symbol,
                span: span.clone(),
              }
              .report(),
            );
          }
        },
      },
      _ => {},
    }
  }

  fn resolve_expression(
    &mut self,
    node_id: &NodeId,
    expr: &ASTExpression,
    scope_kind: ScopeKind,
  ) {
    match expr {
      ASTExpression::Variable(var_expr) => {
        if let Some(def_id) = self.scopes.lookup(&var_expr.name).cloned() {
          self.set_def(node_id, &def_id);
        } else {
          let symbol = self.get_symbol_name(&var_expr.name);
          self.add_diagnostic(
            DiagnosticMessage::UndeclaredVariable {
              name: symbol,
              span: var_expr.span.clone(),
            }
            .report(),
          );
        }
      },
      ASTExpression::Call(call_expr) => {
        self.resolve_node(&call_expr.callee, scope_kind);
        for arg in &call_expr.arguments {
          self.resolve_node(&arg, scope_kind);
        }
      },
      ASTExpression::Binary(binary) => {
        self.resolve_node(&binary.left, scope_kind);
        self.resolve_node(&binary.right, scope_kind);
      },
      ASTExpression::Unary(unary) => {
        self.resolve_node(&unary.operand, scope_kind);
      },
      ASTExpression::Assignment(assign) => {
        self.resolve_node(&assign.target, scope_kind);
        self.resolve_node(&assign.value, scope_kind);
      },
      ASTExpression::Cast(cast) => {
        self.resolve_node(&cast.expression, scope_kind);
      },
      ASTExpression::Reference(ref_) => {
        self.resolve_node(&ref_.inner, scope_kind);
      },
      ASTExpression::Dereference(deref) => {
        self.resolve_node(&deref.inner, scope_kind);
      },
      ASTExpression::VectorAccess(access) => {
        self.resolve_node(&access.name, scope_kind);
        self.resolve_node(&access.index, scope_kind);
      },
      ASTExpression::Grouped(grouped) => {
        self.resolve_node(&grouped.expression, scope_kind);
      },
      ASTExpression::Vector(vector) => {
        for elem in &vector.items {
          self.resolve_node(elem, scope_kind);
        }
      },
      ASTExpression::Path(path) => {
        if let Some(last) = path.segments.last() {
          if let Some(def_id) = self.scopes.lookup(last).cloned() {
            self.set_def(node_id, &def_id);
          } else {
            let symbol = self.get_symbol_name(&last);
            self.add_diagnostic(
              DiagnosticMessage::UndeclaredIdentifier {
                name: symbol,
                span: path.span.clone(),
              }
              .report(),
            );
          }
        }
      },
      ASTExpression::Literal(_) => {},
      ASTExpression::PostfixIncrement { expr, .. } => {
        self.resolve_node(expr, scope_kind);
      },
      ASTExpression::PostfixDecrement { expr, .. } => {
        self.resolve_node(expr, scope_kind);
      },
    }
  }

  fn resolve_expression_inline(
    &mut self,
    expr: &ASTExpression,
    scope_kind: ScopeKind,
  ) {
    match expr {
      ASTExpression::Variable(var_expr) => {
        if self.scopes.lookup(&var_expr.name).is_none() {
          let symbol = self.get_symbol_name(&var_expr.name);
          self.add_diagnostic(
            DiagnosticMessage::UndeclaredVariable {
              name: symbol,
              span: var_expr.span.clone(),
            }
            .report(),
          );
        }
      },
      ASTExpression::Call(call_expr) => {
        self.resolve_node(&call_expr.callee, scope_kind);
        for arg in &call_expr.arguments {
          self.resolve_node(arg, scope_kind);
        }
      },
      ASTExpression::Binary(binary) => {
        self.resolve_node(&binary.left, scope_kind);
        self.resolve_node(&binary.right, scope_kind);
      },
      ASTExpression::Unary(unary) => {
        self.resolve_node(&unary.operand, scope_kind);
      },
      ASTExpression::Assignment(assign) => {
        self.resolve_node(&assign.target, scope_kind);
        self.resolve_node(&assign.value, scope_kind);
      },
      ASTExpression::Cast(cast) => {
        self.resolve_node(&cast.expression, scope_kind);
      },
      ASTExpression::Reference(ref_) => {
        self.resolve_node(&ref_.inner, scope_kind);
      },
      ASTExpression::Dereference(deref) => {
        self.resolve_node(&deref.inner, scope_kind);
      },
      ASTExpression::VectorAccess(access) => {
        self.resolve_node(&access.name, scope_kind);
        self.resolve_node(&access.index, scope_kind);
      },
      ASTExpression::Grouped(grouped) => {
        self.resolve_node(&grouped.expression, scope_kind);
      },
      ASTExpression::Vector(vector) => {
        for elem in &vector.items {
          self.resolve_node(elem, scope_kind);
        }
      },
      ASTExpression::Path(path) => {
        if let Some(last) = path.segments.last() {
          if self.scopes.lookup(last).is_none() {
            let symbol = self.get_symbol_name(&last);
            self.add_diagnostic(
              DiagnosticMessage::UndeclaredIdentifier {
                name: symbol,
                span: path.span.clone(),
              }
              .report(),
            );
          }
        }
      },
      ASTExpression::Literal(_) => {},
      ASTExpression::PostfixIncrement { expr, .. } => {
        self.resolve_node(expr, scope_kind);
      },
      ASTExpression::PostfixDecrement { expr, .. } => {
        self.resolve_node(expr, scope_kind);
      },
    }
  }
}
