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
        for item in &extern_stmt.items {
          self.resolve_node(item, scope_kind);
        }
      },
      ASTStatement::Namespace(ns_stmt) => {
        for item in &ns_stmt.items {
          self.resolve_node(item, scope_kind);
        }
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
        if self.is_builtin_name(&var_expr.name) {
          return;
        }

        if let Some(def_id) = self.scopes.lookup(&var_expr.name).cloned() {
          self.set_def(node_id, &def_id);
        } else {
          self.add_diagnostic(
            DiagnosticMessage::UndeclaredVariable {
              name: self.get_symbol_name(&var_expr.name),
              span: var_expr.span.clone(),
            }
            .report(),
          );
        }
      },
      ASTExpression::Call(call_expr) => {
        let prev = self.in_callee_context;
        self.in_callee_context = true;
        self.resolve_node(&call_expr.callee, scope_kind);
        self.in_callee_context = prev;

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
        let full_path = || {
          path
            .segments
            .iter()
            .map(|s| self.get_symbol_name(s))
            .collect::<Vec<_>>()
            .join("::")
        };

        if let Some(def_id) = self.resolve_qualified_path(&path.segments) {
          use ignis_type::definition::DefinitionKind;

          let def = self.defs.get(&def_id);
          match &def.kind {
            DefinitionKind::Constant(_) => {
              self.set_def(node_id, &def_id);
            },
            DefinitionKind::Function(_) => {
              if self.in_callee_context {
                self.set_def(node_id, &def_id);
              } else {
                self.add_diagnostic(
                  DiagnosticMessage::FunctionPathNotAsCallee {
                    name: full_path(),
                    span: path.span.clone(),
                  }
                  .report(),
                );
              }
            },
            _ => {
              self.add_diagnostic(
                DiagnosticMessage::UnsupportedPathExpression {
                  name: full_path(),
                  span: path.span.clone(),
                }
                .report(),
              );
            },
          }
        } else {
          self.add_diagnostic(
            DiagnosticMessage::UndeclaredIdentifier {
              name: full_path(),
              span: path.span.clone(),
            }
            .report(),
          );
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
        if self.is_builtin_name(&var_expr.name) {
          return;
        }

        if self.scopes.lookup(&var_expr.name).is_none() {
          self.add_diagnostic(
            DiagnosticMessage::UndeclaredVariable {
              name: self.get_symbol_name(&var_expr.name),
              span: var_expr.span.clone(),
            }
            .report(),
          );
        }
      },
      ASTExpression::Call(call_expr) => {
        let prev = self.in_callee_context;
        self.in_callee_context = true;
        self.resolve_node(&call_expr.callee, scope_kind);
        self.in_callee_context = prev;

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
        if self.resolve_qualified_path(&path.segments).is_none() {
          let full_path = path
            .segments
            .iter()
            .map(|s| self.get_symbol_name(s))
            .collect::<Vec<_>>()
            .join("::");
          self.add_diagnostic(
            DiagnosticMessage::UndeclaredIdentifier {
              name: full_path,
              span: path.span.clone(),
            }
            .report(),
          );
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

  /// Resolve a qualified path (`Math::add`) or simple name (`add`).
  pub fn resolve_qualified_path(
    &self,
    segments: &[ignis_type::symbol::SymbolId],
  ) -> Option<ignis_type::definition::DefinitionId> {
    use ignis_type::definition::DefinitionKind;

    if segments.is_empty() {
      return None;
    }

    if segments.len() == 1 {
      return self.scopes.lookup(&segments[0]).cloned();
    }

    let ns_path = &segments[..segments.len() - 1];
    let def_name = &segments[segments.len() - 1];

    // Check for imported namespace in scope
    if let Some(def_id) = self.scopes.lookup(&ns_path[0]) {
      let def = self.defs.get(def_id);

      if let DefinitionKind::Namespace(ns_def) = &def.kind {
        let mut current_ns = ns_def.namespace_id;

        for &segment in &ns_path[1..] {
          current_ns = self.namespaces.lookup_child(current_ns, &segment)?;
        }

        return self.namespaces.lookup_def(current_ns, def_name);
      }
    }

    // Namespace defined in current module
    let ns_id = self.namespaces.lookup(ns_path)?;
    self.namespaces.lookup_def(ns_id, def_name)
  }
}
