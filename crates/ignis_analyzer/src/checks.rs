use crate::{Analyzer, ScopeKind};
use ignis_ast::{expressions::ASTExpression, statements::ASTStatement, ASTNode, NodeId};
use ignis_diagnostics::message::DiagnosticMessage;

impl<'a> Analyzer<'a> {
  pub fn extra_checks_phase(
    &mut self,
    roots: &[NodeId],
  ) {
    self.reset_scopes(roots);
    for root in roots {
      self.extra_checks_node(root, ScopeKind::Global, false, false);
    }
  }

  fn extra_checks_node(
    &mut self,
    node_id: &NodeId,
    scope_kind: ScopeKind,
    in_loop: bool,
    in_function: bool,
  ) {
    let node = self.ast.get(&node_id);

    match node {
      ASTNode::Statement(stmt) => self.extra_checks_statement(node_id, stmt, scope_kind, in_loop, in_function),
      ASTNode::Expression(expr) => self.extra_checks_expression(expr, scope_kind, in_loop, in_function),
    }
  }

  fn extra_checks_statement(
    &mut self,
    node_id: &NodeId,
    stmt: &ASTStatement,
    scope_kind: ScopeKind,
    in_loop: bool,
    in_function: bool,
  ) {
    match stmt {
      ASTStatement::Function(func) => {
        let def_id = self.define_decl_in_current_scope(node_id);
        if let Some(body_id) = &func.body {
          if let Some(def_id) = &def_id {
            if self.types.get(self.type_of(&def_id)) != &ignis_type::types::Type::Void
              && !self.node_has_return(*body_id)
            {
              self.add_diagnostic(
                DiagnosticMessage::MissingReturnStatement {
                  span: func.signature.span.clone(),
                }
                .report(),
              );
            }
          }

          self.scopes.push(ScopeKind::Function);
          if let Some(def_id) = &def_id {
            self.define_function_params_in_scope(def_id);
          }

          self.extra_checks_node(body_id, ScopeKind::Function, false, true);
          self.scopes.pop();
        }
      },
      ASTStatement::Variable(var) => {
        if let Some(value_id) = &var.value {
          self.extra_checks_node(value_id, scope_kind, in_loop, in_function);
        }
        self.define_decl_in_current_scope(node_id);
      },
      ASTStatement::Constant(const_) => {
        // Only check value if it exists (not for extern const)
        if let Some(value_id) = &const_.value {
          self.extra_checks_node(value_id, scope_kind, in_loop, in_function);
        }

        self.define_decl_in_current_scope(node_id);
      },
      ASTStatement::Block(block) => {
        self.scopes.push(ScopeKind::Block);

        // Check for unreachable code after return/break/continue
        let mut found_terminator = false;

        for stmt_id in &block.statements {
          if found_terminator {
            // This statement is unreachable
            let span = self.node_span(stmt_id).clone();
            self.add_diagnostic(DiagnosticMessage::UnreachableCode { span }.report());
          }

          self.extra_checks_node(stmt_id, ScopeKind::Block, in_loop, in_function);

          // Check if this statement is a terminator (return/break/continue)
          if self.is_terminator(*stmt_id) {
            found_terminator = true;
          }
        }

        self.scopes.pop();
      },
      ASTStatement::If(if_stmt) => {
        self.extra_checks_node(&if_stmt.condition, scope_kind, in_loop, in_function);
        self.extra_checks_node(&if_stmt.then_block, ScopeKind::Block, in_loop, in_function);

        if let Some(else_branch) = &if_stmt.else_block {
          self.extra_checks_node(else_branch, ScopeKind::Block, in_loop, in_function);
        }
      },
      ASTStatement::While(while_stmt) => {
        self.scopes.push(ScopeKind::Loop);

        self.extra_checks_node(&while_stmt.condition, ScopeKind::Loop, true, in_function);
        self.extra_checks_node(&while_stmt.body, ScopeKind::Loop, true, in_function);

        self.scopes.pop();
      },
      ASTStatement::For(for_stmt) => {
        self.scopes.push(ScopeKind::Loop);

        self.extra_checks_node(&for_stmt.initializer, ScopeKind::Loop, true, in_function);
        self.extra_checks_node(&for_stmt.condition, ScopeKind::Loop, true, in_function);
        self.extra_checks_node(&for_stmt.increment, ScopeKind::Loop, true, in_function);
        self.extra_checks_node(&for_stmt.body, ScopeKind::Loop, true, in_function);

        self.scopes.pop();
      },
      ASTStatement::Break(brk) => {
        if !in_loop {
          self.add_diagnostic(
            DiagnosticMessage::BreakOutsideLoop {
              span: brk.span.clone(),
            }
            .report(),
          );
        }
      },
      ASTStatement::Continue(cont) => {
        if !in_loop {
          self.add_diagnostic(
            DiagnosticMessage::ContinueOutsideLoop {
              span: cont.span.clone(),
            }
            .report(),
          );
        }
      },
      ASTStatement::Return(ret) => {
        if !in_function {
          self.add_diagnostic(
            DiagnosticMessage::ReturnOutsideFunction {
              span: ret.span.clone(),
            }
            .report(),
          );
        }
        if let Some(value) = &ret.expression {
          self.extra_checks_node(value, scope_kind, in_loop, in_function);
        }
      },
      ASTStatement::Expression(expr) => {
        self.extra_checks_expression(expr, scope_kind, in_loop, in_function);
      },
      ASTStatement::Extern(extern_stmt) => {
        // Validate extern rules: no body for functions, no init for constants
        let item_node = self.ast.get(&extern_stmt.item);

        match item_node {
          ASTNode::Statement(ASTStatement::Function(func)) => {
            if func.body.is_some() {
              let func_name = self.get_symbol_name(&func.signature.name);
              self.add_diagnostic(
                DiagnosticMessage::ExternWithBody {
                  name: func_name,
                  span: extern_stmt.span.clone(),
                }
                .report(),
              );
            }
          },
          ASTNode::Statement(ASTStatement::Constant(const_)) => {
            // Extern constants should not have initializers
            if const_.value.is_some() {
              let const_name = self.get_symbol_name(&const_.name);
              self.add_diagnostic(
                DiagnosticMessage::ExternConstWithInitializer {
                  name: const_name,
                  span: extern_stmt.span.clone(),
                }
                .report(),
              );
            }
          },
          ASTNode::Statement(ASTStatement::Variable(var)) => {
            if var.value.is_some() {
              let var_name = self.get_symbol_name(&var.name);
              self.add_diagnostic(
                DiagnosticMessage::ExternConstWithInitializer {
                  name: var_name,
                  span: extern_stmt.span.clone(),
                }
                .report(),
              );
            }
          },
          _ => {},
        }

        self.extra_checks_node(&extern_stmt.item, scope_kind, in_loop, in_function);
      },
      ASTStatement::Export(export_stmt) => {
        if let ignis_ast::statements::ASTExport::Declaration { decl, .. } = export_stmt {
          self.extra_checks_node(decl, scope_kind, in_loop, in_function);
        }
      },
      _ => {},
    }
  }

  fn extra_checks_expression(
    &mut self,
    expr: &ASTExpression,
    scope_kind: ScopeKind,
    in_loop: bool,
    in_function: bool,
  ) {
    match expr {
      ASTExpression::Call(call) => {
        self.extra_checks_node(&call.callee, scope_kind, in_loop, in_function);
        for arg in &call.arguments {
          self.extra_checks_node(arg, scope_kind, in_loop, in_function);
        }
      },
      ASTExpression::Binary(binary) => {
        self.extra_checks_node(&binary.left, scope_kind, in_loop, in_function);
        self.extra_checks_node(&binary.right, scope_kind, in_loop, in_function);
      },
      ASTExpression::Unary(unary) => {
        self.extra_checks_node(&unary.operand, scope_kind, in_loop, in_function);
      },
      ASTExpression::Assignment(assign) => {
        self.extra_checks_node(&assign.target, scope_kind, in_loop, in_function);
        self.extra_checks_node(&assign.value, scope_kind, in_loop, in_function);
      },
      ASTExpression::Cast(cast) => {
        self.extra_checks_node(&cast.expression, scope_kind, in_loop, in_function);
      },
      ASTExpression::Reference(ref_) => {
        self.extra_checks_node(&ref_.inner, scope_kind, in_loop, in_function);
      },
      ASTExpression::Dereference(deref) => {
        self.extra_checks_node(&deref.inner, scope_kind, in_loop, in_function);
      },
      ASTExpression::VectorAccess(access) => {
        self.extra_checks_node(&access.name, scope_kind, in_loop, in_function);
        self.extra_checks_node(&access.index, scope_kind, in_loop, in_function);
      },
      ASTExpression::Grouped(grouped) => {
        self.extra_checks_node(&grouped.expression, scope_kind, in_loop, in_function);
      },
      ASTExpression::Vector(vector) => {
        for elem in &vector.items {
          self.extra_checks_node(&elem, scope_kind, in_loop, in_function);
        }
      },
      ASTExpression::Path(path) => {
        if let Some(last) = path.segments.last() {
          if self.scopes.lookup(last).is_none() {
            let symbol = self.get_symbol_name(&last);
            self.add_diagnostic(
              DiagnosticMessage::UndefinedIdentifier {
                name: symbol,
                span: path.span.clone(),
              }
              .report(),
            );
          }
        }
      },
      ASTExpression::Variable(var) => {
        if self.scopes.lookup(&var.name).is_none() {
          let symbol = self.get_symbol_name(&var.name);
          self.add_diagnostic(
            DiagnosticMessage::UndeclaredVariable {
              name: symbol,
              span: var.span.clone(),
            }
            .report(),
          );
        }
      },
      ASTExpression::Literal(_) => {},
      ASTExpression::PostfixIncrement { expr, .. } => {
        self.extra_checks_node(expr, scope_kind, in_loop, in_function);
      },
      ASTExpression::PostfixDecrement { expr, .. } => {
        self.extra_checks_node(expr, scope_kind, in_loop, in_function);
      },
    }
  }

  fn node_has_return(
    &self,
    node_id: NodeId,
  ) -> bool {
    match self.ast.get(&node_id) {
      ASTNode::Statement(stmt) => self.statement_has_return(stmt),
      ASTNode::Expression(_) => false,
    }
  }

  fn statement_has_return(
    &self,
    stmt: &ASTStatement,
  ) -> bool {
    match stmt {
      ASTStatement::Return(_) => true,
      ASTStatement::Block(block) => block.statements.iter().any(|stmt_id| self.node_has_return(*stmt_id)),
      ASTStatement::If(if_stmt) => {
        self.node_has_return(if_stmt.then_block)
          || if_stmt
            .else_block
            .as_ref()
            .map(|&else_id| self.node_has_return(else_id))
            .unwrap_or(false)
      },
      ASTStatement::While(while_stmt) => self.node_has_return(while_stmt.body),
      ASTStatement::For(for_stmt) => self.node_has_return(for_stmt.body),
      _ => false,
    }
  }

  /// Check if a statement is a terminator (return/break/continue)
  fn is_terminator(
    &self,
    node_id: NodeId,
  ) -> bool {
    match self.ast.get(&node_id) {
      ASTNode::Statement(stmt) => matches!(
        stmt,
        ASTStatement::Return(_) | ASTStatement::Break(_) | ASTStatement::Continue(_)
      ),
      _ => false,
    }
  }
}
