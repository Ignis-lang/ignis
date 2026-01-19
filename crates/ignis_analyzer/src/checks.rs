use crate::{Analyzer, ScopeKind};
use ignis_ast::{
  expressions::ASTExpression,
  statements::{
    enum_::{ASTEnum, ASTEnumItem},
    record::{ASTMethod, ASTRecord, ASTRecordItem},
    ASTStatement,
  },
  ASTNode, NodeId,
};
use ignis_diagnostics::message::DiagnosticMessage;
use ignis_type::definition::DefinitionKind;

/// Control flow termination status.
#[derive(Clone, Copy, PartialEq, Debug)]
pub enum Termination {
  Always,
  Sometimes,
}

impl Termination {
  /// Both branches must terminate for parallel merge to terminate.
  fn merge_parallel(
    self,
    other: Self,
  ) -> Self {
    match (self, other) {
      (Termination::Always, Termination::Always) => Termination::Always,
      _ => Termination::Sometimes,
    }
  }

  fn merge_sequential(
    self,
    other: Self,
  ) -> Self {
    match self {
      Termination::Always => self,
      Termination::Sometimes => other,
    }
  }
}

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
            let return_type = self.types.get(self.type_of(&def_id));
            let is_void = return_type == &ignis_type::types::Type::Void;

            if !is_void && self.check_termination(*body_id) != Termination::Always {
              self.add_diagnostic(
                DiagnosticMessage::MissingReturnStatement {
                  span: func.signature.span.clone(),
                }
                .report(),
              );
            }
          }

          let mut pushed_generic = false;

          if let Some(def_id) = &def_id {
            if let ignis_type::definition::DefinitionKind::Function(func_def) = &self.defs.get(def_id).kind {
              if !func_def.type_params.is_empty() {
                self.scopes.push(ScopeKind::Generic);
                pushed_generic = true;

                for param_id in &func_def.type_params {
                  let name = self.defs.get(param_id).name;
                  let _ = self.scopes.define(&name, param_id, false);
                }
              }
            }
          }

          self.scopes.push(ScopeKind::Function);
          if let Some(def_id) = &def_id {
            self.define_function_params_in_scope(def_id);
          }

          self.extra_checks_node(body_id, ScopeKind::Function, false, true);
          self.scopes.pop();

          if pushed_generic {
            self.scopes.pop();
          }
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
      ASTStatement::ForOf(for_of) => {
        with_for_of_scope!(self, node_id, for_of, {
          self.extra_checks_node(&for_of.iter, ScopeKind::Loop, true, in_function);
          self.extra_checks_node(&for_of.body, ScopeKind::Loop, true, in_function);
        });
      },
      ASTStatement::Break(brk) => {
        if !in_loop {
          self.add_diagnostic(DiagnosticMessage::BreakOutsideLoop { span: brk.span.clone() }.report());
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
          self.add_diagnostic(DiagnosticMessage::ReturnOutsideFunction { span: ret.span.clone() }.report());
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
        for item in &extern_stmt.items {
          let item_node = self.ast.get(item);

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

          self.extra_checks_node(item, scope_kind, in_loop, in_function);
        }
      },
      ASTStatement::Namespace(ns_stmt) => {
        for item in &ns_stmt.items {
          self.extra_checks_node(item, scope_kind, in_loop, in_function);
        }
      },
      ASTStatement::Export(export_stmt) => {
        if let ignis_ast::statements::ASTExport::Declaration { decl, .. } = export_stmt {
          self.extra_checks_node(decl, scope_kind, in_loop, in_function);
        }
      },
      ASTStatement::Record(rec) => {
        self.check_static_fields_init(rec);

        for item in &rec.items {
          if let ASTRecordItem::Method(method) = item {
            self.check_method_return(method);
          }
        }
      },
      ASTStatement::Enum(enum_) => {
        self.check_enum_fields_init(enum_);

        for item in &enum_.items {
          if let ASTEnumItem::Method(method) = item {
            self.check_method_return(method);
          }
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
      ASTExpression::Ternary(ternary) => {
        self.extra_checks_node(&ternary.condition, scope_kind, in_loop, in_function);
        self.extra_checks_node(&ternary.then_expr, scope_kind, in_loop, in_function);
        self.extra_checks_node(&ternary.else_expr, scope_kind, in_loop, in_function);
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
      ASTExpression::Path(_) => {},
      ASTExpression::Variable(var) => {
        if self.is_builtin_name(&var.name) {
          return;
        }

        if self.scopes.lookup(&var.name).is_none() {
          self.add_diagnostic(
            DiagnosticMessage::UndeclaredVariable {
              name: self.get_symbol_name(&var.name),
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
      ASTExpression::MemberAccess(ma) => {
        self.extra_checks_node(&ma.object, scope_kind, in_loop, in_function);
      },
      ASTExpression::RecordInit(ri) => {
        for field in &ri.fields {
          self.extra_checks_node(&field.value, scope_kind, in_loop, in_function);
        }
      },
    }
  }

  fn check_termination(
    &self,
    node_id: NodeId,
  ) -> Termination {
    match self.ast.get(&node_id) {
      ASTNode::Statement(stmt) => self.statement_termination(stmt),
      ASTNode::Expression(_) => Termination::Sometimes,
    }
  }

  fn statement_termination(
    &self,
    stmt: &ASTStatement,
  ) -> Termination {
    match stmt {
      ASTStatement::Return(_) | ASTStatement::Break(_) | ASTStatement::Continue(_) => Termination::Always,

      ASTStatement::Block(block) => {
        let mut result = Termination::Sometimes;
        for stmt_id in &block.statements {
          let term = self.check_termination(*stmt_id);
          result = result.merge_sequential(term);
          if result == Termination::Always {
            break;
          }
        }
        result
      },

      ASTStatement::If(if_stmt) => {
        let then_term = self.check_termination(if_stmt.then_block);
        match &if_stmt.else_block {
          Some(else_id) => then_term.merge_parallel(self.check_termination(*else_id)),
          None => Termination::Sometimes,
        }
      },

      ASTStatement::While(_) | ASTStatement::For(_) => Termination::Sometimes,

      _ => Termination::Sometimes,
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

  /// Check that static fields in a record have initializers.
  fn check_static_fields_init(
    &mut self,
    rec: &ASTRecord,
  ) {
    let type_name = self.get_symbol_name(&rec.name);

    for item in &rec.items {
      if let ASTRecordItem::Field(field) = item {
        if field.is_static() && field.value.is_none() {
          let field_name = self.get_symbol_name(&field.name);
          self.add_diagnostic(
            DiagnosticMessage::StaticFieldRequiresInit {
              field: field_name,
              type_name: type_name.clone(),
              span: field.span.clone(),
            }
            .report(),
          );
        }
      }
    }
  }

  /// Check that enum fields have initializers (all enum fields are implicitly static).
  fn check_enum_fields_init(
    &mut self,
    enum_: &ASTEnum,
  ) {
    let type_name = self.get_symbol_name(&enum_.name);

    for item in &enum_.items {
      if let ASTEnumItem::Field(field) = item {
        if field.value.is_none() {
          let field_name = self.get_symbol_name(&field.name);
          self.add_diagnostic(
            DiagnosticMessage::StaticFieldRequiresInit {
              field: field_name,
              type_name: type_name.clone(),
              span: field.span.clone(),
            }
            .report(),
          );
        }
      }
    }
  }

  /// Check that a method with non-void return type always returns a value.
  fn check_method_return(
    &mut self,
    method: &ASTMethod,
  ) {
    let Some(def_id) = self.lookup_import_item_def(&method.name_span) else {
      return;
    };

    let def = self.defs.get(def_id);
    let DefinitionKind::Method(method_def) = &def.kind else {
      return;
    };

    let return_type = self.types.get(&method_def.return_type);
    let is_void = return_type == &ignis_type::types::Type::Void;

    if !is_void && self.check_termination(method.body) != Termination::Always {
      self.add_diagnostic(
        DiagnosticMessage::MissingReturnStatement {
          span: method.span.clone(),
        }
        .report(),
      );
    }
  }
}
