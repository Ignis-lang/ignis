use crate::{Analyzer, InferContext, PipeArgInsertion, PipeResolution, ResolvedPath, ScopeKind, TypecheckContext};
use std::collections::{HashMap, HashSet};

use ignis_ast::{
  expressions::{
    builtin_call::ASTBuiltinCall,
    path::ASTPathSegment,
    member_access::{ASTAccessOp, ASTMemberAccess},
    record_init::ASTRecordInit,
    ASTCallExpression, ASTExpression, ASTLiteral,
  },
  pattern::ASTPattern,
  statements::{
    enum_::{ASTEnum, ASTEnumItem},
    record::{ASTRecord, ASTRecordItem},
    type_alias::ASTTypeAlias,
    ASTStatement,
  },
  type_::IgnisTypeSyntax,
  ASTNode, NodeId,
};
use ignis_diagnostics::{
  diagnostic_report::{Diagnostic, Severity},
  message::DiagnosticMessage,
};
use ignis_type::{
  attribute::FunctionAttr,
  definition::{
    ConstValue, Definition, DefinitionId, DefinitionKind, RecordFieldDef, SymbolEntry, VariableDefinition, Visibility,
  },
  inference::ConstraintReason,
  symbol::SymbolId,
  span::Span,
  types::{Substitution, Type, TypeId},
  value::IgnisLiteralValue,
};
use ignis_ast::expressions::assignment::ASTAssignmentOperator;
use ignis_ast::expressions::binary::ASTBinaryOperator;
use ignis_ast::expressions::unary::UnaryOperator;

impl<'a> Analyzer<'a> {
  pub fn typecheck_phase(
    &mut self,
    roots: &[NodeId],
  ) {
    self.reset_scopes(roots);
    let ctx = TypecheckContext::new();
    for root in roots {
      self.typecheck_node(root, ScopeKind::Global, &ctx);
    }

    self.check_duplicate_signatures();

    self.report_unresolved_nulls();

    self.zonk_all_inference_vars();
  }

  fn typecheck_node(
    &mut self,
    node_id: &NodeId,
    scope_kind: ScopeKind,
    ctx: &TypecheckContext,
  ) -> TypeId {
    self.typecheck_node_with_infer(node_id, scope_kind, ctx, &InferContext::none())
  }

  fn report_unresolved_nulls(&mut self) {
    let null_ptr = self.types.null_ptr();
    let error_type = self.types.error();

    let unresolved: Vec<NodeId> = self
      .node_types
      .iter()
      .filter_map(|(node_id, ty)| if *ty == null_ptr { Some(*node_id) } else { None })
      .collect();

    for node_id in unresolved {
      let span = self.node_span(&node_id).clone();

      self.add_diagnostic(DiagnosticMessage::CannotInferNullType { span }.report());

      self.set_type(&node_id, &error_type);
    }
  }

  fn typecheck_node_with_infer(
    &mut self,
    node_id: &NodeId,
    scope_kind: ScopeKind,
    ctx: &TypecheckContext,
    infer: &InferContext,
  ) -> TypeId {
    if let Some(ty) = self.lookup_type(node_id) {
      return *ty;
    }

    let node = self.ast.get(node_id);
    let ty = match node {
      ASTNode::Statement(stmt) => self.typecheck_statement(node_id, stmt, scope_kind, ctx),
      ASTNode::Expression(expr) => self.typecheck_expression(node_id, expr, scope_kind, ctx, infer),
    };

    self.set_type(node_id, &ty);

    ty
  }

  fn typecheck_statement(
    &mut self,
    node_id: &NodeId,
    stmt: &ASTStatement,
    scope_kind: ScopeKind,
    ctx: &TypecheckContext,
  ) -> TypeId {
    match stmt {
      ASTStatement::Variable(var) => {
        let declared_type = self.resolve_type_syntax_with_span(&var.type_, &var.span);

        let var_type = if self.types.is_infer(&declared_type) {
          if let Some(value_id) = &var.value {
            let value_type = self.typecheck_node(value_id, scope_kind, ctx);

            if self.types.is_null_ptr(&value_type) {
              let span = self.node_span(value_id).clone();
              self.add_diagnostic(DiagnosticMessage::CannotInferNullType { span }.report());
              self.set_type(value_id, &self.types.error());
              self.types.error()
            } else {
              value_type
            }
          } else {
            self.infer_ctx.fresh_var(&mut self.types, var.span.clone())
          }
        } else {
          declared_type
        };

        let lookedup_def = self.lookup_def(node_id);

        if let Some(def_id) = lookedup_def.cloned() {
          let def = self.defs.get_mut(&def_id);

          def.kind = DefinitionKind::Variable(ignis_type::definition::VariableDefinition {
            type_id: var_type,
            mutable: var.metadata.is_mutable(),
          });

          if let Some(infer_var_id) = self.types.as_infer_var(&var_type) {
            self
              .scope_infer_vars
              .entry(*self.scopes.current())
              .or_default()
              .push((def_id, infer_var_id));
          }
        }

        if let Some(value_id) = &var.value
          && !self.types.is_infer(&declared_type)
        {
          let infer = InferContext::expecting(var_type);
          let value_type = self.typecheck_node_with_infer(value_id, scope_kind, ctx, &infer);
          self.typecheck_assignment(&var_type, &value_type, &var.span);
        }

        self.define_decl_in_current_scope(node_id);

        self.types.void()
      },
      ASTStatement::LetElse(let_else) => {
        let value_type = self.typecheck_node(&let_else.value, scope_kind, ctx);

        let irrefutable = self.typecheck_pattern(&let_else.pattern, &value_type);
        if irrefutable {
          self.add_diagnostic(
            DiagnosticMessage::IrrefutableLetElsePattern {
              span: let_else.pattern.span().clone(),
            }
            .report(),
          );
        }

        self.typecheck_node(&let_else.else_block, ScopeKind::Block, ctx);

        self.types.void()
      },
      ASTStatement::Function(func) => {
        let def_id = self.define_decl_in_current_scope(node_id);

        // Push generic scope BEFORE resolving types so type params are visible
        if let Some(def_id) = &def_id {
          self.enter_type_params_scope(def_id);
        }

        let return_type = self.resolve_type_syntax_with_span(&func.signature.return_type, &func.signature.span);

        let param_ids = def_id
          .as_ref()
          .and_then(|def_id| match &self.defs.get(def_id).kind {
            DefinitionKind::Function(func_def) => Some(func_def.params.clone()),
            _ => None,
          })
          .unwrap_or_default();

        for (param, param_id) in func.signature.parameters.iter().zip(param_ids.iter()) {
          let param_type = self.resolve_type_syntax_with_span(&param.type_, &param.span);

          if let DefinitionKind::Parameter(param_def) = &mut self.defs.get_mut(param_id).kind {
            param_def.type_id = param_type;
            param_def.mutable = param.metadata.is_mutable();
          }
        }

        if let Some(def_id) = &def_id
          && let DefinitionKind::Function(func_def) = &mut self.defs.get_mut(def_id).kind
        {
          func_def.return_type = return_type;
        }

        if let Some(def_id) = &def_id {
          self.register_extension_method(def_id, &func.signature.span);
        }

        self.scopes.push(ScopeKind::Function);
        if let Some(def_id) = &def_id {
          self.define_function_params_in_scope(def_id);
        }

        let func_ctx = TypecheckContext::with_return(return_type);

        if let Some(body_id) = &func.body {
          self.typecheck_node(body_id, ScopeKind::Function, &func_ctx);
        }

        self.scopes.pop(); // Pop function scope

        // Pop generic scope if it was pushed
        if let Some(def_id) = &def_id {
          self.exit_type_params_scope(def_id);
        }

        return_type
      },
      ASTStatement::Constant(const_) => {
        let const_type = self.resolve_type_syntax_with_span(&const_.ty, &const_.span);

        if let Some(def_id) = self.lookup_def(node_id).cloned() {
          self.defs.get_mut(&def_id).kind = DefinitionKind::Constant(ignis_type::definition::ConstantDefinition {
            type_id: const_type,
            value: None,
            owner_type: None,
          });
        }

        if let Some(value_id) = &const_.value {
          let infer = InferContext::expecting(const_type);
          let value_type = self.typecheck_node_with_infer(value_id, scope_kind, ctx, &infer);
          self.typecheck_assignment(&const_type, &value_type, &const_.span);
        }

        self.define_decl_in_current_scope(node_id);

        const_type
      },
      ASTStatement::Block(block) => {
        self.scopes.push(ScopeKind::Block);
        let mut last_type = self.types.void();

        for stmt_id in &block.statements {
          last_type = self.typecheck_node(stmt_id, ScopeKind::Block, ctx);
        }

        self.scopes.pop();

        last_type
      },
      ASTStatement::If(if_stmt) => {
        self.scopes.push(ScopeKind::Block);
        self.conditional_let_context_depth += 1;

        let cond_type = self.typecheck_node(&if_stmt.condition, scope_kind, ctx);

        self.conditional_let_context_depth -= 1;

        let boolean_type = self.types.boolean();
        let conditional_span = self.node_span(&if_stmt.condition).clone();

        self.typecheck_assignment(&boolean_type, &cond_type, &conditional_span);

        let then_type = self.typecheck_node(&if_stmt.then_block, ScopeKind::Block, ctx);

        self.scopes.pop();

        let else_type = if let Some(else_branch) = &if_stmt.else_block {
          self.typecheck_node(else_branch, ScopeKind::Block, ctx)
        } else {
          self.types.void()
        };

        self.typecheck_common_type(&then_type, &else_type, &if_stmt.span)
      },
      ASTStatement::While(while_stmt) => {
        self.scopes.push(ScopeKind::Loop);

        self.scopes.push(ScopeKind::Block);
        self.conditional_let_context_depth += 1;

        let cond_type = self.typecheck_node(&while_stmt.condition, ScopeKind::Loop, ctx);

        self.conditional_let_context_depth -= 1;

        let boolean_type = self.types.boolean();
        let conditional_span = self.node_span(&while_stmt.condition).clone();

        self.typecheck_assignment(&boolean_type, &cond_type, &conditional_span);
        self.typecheck_node(&while_stmt.body, ScopeKind::Loop, ctx);

        self.scopes.pop();

        self.scopes.pop();

        self.types.void()
      },
      ASTStatement::For(for_stmt) => {
        self.scopes.push(ScopeKind::Loop);
        self.typecheck_node(&for_stmt.initializer, ScopeKind::Loop, ctx);

        let cond_type = self.typecheck_node(&for_stmt.condition, ScopeKind::Loop, ctx);
        let boolean_type = self.types.boolean();
        let conditional_span = self.node_span(&for_stmt.condition).clone();

        self.typecheck_assignment(&boolean_type, &cond_type, &conditional_span);

        self.typecheck_node(&for_stmt.increment, ScopeKind::Loop, ctx);
        self.typecheck_node(&for_stmt.body, ScopeKind::Loop, ctx);

        self.scopes.pop();

        self.types.void()
      },
      ASTStatement::ForOf(for_of) => {
        self.scopes.push(ScopeKind::Loop);

        let iter_type = self.typecheck_node(&for_of.iter, ScopeKind::Loop, ctx);

        if self.types.is_error(&iter_type) {
          self.scopes.pop();
          return self.types.void();
        }

        let element_type = match self.types.get(&iter_type) {
          Type::Vector { element, .. } => *element,
          Type::Record(def_id) => {
            let def_id = *def_id;
            match self.extract_record_iterable_element_type(&def_id) {
              Some(elem) => elem,
              None => {
                self.add_diagnostic(
                  DiagnosticMessage::ForOfExpectsVector {
                    got: self.format_type_for_error(&iter_type),
                    span: self.node_span(&for_of.iter).clone(),
                  }
                  .report(),
                );
                self.scopes.pop();
                return self.types.void();
              },
            }
          },
          _ => {
            self.add_diagnostic(
              DiagnosticMessage::ForOfExpectsVector {
                got: self.format_type_for_error(&iter_type),
                span: self.node_span(&for_of.iter).clone(),
              }
              .report(),
            );
            self.scopes.pop();
            return self.types.void();
          },
        };

        let binding_type = self.resolve_for_of_binding_type(&for_of.binding, element_type, &for_of.iter);

        if let Some(def_id) = self.for_of_binding_defs.get(node_id).cloned() {
          if let DefinitionKind::Variable(var_def) = &mut self.defs.get_mut(&def_id).kind {
            var_def.type_id = binding_type;
          }
          let _ = self.scopes.define(&for_of.binding.name, &def_id, false);
        }

        self.typecheck_node(&for_of.body, ScopeKind::Loop, ctx);

        self.scopes.pop();

        self.types.void()
      },
      ASTStatement::Return(ret) => {
        if let Some(expected_return_type) = ctx.expected_return {
          if let Some(value) = &ret.expression {
            let infer = InferContext::expecting(expected_return_type);
            let value_type = self.typecheck_node_with_infer(value, scope_kind, ctx, &infer);

            if !self.types.is_error(&value_type)
              && !self.types.is_error(&expected_return_type)
              && !matches!(self.types.get(&value_type), Type::Never)
              && !self.types.is_assignable(&expected_return_type, &value_type)
            {
              let expected = self.format_type_for_error(&expected_return_type);
              let got = self.format_type_for_error(&value_type);

              self.add_diagnostic(
                DiagnosticMessage::ReturnTypeMismatch {
                  expected,
                  got,
                  span: ret.span.clone(),
                }
                .report(),
              );
            }
          } else {
            let void_type = self.types.void();
            if !self.types.types_equal(&expected_return_type, &void_type) {
              let expected = self.format_type_for_error(&expected_return_type);

              self.add_diagnostic(
                DiagnosticMessage::MissingReturnValue {
                  expected,
                  span: ret.span.clone(),
                }
                .report(),
              );
            }
          }
        }

        self.types.never()
      },
      ASTStatement::Expression(expr) => {
        self.typecheck_expression(node_id, expr, scope_kind, ctx, &InferContext::none())
      },
      ASTStatement::Break(_) | ASTStatement::Continue(_) => self.types.never(),
      ASTStatement::Extern(extern_stmt) => {
        for item in &extern_stmt.items {
          self.typecheck_node(item, scope_kind, ctx);
        }
        self.types.void()
      },
      ASTStatement::Namespace(ns_stmt) => {
        for item in &ns_stmt.items {
          self.typecheck_node(item, scope_kind, ctx);
        }
        self.types.void()
      },
      ASTStatement::Export(export_stmt) => {
        if let ignis_ast::statements::ASTExport::Declaration { decl, .. } = export_stmt {
          self.typecheck_node(decl, scope_kind, ctx);
        }

        self.types.void()
      },
      ASTStatement::TypeAlias(ta) => {
        self.typecheck_type_alias(node_id, ta);
        self.types.void()
      },
      ASTStatement::Record(rec) => {
        self.typecheck_record(node_id, rec, scope_kind, ctx);
        self.types.void()
      },
      ASTStatement::Enum(en) => {
        self.typecheck_enum(node_id, en, scope_kind, ctx);
        self.types.void()
      },
      ASTStatement::Trait(tr) => {
        self.typecheck_trait(node_id, tr, scope_kind, ctx);
        self.types.void()
      },
      _ => self.types.void(),
    }
  }

  fn typecheck_expression(
    &mut self,
    node_id: &NodeId,
    expr: &ASTExpression,
    scope_kind: ScopeKind,
    ctx: &TypecheckContext,
    infer: &InferContext,
  ) -> TypeId {
    match expr {
      ASTExpression::Literal(lit) => self.typecheck_literal(lit, infer),
      ASTExpression::Variable(var) => {
        let entry = self.scopes.lookup(&var.name).cloned();
        match entry {
          Some(SymbolEntry::Single(def_id)) => {
            self.mark_referenced(def_id);
            let var_type = self.get_definition_type(&def_id);

            if self.types.is_infer_var(&var_type)
              && let Some(expected) = infer.expected
            {
              let _ = self.infer_ctx.unify(
                var_type,
                expected,
                &var.span,
                ConstraintReason::Argument { param_idx: 0 },
                &mut self.types,
              );

              return self.infer_ctx.resolve(var_type, &mut self.types);
            }

            var_type
          },
          Some(SymbolEntry::Overload(_)) => {
            self.add_diagnostic(
              DiagnosticMessage::OverloadGroupAsValue {
                name: self.get_symbol_name(&var.name),
                span: var.span.clone(),
              }
              .report(),
            );
            self.types.error()
          },
          None => self.types.error(),
        }
      },
      ASTExpression::Call(call) => self.typecheck_call(node_id, call, scope_kind, ctx, infer),
      ASTExpression::Binary(binary) => self.typecheck_binary(binary, scope_kind, ctx),
      ASTExpression::Ternary(ternary) => {
        let cond_type = self.typecheck_node(&ternary.condition, scope_kind, ctx);
        let boolean_type = self.types.boolean();
        let conditional_span = self.node_span(&ternary.condition).clone();

        self.typecheck_assignment(&boolean_type, &cond_type, &conditional_span);

        let then_type = self.typecheck_node(&ternary.then_expr, scope_kind, ctx);
        let else_type = self.typecheck_node(&ternary.else_expr, scope_kind, ctx);

        self.typecheck_common_type(&then_type, &else_type, &ternary.span)
      },
      ASTExpression::Unary(unary) => self.typecheck_unary(unary, scope_kind, ctx),
      ASTExpression::Assignment(assign) => self.typecheck_assignment_expr(assign, scope_kind, ctx),
      ASTExpression::Cast(cast) => {
        let expr_type = self.typecheck_node(&cast.expression, scope_kind, ctx);
        self.typecheck_cast(expr_type, &cast.target_type, &cast.span);
        self.resolve_type_syntax_with_span(&cast.target_type, &cast.span)
      },
      ASTExpression::Reference(ref_) => {
        let expr_type = self.typecheck_node(&ref_.inner, scope_kind, ctx);

        let inner_node = self.ast.get(&ref_.inner);
        if let ASTNode::Expression(inner_expr) = inner_node {
          if !self.is_lvalue(inner_expr) {
            self.add_diagnostic(
              DiagnosticMessage::InvalidReferenceTarget {
                span: ref_.span.clone(),
              }
              .report(),
            );
          }

          if ref_.mutable && !self.is_mutable_expression(inner_expr) {
            let var_name = self.get_var_name_from_expr(inner_expr);
            self.add_diagnostic(
              DiagnosticMessage::MutableReferenceToImmutable {
                var_name,
                span: ref_.span.clone(),
              }
              .report(),
            );
          }
        }

        self.types.reference(expr_type, ref_.mutable)
      },
      ASTExpression::Dereference(deref) => {
        let expr_type = self.typecheck_node(&deref.inner, scope_kind, ctx);

        if self.types.is_error(&expr_type) {
          return self.types.error();
        }

        let const_value = self.const_eval_expression_node(&deref.inner, scope_kind);

        if matches!(const_value, Some(ConstValue::Null)) {
          self.add_diagnostic(
            DiagnosticMessage::NullDereference {
              span: deref.span.clone(),
            }
            .report(),
          );

          self.set_type(&deref.inner, &self.types.error());
        }

        if self.types.is_null_ptr(&expr_type) {
          return self.types.error();
        }

        match self.types.get(&expr_type).clone() {
          ignis_type::types::Type::Pointer { inner, .. } => inner,
          ignis_type::types::Type::Reference { inner, .. } => inner,
          _ => {
            let type_name = self.format_type_for_error(&expr_type);
            self.add_diagnostic(
              DiagnosticMessage::DereferenceNonPointer {
                type_name,
                span: deref.span.clone(),
              }
              .report(),
            );
            self.types.error()
          },
        }
      },
      ASTExpression::VectorAccess(access) => {
        let base_type = self.typecheck_node(&access.name, scope_kind, ctx);
        let index_type = self.typecheck_node(&access.index, scope_kind, ctx);

        if !self.types.is_error(&index_type) && !self.is_integer_type(&index_type) {
          self.add_diagnostic(
            DiagnosticMessage::VectorIndexNonInteger {
              index_type: self.format_type_for_error(&index_type),
              span: access.span.clone(),
            }
            .report(),
          );
        }

        if self.types.is_error(&base_type) {
          return self.types.error();
        }

        match self.types.get(&base_type).clone() {
          ignis_type::types::Type::Vector { element, size } => {
            // Compile-time bounds checking for constant indices
            if let Some(ConstValue::Int(index_val)) = self.const_eval_expression_node(&access.index, scope_kind)
              && (index_val < 0 || (index_val as usize) >= size)
            {
              self.add_diagnostic(
                DiagnosticMessage::IndexOutOfBounds {
                  index: index_val,
                  size,
                  span: access.span.clone(),
                }
                .report(),
              );
            }
            element
          },
          ignis_type::types::Type::Pointer { inner, .. } => {
            // Pointers are indexable, return the inner type
            // No bounds checking for raw pointers
            inner
          },
          _ => {
            if !self.types.is_error(&base_type) {
              self.add_diagnostic(
                DiagnosticMessage::AccessNonVector {
                  type_name: self.format_type_for_error(&base_type),
                  span: access.span.clone(),
                }
                .report(),
              );
            }
            self.types.error()
          },
        }
      },
      ASTExpression::Grouped(grouped) => self.typecheck_node(&grouped.expression, scope_kind, ctx),
      ASTExpression::Vector(vector) => {
        for elem in &vector.items {
          self.typecheck_node(elem, scope_kind, ctx);
        }

        let elem_type = vector
          .items
          .first()
          .and_then(|e| self.lookup_type(e).copied())
          .unwrap_or(self.types.error());

        self.types.vector(elem_type, vector.items.len())
      },
      ASTExpression::Path(path) => {
        // Track path segment spans for hover support
        self.track_path_segment_spans(path);
        self.mark_path_prefix_referenced(&path.segments);

        // Resolve the path to get the definition type
        match self.resolve_qualified_path(&path.segments) {
          Some(ResolvedPath::Entry(entry)) => match entry {
            SymbolEntry::Single(def_id) => self.get_definition_type(&def_id),
            SymbolEntry::Overload(_) => {
              self.add_diagnostic(
                DiagnosticMessage::OverloadGroupAsValue {
                  name: path
                    .segments
                    .iter()
                    .map(|s| self.get_symbol_name(&s.name))
                    .collect::<Vec<_>>()
                    .join("::"),
                  span: path.span.clone(),
                }
                .report(),
              );
              self.types.error()
            },
          },
          Some(ResolvedPath::EnumVariant {
            enum_def,
            variant_index,
          }) => {
            // Get the enum definition to check if variant requires payload
            if let DefinitionKind::Enum(ed) = &self.defs.get(&enum_def).kind {
              let variant = &ed.variants[variant_index as usize];

              if !variant.payload.is_empty() {
                // Variant requires payload - emit error
                let variant_name = self.get_symbol_name(&variant.name);
                self.add_diagnostic(
                  DiagnosticMessage::EnumVariantRequiresPayload {
                    variant: variant_name,
                    expected: variant.payload.len(),
                    span: path.span.clone(),
                  }
                  .report(),
                );
                return self.types.error();
              }

              // For generic enums, try to infer type args from expected type
              if !ed.type_params.is_empty()
                && let Some(expected) = &infer.expected
                && let Type::Instance { generic, args } = self.types.get(expected).clone()
                && generic == enum_def
              {
                return self.types.instance(enum_def, args);
              }

              ed.type_id
            } else {
              self.types.error()
            }
          },
          None => {
            if path.segments.len() == 2
              && let Some(enum_def_id) = self.scopes.lookup_def(&path.segments[0].name).cloned()
              && matches!(self.defs.get(&enum_def_id).kind, DefinitionKind::Enum(_))
            {
              let already_reported = self
                .diagnostics
                .iter()
                .any(|diagnostic| diagnostic.error_code == "A0059" && diagnostic.primary_span == path.span);

              if !already_reported {
                self.add_diagnostic(
                  DiagnosticMessage::EnumVariantNotFound {
                    enum_name: self.get_symbol_name(&path.segments[0].name),
                    variant: self.get_symbol_name(&path.segments[1].name),
                    span: path.span.clone(),
                  }
                  .report(),
                );
              }
            }

            self.types.error()
          },
        }
      },
      ASTExpression::PostfixIncrement { expr, span } => {
        let expr_type = self.typecheck_node(expr, scope_kind, ctx);

        let target_node = self.ast.get(expr);
        if let ASTNode::Expression(target_expr) = target_node
          && !self.is_mutable_expression(target_expr)
        {
          let var_name = self.get_var_name_from_expr(target_expr);
          self.add_diagnostic(
            DiagnosticMessage::ImmutableAssignment {
              var_name,
              span: span.clone(),
            }
            .report(),
          );
        }

        if self.types.is_numeric(&expr_type) {
          expr_type
        } else {
          self.types.error()
        }
      },
      ASTExpression::PostfixDecrement { expr, span } => {
        let expr_type = self.typecheck_node(expr, scope_kind, ctx);

        let target_node = self.ast.get(expr);
        if let ASTNode::Expression(target_expr) = target_node
          && !self.is_mutable_expression(target_expr)
        {
          let var_name = self.get_var_name_from_expr(target_expr);
          self.add_diagnostic(
            DiagnosticMessage::ImmutableAssignment {
              var_name,
              span: span.clone(),
            }
            .report(),
          );
        }

        if self.types.is_numeric(&expr_type) {
          expr_type
        } else {
          self.types.error()
        }
      },
      ASTExpression::MemberAccess(ma) => self.typecheck_member_access(ma, scope_kind, ctx, infer),
      ASTExpression::RecordInit(ri) => self.typecheck_record_init(ri, scope_kind, ctx, infer),
      ASTExpression::BuiltinCall(bc) => self.typecheck_builtin_call(node_id, bc, scope_kind, ctx),
      ASTExpression::LetCondition(let_condition) => {
        let value_type = self.typecheck_node(&let_condition.value, scope_kind, ctx);

        if self.conditional_let_context_depth == 0 {
          self.add_diagnostic(
            DiagnosticMessage::LetConditionOutsideConditional {
              span: let_condition.span.clone(),
            }
            .report(),
          );

          self.scopes.push(ScopeKind::Block);
          self.typecheck_pattern(&let_condition.pattern, &value_type);
          self.scopes.pop();
        } else {
          self.typecheck_pattern(&let_condition.pattern, &value_type);
        }

        self.types.boolean()
      },
      ASTExpression::Match(match_expr) => {
        if match_expr.arms.is_empty() {
          self.add_diagnostic(
            DiagnosticMessage::MatchNeedsAtLeastOneArm {
              span: match_expr.span.clone(),
            }
            .report(),
          );
          return self.types.error();
        }

        let scrutinee_type = self.typecheck_node(&match_expr.scrutinee, scope_kind, ctx);

        if self.types.is_error(&scrutinee_type) {
          return self.types.error();
        }

        let mut arm_types = Vec::new();
        let mut saw_irrefutable_arm = false;

        for arm in &match_expr.arms {
          if saw_irrefutable_arm {
            self.add_diagnostic(DiagnosticMessage::UnreachableMatchArm { span: arm.span.clone() }.report());
          }

          self.scopes.push(ScopeKind::Block);

          let irrefutable = self.typecheck_pattern(&arm.pattern, &scrutinee_type);

          if let Some(guard) = arm.guard.as_ref() {
            let guard_type = self.typecheck_node(guard, ScopeKind::Block, ctx);
            if !self.types.types_equal(&guard_type, &self.types.boolean()) && !self.types.is_error(&guard_type) {
              self.add_diagnostic(
                DiagnosticMessage::GuardNotBoolean {
                  got: self.format_type_for_error(&guard_type),
                  span: self.node_span(guard).clone(),
                }
                .report(),
              );
            }
          }

          arm_types.push(self.typecheck_node_with_infer(&arm.body, ScopeKind::Block, ctx, infer));

          self.scopes.pop();

          if irrefutable && arm.guard.is_none() {
            saw_irrefutable_arm = true;
          }
        }

        if !saw_irrefutable_arm && !self.is_match_exhaustive(scrutinee_type, &match_expr.arms) {
          self.add_diagnostic(
            DiagnosticMessage::NonExhaustiveMatch {
              span: match_expr.span.clone(),
            }
            .report(),
          );
        }

        let Some(first_type) = arm_types.first().copied() else {
          return self.types.void();
        };

        arm_types.iter().skip(1).fold(first_type, |current, next| {
          if !self.types.types_equal(&current, next) && !self.types.is_error(&current) && !self.types.is_error(next) {
            self.add_diagnostic(
              DiagnosticMessage::MatchArmTypeMismatch {
                expected: self.format_type_for_error(&current),
                got: self.format_type_for_error(next),
                span: match_expr.span.clone(),
              }
              .report(),
            );
          }
          self.typecheck_common_type(&current, next, &match_expr.span)
        })
      },
      ASTExpression::Lambda(lambda) => self.typecheck_lambda(node_id, lambda, infer),
      ASTExpression::CaptureOverride(co) => self.typecheck_node(&co.inner, scope_kind, ctx),
      ASTExpression::Pipe {
        lhs,
        rhs,
        pipe_span,
        span,
      } => self.typecheck_pipe(node_id, lhs, rhs, pipe_span, span, scope_kind, ctx),

      ASTExpression::PipePlaceholder { span, .. } => {
        self.add_diagnostic(
          DiagnosticMessage::PipePlaceholderOutsidePipe { span: span.clone() }.report(),
        );
        self.types.error()
      },
    }
  }

  fn typecheck_pattern(
    &mut self,
    pattern: &ASTPattern,
    expected_type: &TypeId,
  ) -> bool {
    match pattern {
      ASTPattern::Wildcard { .. } => true,
      ASTPattern::Literal { value, span } => {
        let literal_type = match value {
          IgnisLiteralValue::Int8(_) => self.types.i8(),
          IgnisLiteralValue::Int16(_) => self.types.i16(),
          IgnisLiteralValue::Int32(_) => self.types.i32(),
          IgnisLiteralValue::Int64(_) => self.types.i64(),
          IgnisLiteralValue::UnsignedInt8(_) => self.types.u8(),
          IgnisLiteralValue::UnsignedInt16(_) => self.types.u16(),
          IgnisLiteralValue::UnsignedInt32(_) => self.types.u32(),
          IgnisLiteralValue::UnsignedInt64(_) => self.types.u64(),
          IgnisLiteralValue::Float32(_) => self.types.f32(),
          IgnisLiteralValue::Float64(_) => self.types.f64(),
          IgnisLiteralValue::Boolean(_) => self.types.boolean(),
          IgnisLiteralValue::Char(_) => self.types.char(),
          IgnisLiteralValue::String(_) => self.types.str(),
          IgnisLiteralValue::Atom(_) => self.types.atom(),
          IgnisLiteralValue::Hex(_) => self.types.u32(),
          IgnisLiteralValue::Binary(_) => self.types.u8(),
          IgnisLiteralValue::Null => self.types.null_ptr(),
        };
        self.typecheck_assignment(expected_type, &literal_type, span);
        false
      },
      ASTPattern::Path { segments, args, span } => {
        let path_segments: Vec<ASTPathSegment> = segments
          .iter()
          .map(|(name, segment_span)| ASTPathSegment::new(*name, segment_span.clone()))
          .collect();

        if let Some(ResolvedPath::EnumVariant {
          enum_def,
          variant_index,
        }) = self.resolve_qualified_path(&path_segments)
        {
          let expected_enum = self.resolve_expected_enum_for_pattern(expected_type);

          if let Some((expected_enum_def, subst)) = expected_enum {
            if expected_enum_def != enum_def {
              let enum_name = self.get_symbol_name(&self.defs.get(&enum_def).name);
              self.add_diagnostic(
                DiagnosticMessage::PatternTypeMismatch {
                  expected: self.format_type_for_error(expected_type),
                  got: enum_name,
                  span: span.clone(),
                }
                .report(),
              );
              return false;
            }

            if let DefinitionKind::Enum(ed) = &self.defs.get(&enum_def).kind {
              let variant = &ed.variants[variant_index as usize];
              let expected_payload: Vec<TypeId> = variant
                .payload
                .iter()
                .map(|payload_ty| self.types.substitute(*payload_ty, &subst))
                .collect();
              let arg_count = args.as_ref().map_or(0, |patterns| patterns.len());

              if arg_count != expected_payload.len() {
                let enum_name = self.get_symbol_name(&self.defs.get(&enum_def).name);
                let variant_name = format!("{}::{}", enum_name, self.get_symbol_name(&variant.name));
                self.add_diagnostic(
                  DiagnosticMessage::VariantPayloadArityMismatch {
                    variant: variant_name,
                    expected: expected_payload.len(),
                    got: arg_count,
                    span: span.clone(),
                  }
                  .report(),
                );
              }

              if let Some(pattern_args) = args {
                for (i, arg_pattern) in pattern_args.iter().enumerate() {
                  if let Some(payload_ty) = expected_payload.get(i) {
                    self.typecheck_pattern(arg_pattern, payload_ty);
                  }
                }
              }
            }
            return false;
          }

          self.add_diagnostic(
            DiagnosticMessage::PatternTypeMismatch {
              expected: self.format_type_for_error(expected_type),
              got: "enum variant".to_string(),
              span: span.clone(),
            }
            .report(),
          );
          return false;
        }

        if segments.len() == 1 && args.is_none() {
          let (name, _) = &segments[0];
          let name_str = self.symbols.borrow().get(name).to_string();

          if name_str == "_" {
            return true;
          }

          self.define_pattern_binding_if_absent(*name, span, *expected_type);
          return true;
        }

        if let Some((first, _)) = segments.first()
          && let Some((last, _)) = segments.last()
        {
          self.add_diagnostic(
            DiagnosticMessage::UnknownVariant {
              enum_name: self.get_symbol_name(first),
              variant: self.get_symbol_name(last),
              span: span.clone(),
            }
            .report(),
          );
        }

        false
      },
      ASTPattern::Tuple { elements, .. } => {
        if let Type::Tuple(tuple_elements) = self.types.get(expected_type).clone() {
          for (i, elem_pattern) in elements.iter().enumerate() {
            if i < tuple_elements.len() {
              self.typecheck_pattern(elem_pattern, &tuple_elements[i]);
            }
          }
        } else {
          self.add_diagnostic(
            DiagnosticMessage::PatternTypeMismatch {
              expected: self.format_type_for_error(expected_type),
              got: "tuple".to_string(),
              span: pattern.span().clone(),
            }
            .report(),
          );
        }
        false
      },
      ASTPattern::Or { patterns, span } => {
        if patterns.iter().any(|p| self.pattern_has_bindings(p)) {
          self.add_diagnostic(DiagnosticMessage::OrPatternBindingsDisallowed { span: span.clone() }.report());
        }
        for p in patterns {
          self.typecheck_pattern(p, expected_type);
        }
        patterns.iter().all(|p| matches!(p, ASTPattern::Wildcard { .. }))
      },
    }
  }

  fn pattern_has_bindings(
    &self,
    pattern: &ASTPattern,
  ) -> bool {
    match pattern {
      ASTPattern::Wildcard { .. } | ASTPattern::Literal { .. } => false,
      ASTPattern::Path { segments, args, .. } => {
        if segments.len() == 1 && args.is_none() {
          let (name, segment_span) = &segments[0];
          let name_str = self.symbols.borrow().get(name).to_string();
          if name_str == "_" {
            false
          } else {
            let path = [ASTPathSegment::new(*name, segment_span.clone())];
            !matches!(self.resolve_qualified_path(&path), Some(ResolvedPath::EnumVariant { .. }))
          }
        } else {
          args
            .as_ref()
            .is_some_and(|a| a.iter().any(|p| self.pattern_has_bindings(p)))
        }
      },
      ASTPattern::Tuple { elements, .. } => elements.iter().any(|e| self.pattern_has_bindings(e)),
      ASTPattern::Or { patterns, .. } => patterns.iter().any(|p| self.pattern_has_bindings(p)),
    }
  }

  fn resolve_expected_enum_for_pattern(
    &self,
    expected_type: &TypeId,
  ) -> Option<(DefinitionId, Substitution)> {
    let mut current_type = *expected_type;

    loop {
      match self.types.get(&current_type).clone() {
        Type::Reference { inner, .. } => {
          current_type = inner;
        },
        Type::Enum(def_id) => return Some((def_id, Substitution::new())),
        Type::Instance { generic, args } => {
          if let DefinitionKind::Enum(ed) = &self.defs.get(&generic).kind {
            if ed.type_params.len() == args.len() {
              return Some((generic, Substitution::for_generic(generic, &args)));
            }

            return Some((generic, Substitution::new()));
          }

          return None;
        },
        _ => return None,
      }
    }
  }

  fn is_match_exhaustive(
    &self,
    scrutinee_type: TypeId,
    arms: &[ignis_ast::expressions::match_expression::ASTMatchArm],
  ) -> bool {
    let mut current_type = scrutinee_type;
    loop {
      let Type::Reference { inner, .. } = self.types.get(&current_type).clone() else {
        break;
      };
      current_type = inner;
    }

    match self.types.get(&current_type).clone() {
      Type::Boolean => {
        let mut has_true = false;
        let mut has_false = false;

        for arm in arms {
          if arm.guard.is_some() {
            continue;
          }

          self.collect_bool_coverage(&arm.pattern, &mut has_true, &mut has_false);
        }

        has_true && has_false
      },
      Type::Enum(enum_def) => self.is_enum_match_exhaustive(enum_def, arms),
      Type::Instance { generic, .. } => {
        if matches!(self.defs.get(&generic).kind, DefinitionKind::Enum(_)) {
          self.is_enum_match_exhaustive(generic, arms)
        } else {
          false
        }
      },
      _ => false,
    }
  }

  fn collect_bool_coverage(
    &self,
    pattern: &ASTPattern,
    has_true: &mut bool,
    has_false: &mut bool,
  ) {
    match pattern {
      ASTPattern::Literal {
        value: IgnisLiteralValue::Boolean(value),
        ..
      } => {
        if *value {
          *has_true = true;
        } else {
          *has_false = true;
        }
      },
      ASTPattern::Or { patterns, .. } => {
        for member in patterns {
          self.collect_bool_coverage(member, has_true, has_false);
        }
      },
      _ => {},
    }
  }

  fn is_enum_match_exhaustive(
    &self,
    enum_def: DefinitionId,
    arms: &[ignis_ast::expressions::match_expression::ASTMatchArm],
  ) -> bool {
    let DefinitionKind::Enum(enum_definition) = &self.defs.get(&enum_def).kind else {
      return false;
    };

    let total_variants = enum_definition.variants.len();
    if total_variants == 0 {
      return false;
    }

    let mut covered = std::collections::HashSet::new();
    for arm in arms {
      if arm.guard.is_some() {
        continue;
      }

      self.collect_enum_variant_coverage(&arm.pattern, enum_def, &mut covered);
    }

    covered.len() == total_variants
  }

  fn collect_enum_variant_coverage(
    &self,
    pattern: &ASTPattern,
    enum_def: DefinitionId,
    covered: &mut std::collections::HashSet<u32>,
  ) {
    match pattern {
      ASTPattern::Path { segments, .. } => {
        let path_segments: Vec<ASTPathSegment> = segments
          .iter()
          .map(|(name, span)| ASTPathSegment::new(*name, span.clone()))
          .collect();

        if let Some(ResolvedPath::EnumVariant {
          enum_def: found_enum,
          variant_index,
        }) = self.resolve_qualified_path(&path_segments)
          && found_enum == enum_def
        {
          covered.insert(variant_index);
        }
      },
      ASTPattern::Or { patterns, .. } => {
        for member in patterns {
          self.collect_enum_variant_coverage(member, enum_def, covered);
        }
      },
      _ => {},
    }
  }

  pub(crate) fn define_pattern_binding_if_absent(
    &mut self,
    name: ignis_type::symbol::SymbolId,
    span: &Span,
    type_id: TypeId,
  ) {
    let current_scope = *self.scopes.current();

    if let Some(def_id) = self
      .scopes
      .get_scope(&current_scope)
      .symbols
      .get(&name)
      .and_then(|entry| entry.as_single())
      .copied()
    {
      if type_id != self.types.error()
        && let DefinitionKind::Variable(var_def) = &mut self.defs.get_mut(&def_id).kind
        && var_def.type_id == self.types.error()
      {
        var_def.type_id = type_id;
      }
      return;
    }

    let existing_def = self.defs.iter().find_map(|(def_id, def)| {
      if matches!(def.kind, DefinitionKind::Variable(_)) && def.name == name && def.name_span == *span {
        Some(def_id)
      } else {
        None
      }
    });

    if let Some(def_id) = existing_def {
      if type_id != self.types.error()
        && let DefinitionKind::Variable(var_def) = &mut self.defs.get_mut(&def_id).kind
        && var_def.type_id == self.types.error()
      {
        var_def.type_id = type_id;
      }

      let _ = self.scopes.define(&name, &def_id, false);
      return;
    }

    let var_def = VariableDefinition {
      type_id,
      mutable: false,
    };

    let def = Definition {
      kind: DefinitionKind::Variable(var_def),
      name,
      span: span.clone(),
      name_span: span.clone(),
      visibility: Visibility::Private,
      owner_module: self.current_module,
      owner_namespace: self.current_namespace,
      doc: None,
    };

    let def_id = self.defs.alloc(def);

    if let Err(existing) = self.scopes.define(&name, &def_id, false) {
      let existing_def = self.defs.get(&existing);
      let symbol = self.get_symbol_name(&existing_def.name);
      self.add_diagnostic(
        DiagnosticMessage::VariableAlreadyDefined {
          name: symbol,
          span: span.clone(),
          previous_span: existing_def.span.clone(),
        }
        .report(),
      );
    }
  }

  fn typecheck_type_alias(
    &mut self,
    node_id: &NodeId,
    ta: &ASTTypeAlias,
  ) {
    let Some(def_id) = self.lookup_def(node_id).cloned() else {
      return;
    };

    // Cycle detection: track that we're resolving this alias.
    self.resolving_type_aliases.insert(def_id);

    self.enter_type_params_scope(&def_id);
    let target_type = self.resolve_type_syntax_with_span(&ta.target, &ta.span);
    self.exit_type_params_scope(&def_id);

    if let DefinitionKind::TypeAlias(alias_def) = &mut self.defs.get_mut(&def_id).kind {
      alias_def.target = target_type;
    }

    // Remove from tracking sets after resolution completes.
    self.resolving_type_aliases.remove(&def_id);
    self.type_alias_syntax.remove(&def_id);

    self.define_decl_in_current_scope(node_id);
  }

  // ========================================================================
  // Record Typechecking
  // ========================================================================

  fn typecheck_record(
    &mut self,
    node_id: &NodeId,
    rec: &ASTRecord,
    scope_kind: ScopeKind,
    ctx: &TypecheckContext,
  ) {
    let Some(record_def_id) = self.lookup_def(node_id).cloned() else {
      return;
    };

    self.define_decl_in_current_scope(node_id);

    // Push generic scope BEFORE resolving types so type params are visible
    self.enter_type_params_scope(&record_def_id);

    // Get original field definitions (from binder) that have def_ids
    let original_fields = if let DefinitionKind::Record(rd) = &self.defs.get(&record_def_id).kind {
      rd.fields.clone()
    } else {
      Vec::new()
    };

    // Collect resolved field types
    let mut resolved_fields: Vec<RecordFieldDef> = Vec::new();
    let mut field_index = 0u32;

    // First pass: resolve field types (so methods can access them)
    for item in &rec.items {
      if let ASTRecordItem::Field(field) = item {
        let field_type = self.resolve_type_syntax_with_span(&field.type_, &field.span);

        if field.is_static() {
          // Static field - update the constant definition
          if let DefinitionKind::Record(rd) = &self.defs.get(&record_def_id).kind
            && let Some(const_def_id) = rd.static_fields.get(&field.name).cloned()
          {
            if let DefinitionKind::Constant(const_def) = &mut self.defs.get_mut(&const_def_id).kind {
              const_def.type_id = field_type;
            }

            // Typecheck the initializer if present
            if let Some(value_id) = &field.value {
              let infer = InferContext::expecting(field_type);
              let value_type = self.typecheck_node_with_infer(value_id, scope_kind, ctx, &infer);
              self.typecheck_assignment(&field_type, &value_type, &field.span);
            }
          }
        } else {
          // Instance field - preserve the def_id and attrs from binder
          let original = original_fields
            .get(field_index as usize)
            .expect("field_index out of sync with original_fields");
          let def_id = original.def_id;

          // Update the field definition with resolved type
          if let DefinitionKind::Field(field_def) = &mut self.defs.get_mut(&def_id).kind {
            field_def.type_id = field_type;
          }

          resolved_fields.push(RecordFieldDef {
            name: field.name,
            type_id: field_type,
            index: field_index,
            span: field.span.clone(),
            def_id,
            attrs: original.attrs.clone(),
          });
          field_index += 1;
        }
      }
    }

    // Update record definition with resolved field types BEFORE typechecking methods
    // so that method bodies can access field types correctly
    if let DefinitionKind::Record(rd) = &mut self.defs.get_mut(&record_def_id).kind {
      rd.fields = resolved_fields;
    }

    // Second pass: resolve method signatures (return type, param types)
    // This must happen before typechecking bodies so methods can call each other
    for item in &rec.items {
      if let ASTRecordItem::Method(method) = item {
        let method_def_id = if let DefinitionKind::Record(rd) = &self.defs.get(&record_def_id).kind {
          let entry = if method.is_static() {
            rd.static_methods.get(&method.name)
          } else {
            rd.instance_methods.get(&method.name)
          };
          match entry {
            Some(SymbolEntry::Single(def_id)) => Some(*def_id),
            Some(SymbolEntry::Overload(group)) => {
              group.iter().copied().find(|id| self.defs.get(id).span == method.span)
            },
            None => None,
          }
        } else {
          None
        };

        if let Some(method_def_id) = method_def_id {
          self.resolve_method_signature(&method_def_id, method);
        }
      }
    }

    // Third pass: typecheck method bodies (after ALL signatures are resolved)
    for item in &rec.items {
      if let ASTRecordItem::Method(method) = item {
        let method_def_id = if let DefinitionKind::Record(rd) = &self.defs.get(&record_def_id).kind {
          let entry = if method.is_static() {
            rd.static_methods.get(&method.name)
          } else {
            rd.instance_methods.get(&method.name)
          };
          match entry {
            Some(SymbolEntry::Single(def_id)) => Some(*def_id),
            Some(SymbolEntry::Overload(group)) => {
              group.iter().copied().find(|id| self.defs.get(id).span == method.span)
            },
            None => None,
          }
        } else {
          None
        };

        if let Some(method_def_id) = method_def_id {
          self.typecheck_method(&method_def_id, method, record_def_id, scope_kind, ctx);
        }
      }
    }

    self.validate_lang_trait_methods(&record_def_id, &rec.span);
    self.validate_trait_implementations(&record_def_id, &rec.span);

    // Pop generic scope if it was pushed
    self.exit_type_params_scope(&record_def_id);
  }

  // ========================================================================
  // Enum Typechecking
  // ========================================================================

  fn typecheck_enum(
    &mut self,
    node_id: &NodeId,
    en: &ASTEnum,
    scope_kind: ScopeKind,
    ctx: &TypecheckContext,
  ) {
    let Some(enum_def_id) = self.lookup_def(node_id).cloned() else {
      return;
    };

    self.define_decl_in_current_scope(node_id);

    // Push generic scope BEFORE resolving types so type params are visible
    self.enter_type_params_scope(&enum_def_id);

    // Clone variants to update payload types
    let mut updated_variants = if let DefinitionKind::Enum(ed) = &self.defs.get(&enum_def_id).kind {
      ed.variants.clone()
    } else {
      return;
    };

    let mut variant_idx = 0usize;

    // First pass: resolve variants and fields, and method signatures
    for item in &en.items {
      match item {
        ASTEnumItem::Variant(variant) => {
          // Resolve payload types
          let payload_types: Vec<TypeId> = variant
            .payload
            .iter()
            .map(|ty| self.resolve_type_syntax_with_span(ty, &variant.span))
            .collect();

          if variant_idx < updated_variants.len() {
            // Update variant payload types
            updated_variants[variant_idx].payload = payload_types.clone();

            // Also update the VariantDefinition with resolved payload types
            let variant_def_id = updated_variants[variant_idx].def_id;
            if let DefinitionKind::Variant(vd) = &mut self.defs.get_mut(&variant_def_id).kind {
              vd.payload = payload_types;
            }
          }
          variant_idx += 1;
        },
        ASTEnumItem::Method(method) => {
          let method_def_id = if let DefinitionKind::Enum(ed) = &self.defs.get(&enum_def_id).kind {
            let is_static = method.is_static() || !method.has_self();
            let source = if is_static {
              &ed.static_methods
            } else {
              &ed.instance_methods
            };
            match source.get(&method.name) {
              Some(SymbolEntry::Single(def_id)) => Some(*def_id),
              Some(SymbolEntry::Overload(group)) => {
                group.iter().copied().find(|id| self.defs.get(id).span == method.span)
              },
              None => None,
            }
          } else {
            None
          };

          if let Some(method_def_id) = method_def_id {
            self.resolve_method_signature(&method_def_id, method);
          }
        },
        ASTEnumItem::Field(field) => {
          let field_type = self.resolve_type_syntax_with_span(&field.type_, &field.span);

          // Update the constant definition
          if let DefinitionKind::Enum(ed) = &self.defs.get(&enum_def_id).kind
            && let Some(const_def_id) = ed.static_fields.get(&field.name).cloned()
          {
            if let DefinitionKind::Constant(const_def) = &mut self.defs.get_mut(&const_def_id).kind {
              const_def.type_id = field_type;
            }

            // Typecheck the initializer if present
            if let Some(value_id) = &field.value {
              let infer = InferContext::expecting(field_type);
              let value_type = self.typecheck_node_with_infer(value_id, scope_kind, ctx, &infer);
              self.typecheck_assignment(&field_type, &value_type, &field.span);
            }
          }
        },
      }
    }

    // Second pass: typecheck method bodies
    for item in &en.items {
      if let ASTEnumItem::Method(method) = item {
        let method_def_id = if let DefinitionKind::Enum(ed) = &self.defs.get(&enum_def_id).kind {
          let is_static = method.is_static() || !method.has_self();
          let source = if is_static {
            &ed.static_methods
          } else {
            &ed.instance_methods
          };
          match source.get(&method.name) {
            Some(SymbolEntry::Single(def_id)) => Some(*def_id),
            Some(SymbolEntry::Overload(group)) => {
              group.iter().copied().find(|id| self.defs.get(id).span == method.span)
            },
            None => None,
          }
        } else {
          None
        };

        if let Some(method_def_id) = method_def_id {
          self.typecheck_method(&method_def_id, method, enum_def_id, scope_kind, ctx);
        }
      }
    }

    // Update enum definition with resolved variant payload types
    if let DefinitionKind::Enum(ed) = &mut self.defs.get_mut(&enum_def_id).kind {
      ed.variants = updated_variants;
    }

    self.validate_lang_trait_methods(&enum_def_id, &en.span);

    // Pop generic scope if it was pushed
    self.exit_type_params_scope(&enum_def_id);
  }

  // ========================================================================
  // Trait Typechecking
  // ========================================================================

  fn typecheck_trait(
    &mut self,
    node_id: &NodeId,
    tr: &ignis_ast::statements::ASTTrait,
    _scope_kind: ScopeKind,
    _ctx: &TypecheckContext,
  ) {
    let Some(trait_def_id) = self.lookup_def(node_id).cloned() else {
      return;
    };

    self.enter_type_params_scope(&trait_def_id);

    let method_entries = if let DefinitionKind::Trait(td) = &self.defs.get(&trait_def_id).kind {
      td.methods.clone()
    } else {
      return;
    };

    // Resolve method signatures (params and return types).
    // Default method bodies are deferred — typechecked later per-implementing record.
    for entry in &method_entries {
      let Some(ast_method) = tr.methods.iter().find(|m| m.name == entry.name) else {
        continue;
      };

      let method_def_id = entry.method_def_id;
      self.enter_type_params_scope(&method_def_id);

      let return_type = self.resolve_type_syntax_with_span(&ast_method.return_type, &ast_method.span);

      let param_def_ids = if let DefinitionKind::Method(md) = &self.defs.get(&method_def_id).kind {
        md.params.clone()
      } else {
        continue;
      };

      for (param, param_def_id) in ast_method.parameters.iter().zip(param_def_ids.iter()) {
        let param_type = self.resolve_type_syntax_with_span(&param.type_, &param.span);
        if let DefinitionKind::Parameter(pd) = &mut self.defs.get_mut(param_def_id).kind {
          pd.type_id = param_type;
        }
      }

      if let DefinitionKind::Method(md) = &mut self.defs.get_mut(&method_def_id).kind {
        md.return_type = return_type;
      }

      self.exit_type_params_scope(&method_def_id);
    }

    // Store AST body NodeIds for default methods (typechecked later per-implementing record)
    for entry in &method_entries {
      if !entry.has_default {
        continue;
      }

      let Some(ast_method) = tr.methods.iter().find(|m| m.name == entry.name) else {
        continue;
      };

      let Some(body_node) = &ast_method.body else {
        continue;
      };

      self.trait_default_bodies.insert(entry.method_def_id, *body_node);
    }

    self.exit_type_params_scope(&trait_def_id);
    self.define_decl_in_current_scope(node_id);
  }

  /// Validate that a record correctly implements all its declared traits.
  /// Called after record methods have been typechecked.
  fn validate_trait_implementations(
    &mut self,
    record_def_id: &DefinitionId,
    record_span: &Span,
  ) {
    let type_def = self.defs.get(record_def_id);
    let type_name = self.get_symbol_name(&type_def.name);

    let (implemented_traits, instance_methods) = match &type_def.kind {
      DefinitionKind::Record(rd) => (rd.implemented_traits.clone(), rd.instance_methods.clone()),
      _ => return,
    };

    if implemented_traits.is_empty() {
      return;
    }

    for trait_def_id in &implemented_traits {
      let trait_def = self.defs.get(trait_def_id);
      let trait_name = self.get_symbol_name(&trait_def.name);

      let method_entries = if let DefinitionKind::Trait(td) = &trait_def.kind {
        td.methods.clone()
      } else {
        continue;
      };

      for entry in &method_entries {
        let method_name = self.get_symbol_name(&entry.name);
        let method_entry = instance_methods.get(&entry.name);

        match method_entry {
          Some(sym_entry) => {
            let impl_def_id = match sym_entry {
              SymbolEntry::Single(id) => *id,
              SymbolEntry::Overload(group) => {
                // Trait params exclude self; record params include self as [0]
                let trait_method = self.defs.get(&entry.method_def_id);
                let trait_param_count = if let DefinitionKind::Method(md) = &trait_method.kind {
                  md.params.len()
                } else {
                  0
                };

                let found = group.iter().copied().find(|id| {
                  if let DefinitionKind::Method(md) = &self.defs.get(id).kind {
                    let explicit = if !md.is_static && !md.params.is_empty() {
                      md.params.len() - 1
                    } else {
                      md.params.len()
                    };
                    explicit == trait_param_count
                  } else {
                    false
                  }
                });

                match found {
                  Some(id) => id,
                  None => {
                    self.add_diagnostic(
                      DiagnosticMessage::TraitMissingRequiredMethod {
                        trait_name: trait_name.clone(),
                        method_name: method_name.clone(),
                        type_name: type_name.clone(),
                        span: record_span.clone(),
                      }
                      .report(),
                    );
                    continue;
                  },
                }
              },
            };

            self.validate_trait_method_signature(
              &entry.method_def_id,
              &impl_def_id,
              &trait_name,
              &method_name,
              record_span,
            );
          },

          None => {
            if entry.has_default {
              let cloned_id = self.clone_trait_default_for_record(&entry.method_def_id, record_def_id, record_span);
              if let DefinitionKind::Record(rd) = &mut self.defs.get_mut(record_def_id).kind {
                rd.instance_methods.insert(entry.name, SymbolEntry::Single(cloned_id));
              }
            } else {
              self.add_diagnostic(
                DiagnosticMessage::TraitMissingRequiredMethod {
                  trait_name: trait_name.clone(),
                  method_name: method_name.clone(),
                  type_name: type_name.clone(),
                  span: record_span.clone(),
                }
                .report(),
              );
            }
          },
        }
      }
    }
  }

  /// Validate that a record method's signature matches the trait method's signature.
  fn validate_trait_method_signature(
    &mut self,
    trait_method_id: &DefinitionId,
    impl_method_id: &DefinitionId,
    trait_name: &str,
    method_name: &str,
    span: &Span,
  ) {
    let trait_method = self.defs.get(trait_method_id);
    let impl_method = self.defs.get(impl_method_id);

    let (trait_return, trait_self_mut) = match &trait_method.kind {
      DefinitionKind::Method(md) => (md.return_type, md.self_mutable),
      _ => return,
    };

    let (impl_return, impl_self_mut, impl_is_static) = match &impl_method.kind {
      DefinitionKind::Method(md) => (md.return_type, md.self_mutable, md.is_static),
      _ => return,
    };

    // Trait method params are only explicit params (no injected self).
    let trait_explicit_params = match &trait_method.kind {
      DefinitionKind::Method(md) => md.params.clone(),
      _ => return,
    };

    // Record method params have `self` injected as params[0] by typecheck_method
    // for instance methods. Skip it to get only explicit params.
    let impl_explicit_params: Vec<_> = match &impl_method.kind {
      DefinitionKind::Method(md) => {
        if !impl_is_static && !md.params.is_empty() {
          md.params[1..].to_vec()
        } else {
          md.params.clone()
        }
      },
      _ => return,
    };

    if trait_self_mut != impl_self_mut {
      let expected = if trait_self_mut { "&mut self" } else { "&self" };
      let got = if impl_self_mut { "&mut self" } else { "&self" };
      self.add_diagnostic(
        DiagnosticMessage::TraitMethodSignatureMismatch {
          trait_name: trait_name.to_string(),
          method_name: method_name.to_string(),
          expected: expected.to_string(),
          got: got.to_string(),
          span: span.clone(),
        }
        .report(),
      );
      return;
    }

    if trait_explicit_params.len() != impl_explicit_params.len() {
      let expected = format!("{} parameters", trait_explicit_params.len());
      let got = format!("{} parameters", impl_explicit_params.len());
      self.add_diagnostic(
        DiagnosticMessage::TraitMethodSignatureMismatch {
          trait_name: trait_name.to_string(),
          method_name: method_name.to_string(),
          expected,
          got,
          span: span.clone(),
        }
        .report(),
      );
      return;
    }

    for (trait_param_id, impl_param_id) in trait_explicit_params.iter().zip(impl_explicit_params.iter()) {
      let trait_type = self.defs.type_of(trait_param_id);
      let impl_type = self.defs.type_of(impl_param_id);
      if trait_type != impl_type {
        let expected = self.format_type_for_error(trait_type);
        let got = self.format_type_for_error(impl_type);
        self.add_diagnostic(
          DiagnosticMessage::TraitMethodSignatureMismatch {
            trait_name: trait_name.to_string(),
            method_name: method_name.to_string(),
            expected,
            got,
            span: span.clone(),
          }
          .report(),
        );
        return;
      }
    }

    if trait_return != impl_return {
      let expected = self.format_type_for_error(&trait_return);
      let got = self.format_type_for_error(&impl_return);
      self.add_diagnostic(
        DiagnosticMessage::TraitMethodSignatureMismatch {
          trait_name: trait_name.to_string(),
          method_name: method_name.to_string(),
          expected,
          got,
          span: span.clone(),
        }
        .report(),
      );
    }
  }

  /// Clone a trait's default method for a record, giving it the record's self type.
  /// Typechecks the body in the record's scope and registers the clone for HIR lowering.
  fn clone_trait_default_for_record(
    &mut self,
    trait_method_id: &DefinitionId,
    record_def_id: &DefinitionId,
    record_span: &Span,
  ) -> DefinitionId {
    let trait_method = self.defs.get(trait_method_id);
    let (params, return_type, self_mutable, type_params, inline_mode, attrs) =
      if let DefinitionKind::Method(md) = &trait_method.kind {
        (
          md.params.clone(),
          md.return_type,
          md.self_mutable,
          md.type_params.clone(),
          md.inline_mode,
          md.attrs.clone(),
        )
      } else {
        return *trait_method_id;
      };

    let method_name = trait_method.name;
    let method_span = trait_method.span.clone();
    let method_name_span = trait_method.name_span.clone();
    let method_doc = trait_method.doc.clone();

    let record_type = *self.defs.type_of(record_def_id);
    let self_ref_type = self.types.reference(record_type, self_mutable);

    let self_symbol = self.symbols.borrow_mut().intern("self");
    let self_def = ignis_type::definition::Definition {
      kind: DefinitionKind::Parameter(ignis_type::definition::ParameterDefinition {
        type_id: self_ref_type,
        mutable: self_mutable,
        attrs: vec![],
      }),
      name: self_symbol,
      span: method_span.clone(),
      name_span: record_span.clone(),
      visibility: ignis_type::definition::Visibility::Private,
      owner_module: self.current_module,
      owner_namespace: None,
      doc: None,
    };
    let self_def_id = self.defs.alloc(self_def);

    let mut new_params = vec![self_def_id];
    new_params.extend(params.clone());

    let new_def = ignis_type::definition::Definition {
      kind: DefinitionKind::Method(ignis_type::definition::MethodDefinition {
        owner_type: *record_def_id,
        type_params,
        params: new_params,
        return_type,
        is_static: false,
        self_mutable,
        inline_mode,
        attrs,
      }),
      name: method_name,
      span: method_span,
      name_span: method_name_span,
      visibility: ignis_type::definition::Visibility::Public,
      owner_module: self.current_module,
      owner_namespace: self.defs.get(record_def_id).owner_namespace,
      doc: method_doc,
    };
    let cloned_id = self.defs.alloc(new_def);

    if let Some(&body_node) = self.trait_default_bodies.get(trait_method_id) {
      self.enter_type_params_scope(&cloned_id);
      self.scopes.push(ScopeKind::Function);

      let _ = self.scopes.define(&self_symbol, &self_def_id, false);
      for &param_def_id in &params {
        let param_def = self.defs.get(&param_def_id);
        let _ = self.scopes.define(&param_def.name, &param_def_id, false);
      }

      let infer = InferContext::expecting(return_type);
      self.typecheck_node_with_infer(
        &body_node,
        ScopeKind::Function,
        &TypecheckContext::with_return(return_type),
        &infer,
      );

      self.scopes.pop();
      self.exit_type_params_scope(&cloned_id);

      self.trait_default_clones.insert(cloned_id, body_node);
    }

    cloned_id
  }

  // ========================================================================
  // Method Typechecking (shared by Record and Enum)
  // ========================================================================

  /// Resolve method signature (return type and parameter types) without typechecking the body.
  /// This allows all method signatures to be resolved before any bodies are typechecked,
  /// enabling methods to call each other regardless of declaration order.
  fn resolve_method_signature(
    &mut self,
    method_def_id: &ignis_type::definition::DefinitionId,
    method: &ignis_ast::statements::record::ASTMethod,
  ) {
    // Push method's type params scope (in addition to owner's type params already in scope)
    self.enter_type_params_scope(method_def_id);

    // Resolve return type
    let return_type = self.resolve_type_syntax_with_span(&method.return_type, &method.span);

    // Get parameter definition ids
    let param_def_ids = if let DefinitionKind::Method(md) = &self.defs.get(method_def_id).kind {
      md.params.clone()
    } else {
      return;
    };

    // Resolve parameter types
    for (param, param_def_id) in method.parameters.iter().zip(param_def_ids.iter()) {
      let param_type = self.resolve_type_syntax_with_span(&param.type_, &param.span);

      if let DefinitionKind::Parameter(param_def) = &mut self.defs.get_mut(param_def_id).kind {
        param_def.type_id = param_type;
        param_def.mutable = param.metadata.is_mutable();
      }
    }

    // Update method definition with resolved return type
    if let DefinitionKind::Method(md) = &mut self.defs.get_mut(method_def_id).kind {
      md.return_type = return_type;
    }

    // Pop method's type params scope
    self.exit_type_params_scope(method_def_id);
  }

  fn typecheck_method(
    &mut self,
    method_def_id: &ignis_type::definition::DefinitionId,
    method: &ignis_ast::statements::record::ASTMethod,
    owner_def_id: ignis_type::definition::DefinitionId,
    scope_kind: ScopeKind,
    _ctx: &TypecheckContext,
  ) {
    // Get the return type that was resolved in resolve_method_signature
    let return_type = if let DefinitionKind::Method(md) = &self.defs.get(method_def_id).kind {
      md.return_type
    } else {
      return;
    };

    // Get parameter definition ids
    let param_def_ids = if let DefinitionKind::Method(md) = &self.defs.get(method_def_id).kind {
      md.params.clone()
    } else {
      return;
    };

    // Push method's type params scope (in addition to owner's type params already in scope)
    self.enter_type_params_scope(method_def_id);

    // Typecheck the method body
    self.scopes.push(ScopeKind::Function);

    // For instance methods, inject `self` parameter
    let (is_static, self_mutable) = if let DefinitionKind::Method(md) = &self.defs.get(method_def_id).kind {
      (md.is_static, md.self_mutable)
    } else {
      (true, false)
    };

    if !is_static {
      let owner_def = self.defs.get(&owner_def_id);

      // For generic owners, self should be Type::Instance so it gets substituted during monomorphization
      let self_type = match &owner_def.kind {
        DefinitionKind::Record(rd) if !rd.type_params.is_empty() => {
          let type_params: Vec<TypeId> = rd
            .type_params
            .iter()
            .map(|&tp_def| {
              let tp_def_kind = &self.defs.get(&tp_def).kind;
              if let DefinitionKind::TypeParam(tp) = tp_def_kind {
                self.types.param(tp.owner, tp.index)
              } else {
                panic!("expected TypeParam in record type_params");
              }
            })
            .collect();
          self.types.instance(owner_def_id, type_params)
        },
        DefinitionKind::Enum(ed) if !ed.type_params.is_empty() => {
          let type_params: Vec<TypeId> = ed
            .type_params
            .iter()
            .map(|&tp_def| {
              let tp_def_kind = &self.defs.get(&tp_def).kind;
              if let DefinitionKind::TypeParam(tp) = tp_def_kind {
                self.types.param(tp.owner, tp.index)
              } else {
                panic!("expected TypeParam in enum type_params");
              }
            })
            .collect();
          self.types.instance(owner_def_id, type_params)
        },
        _ => *self.defs.type_of(&owner_def_id),
      };

      // Use &mut Self for methods with &mut self, &Self otherwise
      let self_ref_type = self.types.reference(self_type, self_mutable);

      let self_symbol = self.symbols.borrow_mut().intern("self");
      let self_def = ignis_type::definition::Definition {
        kind: DefinitionKind::Parameter(ignis_type::definition::ParameterDefinition {
          type_id: self_ref_type,
          mutable: self_mutable,
          attrs: vec![],
        }),
        name: self_symbol,
        span: method.span.clone(),
        name_span: method.span.clone(),
        visibility: ignis_type::definition::Visibility::Private,
        owner_module: self.current_module,
        owner_namespace: None,
        doc: None,
      };
      let self_def_id = self.defs.alloc(self_def);
      let _ = self.scopes.define(&self_symbol, &self_def_id, false);

      // Add self to the method's params as the first parameter
      if let DefinitionKind::Method(md) = &mut self.defs.get_mut(method_def_id).kind {
        md.params.insert(0, self_def_id);
      }
    } else {
      let self_symbol = self.symbols.borrow_mut().intern("self");

      if let Some(span) = self.find_first_symbol_usage(method.body, self_symbol) {
        self.add_diagnostic(
          DiagnosticMessage::MethodUsesSelfWithoutSelfParameter {
            method_name: self.get_symbol_name(&method.name),
            span,
          }
          .report(),
        );
      }
    }

    // Register explicit parameters in scope
    for param_def_id in &param_def_ids {
      let param_def = self.defs.get(param_def_id);
      let _ = self.scopes.define(&param_def.name, param_def_id, false);
    }

    // Typecheck body with expected return type
    let method_ctx = TypecheckContext::with_return(return_type);
    self.typecheck_node(&method.body, scope_kind, &method_ctx);

    self.scopes.pop();

    // Pop method's type params scope
    self.exit_type_params_scope(method_def_id);
  }

  // ========================================================================
  // Member Access Typechecking
  // ========================================================================

  fn typecheck_member_access(
    &mut self,
    ma: &ASTMemberAccess,
    scope_kind: ScopeKind,
    ctx: &TypecheckContext,
    infer: &InferContext,
  ) -> TypeId {
    match ma.op {
      ASTAccessOp::Dot => self.typecheck_dot_access(ma, scope_kind, ctx),
      ASTAccessOp::DoubleColon => self.typecheck_static_access(ma, scope_kind, ctx, infer),
    }
  }

  /// Checks if we're currently inside a method of the given record.
  ///
  /// This is used for visibility checks: private fields/methods are accessible
  /// from within the record's own methods.
  fn is_inside_record_method(
    &self,
    record_def_id: &DefinitionId,
  ) -> bool {
    let self_symbol = self.symbols.borrow_mut().intern("self");

    let Some(self_def_id) = self.scopes.lookup_def(&self_symbol) else {
      return false;
    };

    let self_def = self.defs.get(self_def_id);
    let DefinitionKind::Parameter(param) = &self_def.kind else {
      return false;
    };

    // self is always a reference (&Self or &mut Self)
    let self_type = self.types.get(&param.type_id);
    let Type::Reference { inner, .. } = self_type else {
      return false;
    };

    // Get the underlying record/instance type
    let inner_type = self.types.get(inner);
    let self_record_def_id = match inner_type {
      Type::Record(def_id) => def_id,
      Type::Instance { generic, .. } => generic,
      _ => return false,
    };

    self_record_def_id == record_def_id
  }

  fn typecheck_dot_access(
    &mut self,
    ma: &ASTMemberAccess,
    scope_kind: ScopeKind,
    ctx: &TypecheckContext,
  ) -> TypeId {
    let obj_type = self.typecheck_node(&ma.object, scope_kind, ctx);
    let obj_type = self.auto_deref(obj_type);

    if self.types.is_error(&obj_type) {
      return self.types.error();
    }

    // Extract definition and optional type args for records and instances
    let (def_id, type_args) = match self.types.get(&obj_type).clone() {
      Type::Record(def_id) => (def_id, vec![]),
      Type::Instance { generic, args } => (generic, args),
      Type::Enum(def_id) => {
        if let DefinitionKind::Enum(ed) = &self.defs.get(&def_id).kind
          && ed.instance_methods.contains_key(&ma.member)
        {
          let member_name = self.get_symbol_name(&ma.member);
          self.add_diagnostic(
            DiagnosticMessage::MethodMustBeCalled {
              method: member_name,
              span: ma.span.clone(),
            }
            .report(),
          );
          return self.types.error();
        }
        self.add_diagnostic(DiagnosticMessage::DotAccessOnEnum { span: ma.span.clone() }.report());
        return self.types.error();
      },
      _ => {
        let type_name = self.format_type_for_error(&obj_type);
        self.add_diagnostic(
          DiagnosticMessage::DotAccessOnNonRecord {
            type_name,
            span: ma.span.clone(),
          }
          .report(),
        );
        return self.types.error();
      },
    };

    // Check if it's a record definition
    let rd = match &self.defs.get(&def_id).kind {
      DefinitionKind::Record(rd) => rd.clone(),
      DefinitionKind::Enum(ed) => {
        if ed.instance_methods.contains_key(&ma.member) {
          let member_name = self.get_symbol_name(&ma.member);
          self.add_diagnostic(
            DiagnosticMessage::MethodMustBeCalled {
              method: member_name,
              span: ma.span.clone(),
            }
            .report(),
          );
          return self.types.error();
        }
        self.add_diagnostic(DiagnosticMessage::DotAccessOnEnum { span: ma.span.clone() }.report());
        return self.types.error();
      },
      _ => {
        let type_name = self.format_type_for_error(&obj_type);
        self.add_diagnostic(
          DiagnosticMessage::DotAccessOnNonRecord {
            type_name,
            span: ma.span.clone(),
          }
          .report(),
        );
        return self.types.error();
      },
    };

    // Build substitution for generic instances
    let subst = if !type_args.is_empty() && type_args.len() == rd.type_params.len() {
      Substitution::for_generic(def_id, &type_args)
    } else {
      Substitution::new()
    };

    // Check instance fields first
    if let Some(field) = rd.fields.iter().find(|f| f.name == ma.member) {
      // Check visibility
      let field_def = self.defs.get(&field.def_id);
      if field_def.visibility == Visibility::Private && !self.is_inside_record_method(&def_id) {
        let field_name = self.get_symbol_name(&ma.member);
        let type_name = self.format_type_for_error(&obj_type);
        self.add_diagnostic(
          DiagnosticMessage::PrivateFieldAccess {
            field: field_name,
            type_name,
            span: ma.member_span.clone(),
          }
          .report(),
        );
        return self.types.error();
      }

      self.set_import_item_def(&ma.member_span, &field.def_id);
      return self.types.substitute(field.type_id, &subst);
    }

    // Check instance methods
    if rd.instance_methods.contains_key(&ma.member) {
      // Method access without call - this is an error (must be called)
      let member_name = self.get_symbol_name(&ma.member);
      self.add_diagnostic(
        DiagnosticMessage::MethodMustBeCalled {
          method: member_name,
          span: ma.span.clone(),
        }
        .report(),
      );
      return self.types.error();
    }

    // Field not found
    let type_name = self.format_type_for_error(&obj_type);
    let member_name = self.get_symbol_name(&ma.member);
    self.add_diagnostic(
      DiagnosticMessage::FieldNotFound {
        field: member_name,
        type_name,
        span: ma.span.clone(),
      }
      .report(),
    );
    self.types.error()
  }

  fn typecheck_static_access(
    &mut self,
    ma: &ASTMemberAccess,
    scope_kind: ScopeKind,
    ctx: &TypecheckContext,
    infer: &InferContext,
  ) -> TypeId {
    // The object should resolve to a type name (Record, Enum, or Namespace)
    // First, try to resolve it as a path expression to a type
    let def_id = self.resolve_type_expression(&ma.object, scope_kind, ctx);

    let Some(def_id) = def_id else {
      self.add_diagnostic(DiagnosticMessage::StaticAccessOnNonType { span: ma.span.clone() }.report());
      return self.types.error();
    };

    self.register_type_expression_span(&ma.object, &def_id);

    match &self.defs.get(&def_id).kind.clone() {
      DefinitionKind::Record(rd) => {
        // Static method?
        if let Some(method_id) = rd.static_methods.get(&ma.member).and_then(|e| e.as_single()).cloned() {
          self.set_import_item_def(&ma.member_span, &method_id);
          // Method access without call - error
          let member_name = self.get_symbol_name(&ma.member);
          self.add_diagnostic(
            DiagnosticMessage::MethodMustBeCalled {
              method: member_name,
              span: ma.span.clone(),
            }
            .report(),
          );
          return self.get_definition_type(&method_id);
        }
        // Static field?
        if let Some(field_id) = rd.static_fields.get(&ma.member).cloned() {
          self.set_import_item_def(&ma.member_span, &field_id);
          return self.get_definition_type(&field_id);
        }
        // Not found
        let type_name = self.get_symbol_name(&self.defs.get(&def_id).name);
        let member_name = self.get_symbol_name(&ma.member);
        self.add_diagnostic(
          DiagnosticMessage::StaticMemberNotFound {
            member: member_name,
            type_name,
            span: ma.span.clone(),
          }
          .report(),
        );
        self.types.error()
      },
      DefinitionKind::Enum(ed) => {
        // Enum variant?
        if let Some(&tag) = ed.variants_by_name.get(&ma.member) {
          let variant = &ed.variants[tag as usize];
          if variant.payload.is_empty() {
            // Unit variant - returns enum type
            // For generic enums, try to infer type args from expected type
            if !ed.type_params.is_empty()
              && let Some(expected) = &infer.expected
              && let Type::Instance { generic, args } = self.types.get(expected).clone()
              && generic == def_id
            {
              return self.types.instance(def_id, args);
            }
            return ed.type_id;
          } else {
            // Variant with payload - must be called
            let variant_name = self.get_symbol_name(&ma.member);
            self.add_diagnostic(
              DiagnosticMessage::EnumVariantRequiresPayload {
                variant: variant_name,
                expected: variant.payload.len(),
                span: ma.span.clone(),
              }
              .report(),
            );
            return self.types.error();
          }
        }
        // Static method?
        if let Some(method_id) = ed.static_methods.get(&ma.member).and_then(|e| e.as_single()).cloned() {
          self.set_import_item_def(&ma.member_span, &method_id);
          let member_name = self.get_symbol_name(&ma.member);
          self.add_diagnostic(
            DiagnosticMessage::MethodMustBeCalled {
              method: member_name,
              span: ma.span.clone(),
            }
            .report(),
          );
          return self.get_definition_type(&method_id);
        }
        // Static field?
        if let Some(field_id) = ed.static_fields.get(&ma.member).cloned() {
          self.set_import_item_def(&ma.member_span, &field_id);
          return self.get_definition_type(&field_id);
        }
        // Not found
        let type_name = self.get_symbol_name(&self.defs.get(&def_id).name);
        let member_name = self.get_symbol_name(&ma.member);
        self.add_diagnostic(
          DiagnosticMessage::StaticMemberNotFound {
            member: member_name,
            type_name,
            span: ma.span.clone(),
          }
          .report(),
        );
        self.types.error()
      },
      _ => {
        self.add_diagnostic(DiagnosticMessage::StaticAccessOnNonType { span: ma.span.clone() }.report());
        self.types.error()
      },
    }
  }

  /// Try to resolve an expression as a type definition (Record, Enum, Namespace, TypeAlias)
  fn resolve_type_expression(
    &mut self,
    node_id: &NodeId,
    _scope_kind: ScopeKind,
    _ctx: &TypecheckContext,
  ) -> Option<ignis_type::definition::DefinitionId> {
    let node = self.ast.get(node_id);

    match node {
      ASTNode::Expression(ASTExpression::Variable(var)) => {
        // Simple identifier - look up in scope
        self.scopes.lookup_def(&var.name).cloned()
      },
      ASTNode::Expression(ASTExpression::Path(path)) => {
        // Qualified path - resolve through namespaces
        self.mark_path_prefix_referenced(&path.segments);
        self.resolve_qualified_path_to_def(path)
      },
      ASTNode::Expression(ASTExpression::MemberAccess(ma)) => {
        // Nested access like Foo::Bar::Baz
        let base_def = self.resolve_type_expression(&ma.object, _scope_kind, _ctx)?;

        match &self.defs.get(&base_def).kind {
          DefinitionKind::Namespace(ns_def) => self
            .namespaces
            .lookup_def(ns_def.namespace_id, &ma.member)
            .and_then(|e| e.as_single())
            .cloned(),
          _ => None,
        }
      },
      _ => None,
    }
  }

  /// Register span mappings for a type expression (enables hover).
  fn register_type_expression_span(
    &mut self,
    node_id: &NodeId,
    def_id: &DefinitionId,
  ) {
    let node = self.ast.get(node_id);

    match node {
      ASTNode::Expression(ASTExpression::Variable(var)) => {
        self.set_import_item_def(&var.span, def_id);
      },
      ASTNode::Expression(ASTExpression::Path(path)) => {
        self.register_path_spans(path);
      },
      ASTNode::Expression(ASTExpression::MemberAccess(ma)) => {
        self.set_import_item_def(&ma.member_span, def_id);
        if let Some(base_def) =
          self.resolve_type_expression(&ma.object, ScopeKind::Global, &TypecheckContext::default())
        {
          self.register_type_expression_span(&ma.object, &base_def);
        }
      },
      _ => {},
    }
  }

  /// Register span mappings for each segment of a path.
  fn register_path_spans(
    &mut self,
    path: &ignis_ast::expressions::path::ASTPath,
  ) {
    let Some(first_segment) = path.segments.first() else {
      return;
    };
    let Some(first_def) = self.scopes.lookup_def(&first_segment.name).cloned() else {
      return;
    };

    self.set_import_item_def(&first_segment.span, &first_def);
    let mut current_def = first_def;

    for segment in path.segments.iter().skip(1) {
      let DefinitionKind::Namespace(ns_def) = self.defs.get(&current_def).kind.clone() else {
        break;
      };
      let Some(entry) = self.namespaces.lookup_def(ns_def.namespace_id, &segment.name).cloned() else {
        break;
      };
      let Some(def_id) = entry.as_single() else {
        break;
      };

      self.set_import_item_def(&segment.span, def_id);
      current_def = *def_id;
    }
  }

  /// Resolve a qualified path to a definition ID
  fn resolve_qualified_path_to_def(
    &self,
    path: &ignis_ast::expressions::path::ASTPath,
  ) -> Option<ignis_type::definition::DefinitionId> {
    if path.segments.is_empty() {
      return None;
    }

    // Start with first segment in scope
    let first_segment = &path.segments[0];
    let mut current_def = self.scopes.lookup_def(&first_segment.name).cloned()?;

    // Walk through remaining segments
    for segment in path.segments.iter().skip(1) {
      match &self.defs.get(&current_def).kind {
        DefinitionKind::Namespace(ns_def) => {
          current_def = self
            .namespaces
            .lookup_def(ns_def.namespace_id, &segment.name)
            .and_then(|e| e.as_single())
            .cloned()?;
        },
        _ => return None,
      }
    }

    Some(current_def)
  }

  /// Track path segment spans for hover support.
  ///
  /// Walks through the path segments and links each segment's span to its
  /// resolved definition in `import_item_defs`, enabling hover info for
  /// individual path components (e.g., hovering on `String` in `String::toString`
  /// shows namespace info, hovering on `toString` shows function signature).
  fn track_path_segment_spans(
    &mut self,
    path: &ignis_ast::expressions::path::ASTPath,
  ) {
    if path.segments.is_empty() {
      return;
    }

    // Resolve first segment
    let first_segment = &path.segments[0];
    let Some(first_def) = self.scopes.lookup_def(&first_segment.name).cloned() else {
      return;
    };

    self.set_import_item_def(&first_segment.span, &first_def);

    let mut current_def = first_def;

    // Walk through remaining segments
    for segment in path.segments.iter().skip(1) {
      // Clone the definition kind to avoid borrow conflicts
      let def_kind = self.defs.get(&current_def).kind.clone();

      match def_kind {
        DefinitionKind::Namespace(ns_def) => {
          let entry = self.namespaces.lookup_def(ns_def.namespace_id, &segment.name).cloned();

          if let Some(entry) = entry {
            if let Some(def_id) = entry.as_single() {
              self.set_import_item_def(&segment.span, def_id);
              current_def = *def_id;
            } else {
              // Overload group - still link the span to the first candidate for hover
              if let SymbolEntry::Overload(candidates) = &entry
                && let Some(first) = candidates.first()
              {
                self.set_import_item_def(&segment.span, first);
              }
              return;
            }
          } else {
            return;
          }
        },
        DefinitionKind::Record(rd) => {
          // Handle Record::staticMethod or Record::staticField
          if let Some(entry) = rd.static_methods.get(&segment.name) {
            if let Some(def_id) = entry.as_single() {
              self.set_import_item_def(&segment.span, def_id);
            } else if let SymbolEntry::Overload(candidates) = entry
              && let Some(first) = candidates.first()
            {
              self.set_import_item_def(&segment.span, first);
            }
          } else if let Some(field_id) = rd.static_fields.get(&segment.name) {
            self.set_import_item_def(&segment.span, field_id);
          }
          return;
        },
        DefinitionKind::Enum(ed) => {
          // Handle Enum::Variant or Enum::staticMethod or Enum::staticField
          if let Some(entry) = ed.static_methods.get(&segment.name) {
            if let Some(def_id) = entry.as_single() {
              self.set_import_item_def(&segment.span, def_id);
            } else if let SymbolEntry::Overload(candidates) = entry
              && let Some(first) = candidates.first()
            {
              self.set_import_item_def(&segment.span, first);
            }
          } else if let Some(field_id) = ed.static_fields.get(&segment.name) {
            self.set_import_item_def(&segment.span, field_id);
          } else if let Some(tag) = ed.variants_by_name.get(&segment.name) {
            // Handle variant access (Enum::Variant)
            if let Some(variant) = ed.variants.get(*tag as usize) {
              self.set_import_item_def(&segment.span, &variant.def_id);
            }
          }
          return;
        },
        _ => return,
      }
    }
  }

  /// Mark prefix segments of a qualified path (e.g. `Io` in `Io::println`)
  /// as referenced so the unused-import lint doesn't false-positive on them.
  fn mark_path_prefix_referenced(
    &mut self,
    segments: &[ignis_ast::expressions::path::ASTPathSegment],
  ) {
    if segments.len() < 2 {
      return;
    }

    for segment in &segments[..segments.len() - 1] {
      if let Some(def_id) = self.scopes.lookup_def(&segment.name).cloned() {
        self.mark_referenced(def_id);
      }
    }
  }

  /// Auto-dereference references to get the underlying type
  fn auto_deref(
    &self,
    ty: TypeId,
  ) -> TypeId {
    match self.types.get(&ty) {
      Type::Reference { inner, .. } => *inner,
      _ => ty,
    }
  }

  // ========================================================================
  // Record Init Typechecking
  // ========================================================================

  fn typecheck_record_init(
    &mut self,
    ri: &ASTRecordInit,
    scope_kind: ScopeKind,
    ctx: &TypecheckContext,
    infer: &InferContext,
  ) -> TypeId {
    // Resolve the type path
    let def_id = self.resolve_record_path(&ri.path);

    let Some(def_id) = def_id else {
      let path_str = ri
        .path
        .iter()
        .map(|(sym, _)| self.get_symbol_name(sym))
        .collect::<Vec<_>>()
        .join("::");
      self.add_diagnostic(
        DiagnosticMessage::UndefinedType {
          name: path_str,
          span: ri.span.clone(),
        }
        .report(),
      );
      return self.types.error();
    };

    // Check it's a record
    let rd = match &self.defs.get(&def_id).kind {
      DefinitionKind::Record(rd) => rd.clone(),
      _ => {
        let type_name = self.get_symbol_name(&self.defs.get(&def_id).name);
        self.add_diagnostic(
          DiagnosticMessage::NotARecord {
            name: type_name,
            span: ri.span.clone(),
          }
          .report(),
        );
        return self.types.error();
      },
    };

    // Determine type args: from explicit syntax, infer context, or none for non-generic
    let type_args: Vec<TypeId> = if let Some(ref explicit_args) = ri.type_args {
      // Explicit type args on record init: Box<i32> { ... }
      explicit_args
        .iter()
        .map(|ts| self.resolve_type_syntax_with_span(ts, &ri.span))
        .collect()
    } else if !rd.type_params.is_empty() {
      // Generic record - try to infer from expected type
      if let Some(expected) = &infer.expected {
        match self.types.get(expected).clone() {
          Type::Instance { generic, args } if generic == def_id => args,
          _ => {
            // Expected type doesn't match - will be caught later
            vec![]
          },
        }
      } else {
        // No expected type and no explicit args - can't infer
        vec![]
      }
    } else {
      // Non-generic record
      vec![]
    };

    // Build substitution for generic records
    let subst = if !rd.type_params.is_empty() && type_args.len() == rd.type_params.len() {
      Substitution::for_generic(def_id, &type_args)
    } else {
      Substitution::new()
    };

    let mut initialized: HashSet<ignis_type::symbol::SymbolId> = HashSet::new();

    // Typecheck each field initializer
    for init_field in &ri.fields {
      // Find the field definition
      let field_def = rd.fields.iter().find(|f| f.name == init_field.name);

      let Some(field_def) = field_def else {
        let field_name = self.get_symbol_name(&init_field.name);
        let type_name = self.get_symbol_name(&self.defs.get(&def_id).name);
        self.add_diagnostic(
          DiagnosticMessage::UnknownField {
            field: field_name,
            type_name,
            span: init_field.span.clone(),
          }
          .report(),
        );
        continue;
      };

      self.set_import_item_def(&init_field.name_span, &field_def.def_id);

      // Check for duplicate
      if !initialized.insert(init_field.name) {
        let field_name = self.get_symbol_name(&init_field.name);
        self.add_diagnostic(
          DiagnosticMessage::DuplicateFieldInit {
            field: field_name,
            span: init_field.span.clone(),
          }
          .report(),
        );
        continue;
      }

      // Substitute type params in field type
      let field_type = self.types.substitute(field_def.type_id, &subst);

      // Typecheck the value
      let field_infer = InferContext::expecting(field_type);
      let value_type = self.typecheck_node_with_infer(&init_field.value, scope_kind, ctx, &field_infer);
      self.typecheck_assignment(&field_type, &value_type, &init_field.span);
    }

    // Check all required fields are initialized
    for field in &rd.fields {
      if !initialized.contains(&field.name) {
        let field_name = self.get_symbol_name(&field.name);
        let type_name = self.get_symbol_name(&self.defs.get(&def_id).name);
        self.add_diagnostic(
          DiagnosticMessage::MissingFieldInit {
            field: field_name,
            type_name,
            span: ri.span.clone(),
          }
          .report(),
        );
      }
    }

    // Return the concrete type (with type args) or generic type
    if !type_args.is_empty() {
      self.types.instance(def_id, type_args)
    } else {
      rd.type_id
    }
  }

  /// Resolve a path from record init to a definition
  fn resolve_record_path(
    &self,
    path: &[(ignis_type::symbol::SymbolId, Span)],
  ) -> Option<ignis_type::definition::DefinitionId> {
    if path.is_empty() {
      return None;
    }

    // Start with first segment in scope
    let (first_sym, _) = &path[0];
    let mut current_def = self.scopes.lookup_def(first_sym).cloned()?;

    // Walk through remaining segments
    for (segment_sym, _) in path.iter().skip(1) {
      match &self.defs.get(&current_def).kind {
        DefinitionKind::Namespace(ns_def) => {
          current_def = self
            .namespaces
            .lookup_def(ns_def.namespace_id, segment_sym)
            .and_then(|e| e.as_single())
            .cloned()?;
        },
        _ => return None,
      }
    }

    Some(current_def)
  }

  fn typecheck_literal(
    &mut self,
    lit: &ASTLiteral,
    infer: &InferContext,
  ) -> TypeId {
    match &lit.value {
      IgnisLiteralValue::Int8(v) => self.coerce_signed_literal(*v as i64, self.types.i8(), infer, &lit.span),
      IgnisLiteralValue::Int16(v) => self.coerce_signed_literal(*v as i64, self.types.i16(), infer, &lit.span),
      IgnisLiteralValue::Int32(v) => self.coerce_signed_literal(*v as i64, self.types.i32(), infer, &lit.span),
      IgnisLiteralValue::Int64(v) => self.coerce_signed_literal(*v, self.types.i64(), infer, &lit.span),

      IgnisLiteralValue::UnsignedInt8(v) => self.coerce_unsigned_literal(*v as u64, self.types.u8(), infer, &lit.span),
      IgnisLiteralValue::UnsignedInt16(v) => {
        self.coerce_unsigned_literal(*v as u64, self.types.u16(), infer, &lit.span)
      },
      IgnisLiteralValue::UnsignedInt32(v) => {
        self.coerce_unsigned_literal(*v as u64, self.types.u32(), infer, &lit.span)
      },
      IgnisLiteralValue::UnsignedInt64(v) => self.coerce_unsigned_literal(*v, self.types.u64(), infer, &lit.span),

      IgnisLiteralValue::Hex(s) => {
        let digits = s.trim_start_matches("0x").trim_start_matches("0X");
        let v = u64::from_str_radix(digits, 16).unwrap_or(0);
        self.coerce_unsigned_literal(v, self.types.u32(), infer, &lit.span)
      },
      IgnisLiteralValue::Binary(s) => {
        let digits = s.trim_start_matches("0b").trim_start_matches("0B");
        let v = u64::from_str_radix(digits, 2).unwrap_or(0);
        self.coerce_unsigned_literal(v, self.types.u8(), infer, &lit.span)
      },

      IgnisLiteralValue::Float32(_) => self.coerce_float_literal(self.types.f32(), infer),
      IgnisLiteralValue::Float64(_) => self.coerce_float_literal(self.types.f64(), infer),

      IgnisLiteralValue::Boolean(_) => self.types.boolean(),
      IgnisLiteralValue::Char(_) => self.types.char(),
      IgnisLiteralValue::String(_) => self.types.str(),
      IgnisLiteralValue::Atom(_) => self.types.atom(),
      IgnisLiteralValue::Null => {
        if let Some(expected) = &infer.expected {
          if self.is_pointer_type(expected) {
            return *expected;
          }

          self.add_diagnostic(DiagnosticMessage::InvalidNullLiteral { span: lit.span.clone() }.report());

          return self.types.error();
        }

        self.types.null_ptr()
      },
    }
  }

  fn coerce_signed_literal(
    &mut self,
    value: i64,
    default: TypeId,
    infer: &InferContext,
    span: &Span,
  ) -> TypeId {
    if let Some(expected) = &infer.expected
      && self.is_integer_type(expected)
    {
      if self.signed_fits_in_type(value, expected) {
        return *expected;
      } else {
        self.add_diagnostic(
          DiagnosticMessage::IntegerOverflow {
            value,
            target_type: self.format_type_for_error(expected),
            span: span.clone(),
          }
          .report(),
        );
        return self.types.error();
      }
    }
    default
  }

  fn coerce_unsigned_literal(
    &mut self,
    value: u64,
    default: TypeId,
    infer: &InferContext,
    span: &Span,
  ) -> TypeId {
    if let Some(expected) = &infer.expected
      && self.is_integer_type(expected)
    {
      if self.unsigned_fits_in_type(value, expected) {
        return *expected;
      } else {
        self.add_diagnostic(
          DiagnosticMessage::IntegerOverflow {
            value: value as i64,
            target_type: self.format_type_for_error(expected),
            span: span.clone(),
          }
          .report(),
        );
        return self.types.error();
      }
    }
    default
  }

  fn coerce_float_literal(
    &self,
    default: TypeId,
    infer: &InferContext,
  ) -> TypeId {
    if let Some(expected) = &infer.expected
      && self.is_float_type(expected)
    {
      return *expected;
    }
    default
  }

  fn is_integer_type(
    &self,
    ty: &TypeId,
  ) -> bool {
    matches!(
      self.types.get(ty),
      ignis_type::types::Type::I8
        | ignis_type::types::Type::I16
        | ignis_type::types::Type::I32
        | ignis_type::types::Type::I64
        | ignis_type::types::Type::U8
        | ignis_type::types::Type::U16
        | ignis_type::types::Type::U32
        | ignis_type::types::Type::U64
    )
  }

  fn is_float_type(
    &self,
    ty: &TypeId,
  ) -> bool {
    matches!(self.types.get(ty), ignis_type::types::Type::F32 | ignis_type::types::Type::F64)
  }

  fn signed_fits_in_type(
    &self,
    value: i64,
    ty: &TypeId,
  ) -> bool {
    match self.types.get(ty) {
      Type::I8 => value >= i8::MIN as i64 && value <= i8::MAX as i64,
      Type::I16 => value >= i16::MIN as i64 && value <= i16::MAX as i64,
      Type::I32 => value >= i32::MIN as i64 && value <= i32::MAX as i64,
      Type::I64 => true,
      Type::U8 => value >= 0 && value <= u8::MAX as i64,
      Type::U16 => value >= 0 && value <= u16::MAX as i64,
      Type::U32 => value >= 0 && value <= u32::MAX as i64,
      Type::U64 => value >= 0,
      _ => false,
    }
  }

  fn unsigned_fits_in_type(
    &self,
    value: u64,
    ty: &TypeId,
  ) -> bool {
    match self.types.get(ty) {
      Type::I8 => value <= i8::MAX as u64,
      Type::I16 => value <= i16::MAX as u64,
      Type::I32 => value <= i32::MAX as u64,
      Type::I64 => value <= i64::MAX as u64,
      Type::U8 => value <= u8::MAX as u64,
      Type::U16 => value <= u16::MAX as u64,
      Type::U32 => value <= u32::MAX as u64,
      Type::U64 => true,
      _ => false,
    }
  }

  fn is_ptr_coercion(
    &self,
    from: &TypeId,
    to: &TypeId,
  ) -> bool {
    matches!(
      (self.types.get(from), self.types.get(to)),
      (Type::Pointer { .. }, Type::Pointer { .. })
    )
  }

  fn typecheck_call(
    &mut self,
    node_id: &NodeId,
    call: &ASTCallExpression,
    scope_kind: ScopeKind,
    ctx: &TypecheckContext,
    infer: &InferContext,
  ) -> TypeId {
    // Check for builtins - they are handled specially
    if let ASTNode::Expression(ASTExpression::Variable(var)) = self.ast.get(&call.callee) {
      let name = self.get_symbol_name(&var.name);

      match name.as_str() {
        "typeOf" => return self.typecheck_typeof_builtin(call, scope_kind, ctx),
        "sizeOf" => return self.typecheck_sizeof_builtin(call, scope_kind, ctx),
        "alignOf" => return self.typecheck_alignof_builtin(call, scope_kind, ctx),
        "maxOf" => return self.typecheck_maxof_builtin(call, scope_kind, ctx),
        "minOf" => return self.typecheck_minof_builtin(call, scope_kind, ctx),
        _ => {},
      }
    }

    // Check for method calls: obj.method() or Type::method()
    if let ASTNode::Expression(ASTExpression::MemberAccess(ma)) = self.ast.get(&call.callee) {
      return self.typecheck_method_call(node_id, ma, call, scope_kind, ctx, infer);
    }

    // Check for path-based calls that might be static method or enum variant
    if let ASTNode::Expression(ASTExpression::Path(path)) = self.ast.get(&call.callee)
      && let Some(result) = self.typecheck_path_call(node_id, path, call, scope_kind, ctx, infer)
    {
      return result;
    }
    // Fall through to normal call handling if path doesn't resolve to record/enum

    // Get the callee entry from scope or resolve it if it's a path
    let callee_entry = match self.ast.get(&call.callee) {
      ASTNode::Expression(ASTExpression::Variable(var)) => self.scopes.lookup(&var.name).cloned(),
      ASTNode::Expression(ASTExpression::Path(path)) => {
        self.mark_path_prefix_referenced(&path.segments);
        match self.resolve_qualified_path(&path.segments) {
          Some(ResolvedPath::Entry(entry)) => Some(entry),
          _ => None,
        }
      },
      _ => None,
    };

    let mut arg_types_for_resolution: Option<Vec<TypeId>> = None;
    let callee_is_name = matches!(
      self.ast.get(&call.callee),
      ASTNode::Expression(ASTExpression::Variable(_)) | ASTNode::Expression(ASTExpression::Path(_))
    );
    let callee_unresolved = callee_is_name && callee_entry.is_none();

    let resolved_def_id = match callee_entry.as_ref() {
      Some(SymbolEntry::Overload(candidates)) if candidates.len() > 1 => {
        let arg_types: Vec<TypeId> = call
          .arguments
          .iter()
          .map(|arg| self.typecheck_node(arg, scope_kind, ctx))
          .collect();
        arg_types_for_resolution = Some(arg_types);

        match self.resolve_overload(candidates, arg_types_for_resolution.as_ref().unwrap(), &call.span, None) {
          Ok(def_id) => Some(def_id),
          Err(()) => return self.types.error(),
        }
      },
      Some(SymbolEntry::Overload(candidates)) => candidates.first().copied(),
      _ => callee_entry.as_ref().and_then(|e| e.as_single().cloned()),
    };

    if let Some(def_id) = resolved_def_id {
      self.set_resolved_call(node_id, def_id);
      // Also record for the callee so hover works on the function name
      self.set_resolved_call(&call.callee, def_id);
      self.mark_referenced(def_id);
    }

    // Check if this is a generic function (has type params)
    let is_generic_func = resolved_def_id
      .as_ref()
      .map(|def_id| {
        if let DefinitionKind::Function(func_def) = &self.defs.get(def_id).kind {
          !func_def.type_params.is_empty()
        } else {
          false
        }
      })
      .unwrap_or(false);

    // Build substitution for generic function calls with explicit type arguments
    let substitution = self.build_call_substitution(&resolved_def_id, call);

    // For generic functions without explicit type args, skip strict type checking
    // (type inference happens during lowering)
    let skip_type_checking = is_generic_func && substitution.is_none();

    // Get param/return types from resolved function definition
    let (param_types, ret_type, is_variadic, is_function_def) = if let Some(def_id) = &resolved_def_id {
      match &self.defs.get(def_id).kind {
        DefinitionKind::Function(func_def) => {
          let params: Vec<TypeId> = func_def.params.iter().map(|p| self.defs.type_of(p)).cloned().collect();
          let ret = func_def.return_type;
          if let Some(ref subst) = substitution {
            let subst_params: Vec<TypeId> = params.iter().map(|p| self.types.substitute(*p, subst)).collect();
            let subst_ret = self.types.substitute(ret, subst);
            (subst_params, subst_ret, func_def.is_variadic, true)
          } else {
            (params, ret, func_def.is_variadic, true)
          }
        },
        // Variables/parameters/constants of function type (closures) are callable.
        DefinitionKind::Variable(_) | DefinitionKind::Parameter(_) | DefinitionKind::Constant(_) => {
          let var_ty = *self.defs.type_of(def_id);
          if let Type::Function {
            params,
            ret,
            is_variadic,
          } = self.types.get(&var_ty).clone()
          {
            (params, ret, is_variadic, true)
          } else {
            (vec![], self.types.error(), false, false)
          }
        },
        _ => (vec![], self.types.error(), false, false),
      }
    } else {
      (vec![], self.types.error(), false, false)
    };

    if !is_function_def {
      for arg in &call.arguments {
        self.typecheck_node(arg, scope_kind, ctx);
      }

      if resolved_def_id.is_none()
        && let Some(ty) = self.lookup_type(&call.callee)
        && self.types.is_error(ty)
      {
        return self.types.error();
      }

      if callee_unresolved {
        return self.types.error();
      }

      let type_name = if let Some(def_id) = &resolved_def_id {
        self.format_type_for_error(self.defs.type_of(def_id))
      } else {
        "<error>".to_string()
      };
      self.add_diagnostic(
        DiagnosticMessage::NotCallable {
          type_name,
          span: call.span.clone(),
        }
        .report(),
      );
      return self.types.error();
    }

    let arg_types: Vec<TypeId> = if let Some(arg_types) = arg_types_for_resolution {
      arg_types
    } else if skip_type_checking {
      call
        .arguments
        .iter()
        .map(|arg| self.typecheck_node(arg, scope_kind, ctx))
        .collect()
    } else {
      call
        .arguments
        .iter()
        .enumerate()
        .map(|(i, arg)| {
          if let Some(param_type) = param_types.get(i) {
            let infer = InferContext::expecting(*param_type);
            self.typecheck_node_with_infer(arg, scope_kind, ctx, &infer)
          } else {
            self.typecheck_node(arg, scope_kind, ctx)
          }
        })
        .collect()
    };

    let func_name = match self.ast.get(&call.callee) {
      ASTNode::Expression(ASTExpression::Variable(var)) => self.get_symbol_name(&var.name),
      ASTNode::Expression(ASTExpression::Path(path)) => path
        .segments
        .iter()
        .map(|s| self.get_symbol_name(&s.name))
        .collect::<Vec<_>>()
        .join("::"),
      _ => "<function>".to_string(),
    };

    if is_variadic {
      if arg_types.len() < param_types.len() {
        self.add_diagnostic(
          DiagnosticMessage::ArgumentCountMismatch {
            expected: param_types.len(),
            got: arg_types.len(),
            func_name: func_name.clone(),
            span: call.span.clone(),
          }
          .report(),
        );
      }
    } else if arg_types.len() != param_types.len() {
      self.add_diagnostic(
        DiagnosticMessage::ArgumentCountMismatch {
          expected: param_types.len(),
          got: arg_types.len(),
          func_name: func_name.clone(),
          span: call.span.clone(),
        }
        .report(),
      );
    }

    // Skip argument type checking for generic functions without explicit type args
    // (type inference happens during lowering)
    if !skip_type_checking {
      let check_count = std::cmp::min(arg_types.len(), param_types.len());
      for i in 0..check_count {
        if self.types.is_error(&arg_types[i]) || self.types.is_error(&param_types[i]) {
          continue;
        }

        if self.types.is_infer_var(&arg_types[i]) {
          let span = self.node_span(&call.arguments[i]).clone();
          let _ = self.infer_ctx.unify(
            arg_types[i],
            param_types[i],
            &span,
            ConstraintReason::Argument { param_idx: i },
            &mut self.types,
          );
          continue;
        }

        if !self.types.is_assignable(&param_types[i], &arg_types[i]) {
          // Special case: deallocate accepts any *mut T, coercing to *mut u8
          if func_name == "deallocate" && i == 0 && self.is_ptr_coercion(&arg_types[i], &param_types[i]) {
            continue;
          }

          let expected = self.format_type_for_error(&param_types[i]);
          let got = self.format_type_for_error(&arg_types[i]);

          self.add_diagnostic(
            DiagnosticMessage::ArgumentTypeMismatch {
              param_idx: i + 1,
              expected,
              got,
              span: self.node_span(&call.arguments[i]).clone(),
            }
            .report(),
          );
        }
      }
    }

    ret_type
  }

  /// Typechecks `lhs |> rhs` and records how lowering should emit the call.
  fn typecheck_pipe(
    &mut self,
    node_id: &NodeId,
    lhs: &NodeId,
    rhs: &NodeId,
    pipe_span: &ignis_type::span::Span,
    _span: &ignis_type::span::Span,
    scope_kind: ScopeKind,
    ctx: &TypecheckContext,
  ) -> TypeId {
    let lhs_type = self.typecheck_node(lhs, scope_kind, ctx);

    let rhs_node = self.ast.get(rhs).clone();
    match rhs_node {
      ASTNode::Expression(ASTExpression::Call(ref call)) => {
        self.typecheck_pipe_call(node_id, lhs, &lhs_type, rhs, call, pipe_span, scope_kind, ctx)
      },

      ASTNode::Expression(ASTExpression::Variable(ref var)) => {
        self.typecheck_pipe_bare_callee(node_id, lhs, &lhs_type, rhs, &var.name, pipe_span, scope_kind, ctx)
      },

      ASTNode::Expression(ASTExpression::Path(ref path)) => {
        self.typecheck_pipe_path(node_id, lhs, &lhs_type, rhs, path, pipe_span, scope_kind, ctx)
      },

      ASTNode::Expression(ASTExpression::Lambda(_)) => {
        self.typecheck_pipe_lambda(node_id, lhs, &lhs_type, rhs, pipe_span, scope_kind, ctx)
      },

      ASTNode::Expression(ASTExpression::PipePlaceholder { ref span, .. }) => {
        self.add_diagnostic(
          DiagnosticMessage::PipePlaceholderNotInCall {
            span: span.clone(),
            pipe_span: pipe_span.clone(),
          }
          .report(),
        );
        self.types.error()
      },

      ASTNode::Expression(ASTExpression::MemberAccess(ref ma)) if ma.op == ignis_ast::expressions::ASTAccessOp::Dot => {
        self.add_diagnostic(
          DiagnosticMessage::PipeRhsInstanceMethod {
            rhs_span: ma.span.clone(),
            pipe_span: pipe_span.clone(),
          }
          .report(),
        );
        self.types.error()
      },

      _ => {
        let rhs_desc = self.describe_expression_kind(rhs);
        self.add_diagnostic(
          DiagnosticMessage::PipeRhsNotCallable {
            rhs_span: self.node_span(rhs).clone(),
            rhs_desc,
            pipe_span: pipe_span.clone(),
          }
          .report(),
        );
        self.types.error()
      },
    }
  }

  /// Handles `x |> f(a, b)` and `x |> f(a, _, b)`.
  fn typecheck_pipe_call(
    &mut self,
    node_id: &NodeId,
    lhs: &NodeId,
    lhs_type: &TypeId,
    _rhs: &NodeId,
    call: &ASTCallExpression,
    pipe_span: &ignis_type::span::Span,
    scope_kind: ScopeKind,
    ctx: &TypecheckContext,
  ) -> TypeId {
    // --- Placeholder scan ---

    let placeholder_positions: Vec<(usize, ignis_type::span::Span)> = call
      .arguments
      .iter()
      .enumerate()
      .filter_map(|(i, arg)| {
        if let ASTNode::Expression(ASTExpression::PipePlaceholder { span, .. }) = self.ast.get(arg) {
          Some((i, span.clone()))
        } else {
          None
        }
      })
      .collect();

    if placeholder_positions.len() > 1 {
      self.add_diagnostic(
        DiagnosticMessage::PipeMultiplePlaceholders {
          first: placeholder_positions[0].1.clone(),
          second: placeholder_positions[1].1.clone(),
          pipe_span: pipe_span.clone(),
        }
        .report(),
      );
      // Typecheck non-placeholder args to avoid cascading errors
      for arg in &call.arguments {
        if !matches!(self.ast.get(arg), ASTNode::Expression(ASTExpression::PipePlaceholder { .. })) {
          self.typecheck_node(arg, scope_kind, ctx);
        }
      }
      return self.types.error();
    }

    let insertion = if let Some(&(index, _)) = placeholder_positions.first() {
      PipeArgInsertion::ReplaceAt(index)
    } else {
      PipeArgInsertion::Prepend
    };

    // --- MemberAccess callee rejection ---

    let callee_node = self.ast.get(&call.callee).clone();

    if let ASTNode::Expression(ASTExpression::MemberAccess(ma)) = &callee_node {
      self.add_diagnostic(
        DiagnosticMessage::PipeRhsUnsupportedCallee {
          callee_span: ma.span.clone(),
          pipe_span: pipe_span.clone(),
        }
        .report(),
      );
      for arg in &call.arguments {
        if !matches!(self.ast.get(arg), ASTNode::Expression(ASTExpression::PipePlaceholder { .. })) {
          self.typecheck_node(arg, scope_kind, ctx);
        }
      }
      return self.types.error();
    }

    // --- Callee resolution ---

    let (callee_entry, callee_is_path) = match &callee_node {
      ASTNode::Expression(ASTExpression::Variable(var)) => (self.scopes.lookup(&var.name).cloned(), false),
      ASTNode::Expression(ASTExpression::Path(path)) => {
        self.mark_path_prefix_referenced(&path.segments);
        let entry = match self.resolve_qualified_path(&path.segments) {
          Some(ResolvedPath::Entry(entry)) => Some(entry),
          _ => None,
        };
        (entry, true)
      },
      _ => (None, false),
    };

    let mut extra_arg_types_for_resolution: Option<Vec<TypeId>> = None;

    let def_id = match callee_entry.as_ref() {
      Some(SymbolEntry::Overload(candidates)) if candidates.len() > 1 => {
        let extra_arg_types: Vec<TypeId> = call
          .arguments
          .iter()
          .enumerate()
          .map(|(i, arg)| {
            if matches!(insertion, PipeArgInsertion::ReplaceAt(idx) if idx == i) {
              *lhs_type
            } else {
              self.typecheck_node(arg, scope_kind, ctx)
            }
          })
          .collect();

        let arg_types = match insertion {
          PipeArgInsertion::Prepend => {
            let mut all = Vec::with_capacity(1 + extra_arg_types.len());
            all.push(*lhs_type);
            all.extend(extra_arg_types.iter().copied());
            all
          },
          PipeArgInsertion::ReplaceAt(_) => extra_arg_types.clone(),
        };

        extra_arg_types_for_resolution = Some(extra_arg_types);

        match self.resolve_overload(candidates, &arg_types, &call.span, None) {
          Ok(def_id) => Some(def_id),
          Err(()) => return self.types.error(),
        }
      },
      Some(SymbolEntry::Overload(candidates)) => candidates.first().copied(),
      _ => callee_entry.as_ref().and_then(|e| e.as_single().cloned()),
    };

    if let Some(def_id) = def_id {
      self.set_resolved_call(node_id, def_id);
      self.set_resolved_call(&call.callee, def_id);
      self.mark_referenced(def_id);
    }

    let substitution = self.build_call_substitution(&def_id, call);

    let (mut param_types, mut ret_type, is_closure) = if let Some(def_id) = &def_id {
      self.pipe_callee_signature(def_id)
    } else {
      (vec![], self.types.error(), false)
    };

    if let (Some(def_id), Some(subst)) = (def_id.as_ref(), substitution.as_ref())
      && matches!(
        &self.defs.get(def_id).kind,
        DefinitionKind::Function(_) | DefinitionKind::Method(_)
      )
    {
      param_types = param_types.iter().map(|ty| self.types.substitute(*ty, subst)).collect();
      ret_type = self.types.substitute(ret_type, subst);
    }

    if param_types.is_empty() && def_id.is_none() {
      for arg in &call.arguments {
        if !matches!(self.ast.get(arg), ASTNode::Expression(ASTExpression::PipePlaceholder { .. })) {
          self.typecheck_node(arg, scope_kind, ctx);
        }
      }
      return self.types.error();
    }

    // --- Arity and type checking (mode-dependent) ---

    match insertion {
      PipeArgInsertion::Prepend => {
        let extra_arg_types: Vec<TypeId> = if let Some(extra_arg_types) = extra_arg_types_for_resolution {
          extra_arg_types
        } else {
          call
            .arguments
            .iter()
            .enumerate()
            .map(|(i, arg)| {
              if let Some(param_type) = param_types.get(i + 1) {
                self.typecheck_node_with_infer(arg, scope_kind, ctx, &InferContext::expecting(*param_type))
              } else {
                self.typecheck_node(arg, scope_kind, ctx)
              }
            })
            .collect()
        };

        let total_args = 1 + extra_arg_types.len();
        if total_args != param_types.len() {
          let func_name = self.callee_name_for_diagnostics(&callee_node, callee_is_path);
          self.add_diagnostic(
            DiagnosticMessage::ArgumentCountMismatch {
              expected: param_types.len(),
              got: total_args,
              func_name,
              span: pipe_span.clone(),
            }
            .report(),
          );
        }

        if let Some(first_param) = param_types.first() {
          self.check_pipe_arg_type(first_param, lhs_type, lhs, pipe_span);
        }

        let check_count = std::cmp::min(extra_arg_types.len(), param_types.len().saturating_sub(1));
        for i in 0..check_count {
          let param_type = &param_types[i + 1];
          if !self.types.is_error(&extra_arg_types[i])
            && !self.types.is_error(param_type)
            && !self.types.is_assignable(param_type, &extra_arg_types[i])
          {
            self.add_diagnostic(
              DiagnosticMessage::ArgumentTypeMismatch {
                param_idx: i + 2,
                expected: self.format_type_for_error(param_type),
                got: self.format_type_for_error(&extra_arg_types[i]),
                span: self.node_span(&call.arguments[i]).clone(),
              }
              .report(),
            );
          }
        }
      },

      PipeArgInsertion::ReplaceAt(placeholder_index) => {
        let extra_arg_types: Vec<TypeId> = if let Some(extra_arg_types) = extra_arg_types_for_resolution {
          extra_arg_types
        } else {
          call
            .arguments
            .iter()
            .enumerate()
            .map(|(i, arg)| {
              if i == placeholder_index {
                // Placeholder position — LHS type goes here
                *lhs_type
              } else if let Some(param_type) = param_types.get(i) {
                self.typecheck_node_with_infer(arg, scope_kind, ctx, &InferContext::expecting(*param_type))
              } else {
                self.typecheck_node(arg, scope_kind, ctx)
              }
            })
            .collect()
        };

        let total_args = extra_arg_types.len();
        if total_args != param_types.len() {
          let func_name = self.callee_name_for_diagnostics(&callee_node, callee_is_path);
          self.add_diagnostic(
            DiagnosticMessage::ArgumentCountMismatch {
              expected: param_types.len(),
              got: total_args,
              func_name,
              span: pipe_span.clone(),
            }
            .report(),
          );
        }

        // Check LHS type against the placeholder's parameter slot
        if let Some(param_type) = param_types.get(placeholder_index) {
          self.check_pipe_arg_type(param_type, lhs_type, lhs, pipe_span);
        }

        // Check non-placeholder arguments
        let check_count = std::cmp::min(extra_arg_types.len(), param_types.len());
        for i in 0..check_count {
          if i == placeholder_index {
            continue;
          }
          let param_type = &param_types[i];
          if !self.types.is_error(&extra_arg_types[i])
            && !self.types.is_error(param_type)
            && !self.types.is_assignable(param_type, &extra_arg_types[i])
          {
            self.add_diagnostic(
              DiagnosticMessage::ArgumentTypeMismatch {
                param_idx: i + 1,
                expected: self.format_type_for_error(param_type),
                got: self.format_type_for_error(&extra_arg_types[i]),
                span: self.node_span(&call.arguments[i]).clone(),
              }
              .report(),
            );
          }
        }
      },
    }

    // --- Record resolution ---

    let type_args = self.resolve_pipe_type_args(&def_id, call);

    if is_closure {
      self.pipe_resolutions.insert(
        *node_id,
        PipeResolution::ClosureCall {
          callee_node: call.callee,
          extra_args: call.arguments.clone(),
          insertion,
        },
      );
    } else if let Some(def_id) = def_id {
      self.pipe_resolutions.insert(
        *node_id,
        PipeResolution::DirectCall {
          def_id,
          extra_args: call.arguments.clone(),
          type_args,
          insertion,
        },
      );
    }

    ret_type
  }

  /// Handles `x |> f`.
  fn typecheck_pipe_bare_callee(
    &mut self,
    node_id: &NodeId,
    lhs: &NodeId,
    lhs_type: &TypeId,
    rhs: &NodeId,
    name: &ignis_type::symbol::SymbolId,
    pipe_span: &ignis_type::span::Span,
    scope_kind: ScopeKind,
    _ctx: &TypecheckContext,
  ) -> TypeId {
    let entry = self.scopes.lookup(name).cloned();
    let def_id = match entry.as_ref() {
      Some(SymbolEntry::Overload(candidates)) if candidates.len() > 1 => {
        let arg_types = vec![*lhs_type];
        match self.resolve_overload(candidates, &arg_types, pipe_span, None) {
          Ok(def_id) => Some(def_id),
          Err(()) => return self.types.error(),
        }
      },
      Some(SymbolEntry::Overload(candidates)) => candidates.first().copied(),
      _ => entry.as_ref().and_then(|e| e.as_single().cloned()),
    };

    if def_id.is_none() {
      let infer = InferContext::none();
      self.typecheck_node_with_infer(rhs, scope_kind, _ctx, &infer);
      return self.types.error();
    }

    let def_id = def_id.unwrap();
    self.set_resolved_call(node_id, def_id);
    self.set_resolved_call(rhs, def_id);
    self.mark_referenced(def_id);

    let (param_types, ret_type, is_closure) = self.pipe_callee_signature(&def_id);

    if param_types.is_empty() {
      self.add_diagnostic(
        DiagnosticMessage::ArgumentCountMismatch {
          expected: 0,
          got: 1,
          func_name: self.get_symbol_name(name),
          span: pipe_span.clone(),
        }
        .report(),
      );
      return self.types.error();
    }

    if param_types.len() != 1 {
      self.add_diagnostic(
        DiagnosticMessage::ArgumentCountMismatch {
          expected: param_types.len(),
          got: 1,
          func_name: self.get_symbol_name(name),
          span: pipe_span.clone(),
        }
        .report(),
      );
    }

    self.check_pipe_arg_type(&param_types[0], lhs_type, lhs, pipe_span);

    if is_closure {
      self.pipe_resolutions.insert(
        *node_id,
        PipeResolution::ClosureCall {
          callee_node: *rhs,
          extra_args: vec![],
          insertion: PipeArgInsertion::Prepend,
        },
      );
    } else {
      self.pipe_resolutions.insert(
        *node_id,
        PipeResolution::DirectCall {
          def_id,
          extra_args: vec![],
          type_args: vec![],
          insertion: PipeArgInsertion::Prepend,
        },
      );
    }

    ret_type
  }

  /// Handles `x |> Mod::func`.
  fn typecheck_pipe_path(
    &mut self,
    node_id: &NodeId,
    lhs: &NodeId,
    lhs_type: &TypeId,
    rhs: &NodeId,
    path: &ignis_ast::expressions::path::ASTPath,
    pipe_span: &ignis_type::span::Span,
    _scope_kind: ScopeKind,
    _ctx: &TypecheckContext,
  ) -> TypeId {
    self.mark_path_prefix_referenced(&path.segments);

    let entry = match self.resolve_qualified_path(&path.segments) {
      Some(ResolvedPath::Entry(entry)) => Some(entry),
      _ => None,
    };

    let def_id = match entry.as_ref() {
      Some(SymbolEntry::Overload(candidates)) if candidates.len() > 1 => {
        let arg_types = vec![*lhs_type];
        match self.resolve_overload(candidates, &arg_types, pipe_span, None) {
          Ok(def_id) => Some(def_id),
          Err(()) => return self.types.error(),
        }
      },
      Some(SymbolEntry::Overload(candidates)) => candidates.first().copied(),
      _ => entry.as_ref().and_then(|e| e.as_single().cloned()),
    };

    if def_id.is_none() {
      self.typecheck_node(rhs, _scope_kind, _ctx);
      return self.types.error();
    }

    let def_id = def_id.unwrap();
    self.set_resolved_call(node_id, def_id);
    self.set_resolved_call(rhs, def_id);
    self.mark_referenced(def_id);

    let (param_types, ret_type, is_closure) = self.pipe_callee_signature(&def_id);

    if param_types.is_empty() {
      let path_name = path
        .segments
        .iter()
        .map(|s| self.get_symbol_name(&s.name))
        .collect::<Vec<_>>()
        .join("::");
      self.add_diagnostic(
        DiagnosticMessage::ArgumentCountMismatch {
          expected: 0,
          got: 1,
          func_name: path_name,
          span: pipe_span.clone(),
        }
        .report(),
      );
      return self.types.error();
    }

    if param_types.len() != 1 {
      let path_name = path
        .segments
        .iter()
        .map(|s| self.get_symbol_name(&s.name))
        .collect::<Vec<_>>()
        .join("::");
      self.add_diagnostic(
        DiagnosticMessage::ArgumentCountMismatch {
          expected: param_types.len(),
          got: 1,
          func_name: path_name,
          span: pipe_span.clone(),
        }
        .report(),
      );
    }

    self.check_pipe_arg_type(&param_types[0], lhs_type, lhs, pipe_span);

    if is_closure {
      self.pipe_resolutions.insert(
        *node_id,
        PipeResolution::ClosureCall {
          callee_node: *rhs,
          extra_args: vec![],
          insertion: PipeArgInsertion::Prepend,
        },
      );
    } else {
      self.pipe_resolutions.insert(
        *node_id,
        PipeResolution::DirectCall {
          def_id,
          extra_args: vec![],
          type_args: vec![],
          insertion: PipeArgInsertion::Prepend,
        },
      );
    }

    ret_type
  }

  /// Handles `x |> (..lambda..)`.
  fn typecheck_pipe_lambda(
    &mut self,
    node_id: &NodeId,
    lhs: &NodeId,
    lhs_type: &TypeId,
    rhs: &NodeId,
    pipe_span: &ignis_type::span::Span,
    scope_kind: ScopeKind,
    ctx: &TypecheckContext,
  ) -> TypeId {
    let rhs_type = self.typecheck_node(rhs, scope_kind, ctx);

    if self.types.is_error(&rhs_type) {
      return self.types.error();
    }

    let (param_types, ret_type) = match self.types.get(&rhs_type).clone() {
      Type::Function { params, ret, .. } => (params, ret),
      _ => {
        self.add_diagnostic(
          DiagnosticMessage::PipeRhsNotCallable {
            rhs_span: self.node_span(rhs).clone(),
            rhs_desc: "expression".to_string(),
            pipe_span: pipe_span.clone(),
          }
          .report(),
        );
        return self.types.error();
      },
    };

    if param_types.is_empty() {
      self.add_diagnostic(
        DiagnosticMessage::ArgumentCountMismatch {
          expected: 0,
          got: 1,
          func_name: "<lambda>".to_string(),
          span: pipe_span.clone(),
        }
        .report(),
      );
      return self.types.error();
    }

    if param_types.len() != 1 {
      self.add_diagnostic(
        DiagnosticMessage::ArgumentCountMismatch {
          expected: param_types.len(),
          got: 1,
          func_name: "<lambda>".to_string(),
          span: pipe_span.clone(),
        }
        .report(),
      );
    }

    self.check_pipe_arg_type(&param_types[0], lhs_type, lhs, pipe_span);

    self.pipe_resolutions.insert(
      *node_id,
      PipeResolution::ClosureCall {
        callee_node: *rhs,
        extra_args: vec![],
        insertion: PipeArgInsertion::Prepend,
      },
    );

    ret_type
  }

  /// Returns `(params, return_type, is_closure_call)` for a callable definition.
  fn pipe_callee_signature(
    &self,
    def_id: &DefinitionId,
  ) -> (Vec<TypeId>, TypeId, bool) {
    match &self.defs.get(def_id).kind {
      DefinitionKind::Function(func_def) => {
        let params: Vec<TypeId> = func_def.params.iter().map(|p| self.defs.type_of(p)).cloned().collect();
        (params, func_def.return_type, false)
      },
      DefinitionKind::Method(method_def) => {
        let start = self.method_param_start(method_def);
        let params: Vec<TypeId> = method_def.params[start..]
          .iter()
          .map(|p| self.defs.type_of(p))
          .cloned()
          .collect();
        (params, method_def.return_type, false)
      },
      DefinitionKind::Variable(_) | DefinitionKind::Parameter(_) | DefinitionKind::Constant(_) => {
        let var_ty = *self.defs.type_of(def_id);
        if let Type::Function { params, ret, .. } = self.types.get(&var_ty).clone() {
          (params, ret, true)
        } else {
          (vec![], self.types.error(), false)
        }
      },
      _ => (vec![], self.types.error(), false),
    }
  }

  /// Checks that the piped value matches the first parameter type.
  fn check_pipe_arg_type(
    &mut self,
    param_type: &TypeId,
    arg_type: &TypeId,
    arg_node: &NodeId,
    _pipe_span: &ignis_type::span::Span,
  ) {
    if self.types.is_error(arg_type) || self.types.is_error(param_type) {
      return;
    }

    if !self.types.is_assignable(param_type, arg_type) {
      self.add_diagnostic(
        DiagnosticMessage::ArgumentTypeMismatch {
          param_idx: 1,
          expected: self.format_type_for_error(param_type),
          got: self.format_type_for_error(arg_type),
          span: self.node_span(arg_node).clone(),
        }
        .report(),
      );
    }
  }

  /// Resolves explicit type arguments for a pipe call.
  fn resolve_pipe_type_args(
    &mut self,
    def_id: &Option<DefinitionId>,
    call: &ASTCallExpression,
  ) -> Vec<TypeId> {
    let Some(def_id) = def_id else {
      return vec![];
    };
    let Some(ref type_args_syntax) = call.type_args else {
      return vec![];
    };

    let type_params = match &self.defs.get(def_id).kind {
      DefinitionKind::Function(func_def) => {
        if func_def.type_params.is_empty() {
          return vec![];
        }
        func_def.type_params.clone()
      },
      DefinitionKind::Method(method_def) => {
        if method_def.type_params.is_empty() {
          return vec![];
        }
        method_def.type_params.clone()
      },
      _ => {
        return vec![];
      },
    };

    if type_args_syntax.len() != type_params.len() {
      return vec![];
    }

    type_args_syntax
      .iter()
      .map(|syntax| self.resolve_type_syntax(syntax))
      .collect()
  }

  fn describe_expression_kind(
    &self,
    node_id: &NodeId,
  ) -> String {
    match self.ast.get(node_id) {
      ASTNode::Expression(ASTExpression::Literal(_)) => "literal".to_string(),
      ASTNode::Expression(ASTExpression::Binary(_)) => "binary expression".to_string(),
      ASTNode::Expression(ASTExpression::Unary(_)) => "unary expression".to_string(),
      ASTNode::Expression(ASTExpression::Ternary(_)) => "ternary expression".to_string(),
      ASTNode::Expression(ASTExpression::Assignment(_)) => "assignment".to_string(),
      ASTNode::Expression(ASTExpression::RecordInit(_)) => "record initialization".to_string(),
      ASTNode::Expression(ASTExpression::Vector(_)) => "vector literal".to_string(),
      _ => "expression".to_string(),
    }
  }

  fn callee_name_for_diagnostics(
    &self,
    callee_node: &ASTNode,
    is_path: bool,
  ) -> String {
    match callee_node {
      ASTNode::Expression(ASTExpression::Variable(var)) => self.get_symbol_name(&var.name),
      ASTNode::Expression(ASTExpression::Path(path)) if is_path => path
        .segments
        .iter()
        .map(|s| self.get_symbol_name(&s.name))
        .collect::<Vec<_>>()
        .join("::"),
      _ => "<function>".to_string(),
    }
  }

  /// Build a type substitution for a generic function call.
  ///
  /// If the function has type parameters and explicit type arguments are provided,
  /// creates a Substitution mapping each type param to its concrete type.
  /// Returns None if the function is not generic or no type args are provided.
  fn build_call_substitution(
    &mut self,
    func_def_id: &Option<DefinitionId>,
    call: &ASTCallExpression,
  ) -> Option<Substitution> {
    let func_def_id = (*func_def_id)?;
    let type_args_syntax = call.type_args.as_ref()?;

    // Get the function's type parameters
    let type_params = if let DefinitionKind::Function(func_def) = &self.defs.get(&func_def_id).kind {
      if func_def.type_params.is_empty() {
        return None;
      }
      func_def.type_params.clone()
    } else {
      return None;
    };

    // Resolve the explicit type arguments
    let resolved_type_args: Vec<TypeId> = type_args_syntax
      .iter()
      .map(|ty| self.resolve_type_syntax_with_span(ty, &call.span))
      .collect();

    // Check that the number of type arguments matches the number of type parameters
    if resolved_type_args.len() != type_params.len() {
      self.add_diagnostic(Diagnostic {
        severity: Severity::Error,
        message: format!(
          "Expected {} type argument(s), got {}",
          type_params.len(),
          resolved_type_args.len()
        ),
        error_code: "A0050".to_string(),
        primary_span: call.span.clone(),
        labels: vec![],
        notes: vec![],
      });
      return None;
    }

    // Build the substitution: map each type param's (owner, index) to the resolved type arg
    Some(Substitution::for_generic(func_def_id, &resolved_type_args))
  }

  /// Instantiate a callee's parameter and return types by substituting explicit type arguments.
  ///
  /// Returns `Some((param_types, return_type, substitution))` if the call has explicit type args
  /// and they match the callee's type parameters. Returns `None` if no type args provided or
  /// if there's an error (arity mismatch, type args on non-generic callee).
  fn instantiate_callee_signature(
    &mut self,
    def_id: &DefinitionId,
    call: &ASTCallExpression,
  ) -> Option<(Vec<TypeId>, TypeId, Substitution)> {
    let type_args_syntax = call.type_args.as_ref()?;

    // Get type params and signature from the definition
    let (type_params, raw_param_ids, return_type, owner_type_params) = match &self.defs.get(def_id).kind {
      DefinitionKind::Function(func_def) => (
        func_def.type_params.clone(),
        func_def.params.clone(),
        func_def.return_type,
        vec![],
      ),
      DefinitionKind::Method(method_def) => {
        // Get owner's type params for methods on generic records/enums
        let owner_type_params = match &self.defs.get(&method_def.owner_type).kind {
          DefinitionKind::Record(rd) => rd.type_params.clone(),
          DefinitionKind::Enum(ed) => ed.type_params.clone(),
          _ => vec![],
        };
        (
          method_def.type_params.clone(),
          method_def.params.clone(),
          method_def.return_type,
          owner_type_params,
        )
      },
      _ => return None,
    };

    // If method has no type params but owner does, the type args are for the owner
    let (effective_type_params, subst_owner) = if type_params.is_empty() && !owner_type_params.is_empty() {
      // Type args are for the owner (e.g., Vector::init<i32>() -> type args for Vector)
      let owner_def_id = match &self.defs.get(def_id).kind {
        DefinitionKind::Method(md) => md.owner_type,
        _ => return None,
      };
      (owner_type_params, Some(owner_def_id))
    } else if !type_params.is_empty() {
      // Type args are for the method itself
      (type_params, None)
    } else {
      // Neither generic - this is an error
      self.add_diagnostic(Diagnostic {
        severity: Severity::Error,
        message: "Type arguments provided but callee is not generic".to_string(),
        error_code: "A0077".to_string(),
        primary_span: call.span.clone(),
        labels: vec![],
        notes: vec![],
      });
      return None;
    };

    // Resolve the explicit type arguments
    let resolved_type_args: Vec<TypeId> = type_args_syntax
      .iter()
      .map(|ty| self.resolve_type_syntax_with_span(ty, &call.span))
      .collect();

    // Check arity
    if resolved_type_args.len() != effective_type_params.len() {
      self.add_diagnostic(Diagnostic {
        severity: Severity::Error,
        message: format!(
          "Expected {} type argument(s), got {}",
          effective_type_params.len(),
          resolved_type_args.len()
        ),
        error_code: "A0050".to_string(),
        primary_span: call.span.clone(),
        labels: vec![],
        notes: vec![],
      });
      return None;
    }

    // Build substitution - use owner's def_id if type args are for owner
    let subst_def_id = subst_owner.unwrap_or(*def_id);
    let subst = Substitution::for_generic(subst_def_id, &resolved_type_args);

    // Get raw param types and substitute
    let param_types: Vec<TypeId> = raw_param_ids
      .iter()
      .map(|p| {
        let raw_type = self.get_definition_type(p);
        self.types.substitute(raw_type, &subst)
      })
      .collect();

    // Substitute return type
    let subst_return = self.types.substitute(return_type, &subst);

    Some((param_types, subst_return, subst))
  }

  /// Build a combined `Substitution` for a method call on a `Type::Instance`.
  ///
  /// Binds the owner's type params (from the instance args) and, when the
  /// method has its own type params with explicit call-site type args, also
  /// binds those.
  fn build_instance_method_subst(
    &mut self,
    owner_def: DefinitionId,
    owner_args: &[TypeId],
    method_id: DefinitionId,
    method: &ignis_type::definition::MethodDefinition,
    call: &ASTCallExpression,
  ) -> Substitution {
    let owner_subst = if !owner_args.is_empty() {
      Substitution::for_generic(owner_def, owner_args)
    } else {
      Substitution::new()
    };

    if method.type_params.is_empty() {
      return owner_subst;
    }

    // Method has its own type params — resolve explicit type args if provided
    let Some(type_args_syntax) = call.type_args.as_ref() else {
      // No explicit type args — inference will be attempted later
      return owner_subst;
    };

    let method_type_args: Vec<TypeId> = type_args_syntax
      .iter()
      .map(|ty| self.resolve_type_syntax_with_span(ty, &call.span))
      .collect();

    if method_type_args.len() != method.type_params.len() {
      self.add_diagnostic(Diagnostic {
        severity: Severity::Error,
        message: format!(
          "Expected {} type argument(s) for method, got {}",
          method.type_params.len(),
          method_type_args.len()
        ),
        error_code: "A0050".to_string(),
        primary_span: call.span.clone(),
        labels: vec![],
        notes: vec![],
      });
      return owner_subst;
    }

    Substitution::for_method(owner_def, owner_args, method_id, &method_type_args)
  }

  /// Infer method-level type params by matching actual argument types against
  /// the method's parameter types (already partially substituted with the
  /// owner's type params).
  ///
  /// For example, if param type is `(T) -> U` (with T already resolved to i32
  /// via owner subst, giving `(i32) -> U`) and the actual arg type is
  /// `fn(i32) -> i64`, we can infer U = i64.
  fn infer_method_type_params_from_args(
    &mut self,
    owner_def: DefinitionId,
    owner_args: &[TypeId],
    method_id: DefinitionId,
    explicit_params: &[DefinitionId],
    arg_types: &[TypeId],
  ) -> Substitution {
    let mut inferred = Substitution::for_generic(owner_def, owner_args);

    for (i, param_def) in explicit_params.iter().enumerate() {
      let Some(arg_type) = arg_types.get(i) else { continue };
      if self.types.is_error(arg_type) {
        continue;
      }

      let raw_param_type = self.get_definition_type(param_def);
      // Substitute owner params first so we only have method params left as Param
      let partial = self.types.substitute(raw_param_type, &inferred);
      self.collect_type_param_bindings(method_id, partial, *arg_type, &mut inferred);
    }

    inferred
  }

  /// Recursively match an expected type (which may contain `Type::Param` holes
  /// owned by `target_def`) against an actual type to discover bindings.
  fn collect_type_param_bindings(
    &self,
    target_def: DefinitionId,
    expected: TypeId,
    actual: TypeId,
    subst: &mut Substitution,
  ) {
    let expected_ty = self.types.get(&expected).clone();
    let actual_ty = self.types.get(&actual).clone();

    match (&expected_ty, &actual_ty) {
      (Type::Param { owner, index }, _) if *owner == target_def => {
        subst.bind(target_def, *index, actual);
      },
      (
        Type::Function {
          params: ep, ret: er, ..
        },
        Type::Function {
          params: ap, ret: ar, ..
        },
      ) => {
        for (e, a) in ep.iter().zip(ap.iter()) {
          self.collect_type_param_bindings(target_def, *e, *a, subst);
        }
        self.collect_type_param_bindings(target_def, *er, *ar, subst);
      },
      (Type::Reference { inner: ei, .. }, Type::Reference { inner: ai, .. })
      | (Type::Pointer { inner: ei, .. }, Type::Pointer { inner: ai, .. }) => {
        self.collect_type_param_bindings(target_def, *ei, *ai, subst);
      },
      (Type::Instance { generic: eg, args: ea }, Type::Instance { generic: ag, args: aa }) if eg == ag => {
        for (e, a) in ea.iter().zip(aa.iter()) {
          self.collect_type_param_bindings(target_def, *e, *a, subst);
        }
      },
      _ => {},
    }
  }

  /// Typecheck a method call: obj.method(args) or Type::method(args)
  fn typecheck_method_call(
    &mut self,
    node_id: &NodeId,
    ma: &ASTMemberAccess,
    call: &ASTCallExpression,
    scope_kind: ScopeKind,
    ctx: &TypecheckContext,
    infer: &InferContext,
  ) -> TypeId {
    match ma.op {
      ASTAccessOp::Dot => self.typecheck_instance_method_call(node_id, ma, call, scope_kind, ctx),
      ASTAccessOp::DoubleColon => self.typecheck_static_method_call(node_id, ma, call, scope_kind, ctx, infer),
    }
  }

  /// Typecheck an instance method call: obj.method(args)
  fn typecheck_instance_method_call(
    &mut self,
    node_id: &NodeId,
    ma: &ASTMemberAccess,
    call: &ASTCallExpression,
    scope_kind: ScopeKind,
    ctx: &TypecheckContext,
  ) -> TypeId {
    let obj_type = self.typecheck_node(&ma.object, scope_kind, ctx);
    let obj_type = self.auto_deref(obj_type);

    if self.types.is_error(&obj_type) {
      for arg in &call.arguments {
        self.typecheck_node(arg, scope_kind, ctx);
      }
      return self.types.error();
    }

    match self.types.get(&obj_type).clone() {
      Type::Record(def_id) => {
        let rd = if let DefinitionKind::Record(rd) = &self.defs.get(&def_id).kind {
          rd.clone()
        } else {
          return self.types.error();
        };

        // Check instance methods
        if let Some(entry) = rd.instance_methods.get(&ma.member) {
          match entry {
            SymbolEntry::Single(method_id) => {
              self.set_import_item_def(&ma.member_span, method_id);
              let method = {
                let method_def = self.defs.get(method_id);
                let DefinitionKind::Method(method) = &method_def.kind else {
                  return self.types.error();
                };
                method.clone()
              };

              // Check if method requires &mut self but receiver is not mutable
              if method.self_mutable {
                let obj_node = self.ast.get(&ma.object);
                if let ASTNode::Expression(obj_expr) = obj_node
                  && !self.is_mutable_expression(obj_expr)
                {
                  let method_name = self.get_symbol_name(&ma.member);
                  let var_name = self.get_var_name_from_expr(obj_expr);
                  self.add_diagnostic(
                    DiagnosticMessage::MutatingMethodOnImmutable {
                      method: method_name,
                      var_name,
                      span: ma.span.clone(),
                    }
                    .report(),
                  );
                }
              }

              // For instance methods, skip the first param (self) when checking explicit args
              // The receiver is passed implicitly, not as an explicit argument
              let start = self.method_param_start(&method);
              let explicit_params: Vec<_> = method.params[start..].to_vec();

              // Get param types for inference, applying substitution if explicit type args provided
              let (param_types, return_type) =
                if let Some((subst_params, subst_ret, _)) = self.instantiate_callee_signature(method_id, call) {
                  // Skip self param from substituted params
                  (subst_params[start..].to_vec(), subst_ret)
                } else {
                  let raw_params: Vec<TypeId> = explicit_params.iter().map(|p| self.get_definition_type(p)).collect();
                  (raw_params, method.return_type)
                };

              // Typecheck arguments
              let arg_types: Vec<TypeId> = call
                .arguments
                .iter()
                .enumerate()
                .map(|(i, arg)| {
                  if let Some(param_type) = param_types.get(i) {
                    let infer = InferContext::expecting(*param_type);
                    self.typecheck_node_with_infer(arg, scope_kind, ctx, &infer)
                  } else {
                    self.typecheck_node(arg, scope_kind, ctx)
                  }
                })
                .collect();

              // Check argument count (excluding self for instance methods)
              let method_name = self.get_symbol_name(&ma.member);
              if arg_types.len() != explicit_params.len() {
                self.add_diagnostic(
                  DiagnosticMessage::ArgumentCountMismatch {
                    expected: explicit_params.len(),
                    got: arg_types.len(),
                    func_name: method_name.clone(),
                    span: call.span.clone(),
                  }
                  .report(),
                );
              }

              // Check argument types
              let check_count = std::cmp::min(arg_types.len(), param_types.len());
              for i in 0..check_count {
                if self.types.is_error(&arg_types[i]) || self.types.is_error(&param_types[i]) {
                  continue;
                }
                if !self.types.is_assignable(&param_types[i], &arg_types[i]) {
                  let expected = self.format_type_for_error(&param_types[i]);
                  let got = self.format_type_for_error(&arg_types[i]);
                  self.add_diagnostic(
                    DiagnosticMessage::ArgumentTypeMismatch {
                      param_idx: i + 1,
                      expected,
                      got,
                      span: self.node_span(&call.arguments[i]).clone(),
                    }
                    .report(),
                  );
                }
              }

              return return_type;
            },
            SymbolEntry::Overload(candidates) => {
              let arg_types: Vec<TypeId> = call
                .arguments
                .iter()
                .map(|arg| self.typecheck_node(arg, scope_kind, ctx))
                .collect();

              let resolved_def_id = match self.resolve_overload(candidates, &arg_types, &call.span, None) {
                Ok(def_id) => def_id,
                Err(()) => return self.types.error(),
              };

              self.set_resolved_call(node_id, resolved_def_id);
              self.set_resolved_call(&call.callee, resolved_def_id);
              self.mark_referenced(resolved_def_id);

              let method = {
                let method_def = self.defs.get(&resolved_def_id);
                let DefinitionKind::Method(method) = &method_def.kind else {
                  return self.types.error();
                };
                method.clone()
              };

              let start = self.method_param_start(&method);
              let explicit_params: Vec<_> = method.params[start..].to_vec();

              // Get param types, applying substitution if explicit type args provided
              let (param_types, return_type) =
                if let Some((subst_params, subst_ret, _)) = self.instantiate_callee_signature(&resolved_def_id, call) {
                  (subst_params[start..].to_vec(), subst_ret)
                } else {
                  let raw_params: Vec<TypeId> = explicit_params.iter().map(|p| self.get_definition_type(p)).collect();
                  (raw_params, method.return_type)
                };

              let method_name = self.get_symbol_name(&ma.member);
              if arg_types.len() != explicit_params.len() {
                self.add_diagnostic(
                  DiagnosticMessage::ArgumentCountMismatch {
                    expected: explicit_params.len(),
                    got: arg_types.len(),
                    func_name: method_name.clone(),
                    span: call.span.clone(),
                  }
                  .report(),
                );
              }

              let check_count = std::cmp::min(arg_types.len(), param_types.len());
              for i in 0..check_count {
                if self.types.is_error(&arg_types[i]) || self.types.is_error(&param_types[i]) {
                  continue;
                }
                if !self.types.is_assignable(&param_types[i], &arg_types[i]) {
                  let expected = self.format_type_for_error(&param_types[i]);
                  let got = self.format_type_for_error(&arg_types[i]);
                  self.add_diagnostic(
                    DiagnosticMessage::ArgumentTypeMismatch {
                      param_idx: i + 1,
                      expected,
                      got,
                      span: self.node_span(&call.arguments[i]).clone(),
                    }
                    .report(),
                  );
                }
              }

              return return_type;
            },
          }
        }

        // Not a method - check if it's a field being called (error)
        if rd.fields.iter().any(|f| f.name == ma.member) {
          let type_name = self.format_type_for_error(&obj_type);
          let member_name = self.get_symbol_name(&ma.member);
          self.add_diagnostic(
            DiagnosticMessage::NotCallable {
              type_name: format!("{}.{}", type_name, member_name),
              span: call.span.clone(),
            }
            .report(),
          );
          return self.types.error();
        }

        if let Some(result) = self.try_resolve_extension_method(node_id, &obj_type, ma, call, scope_kind, ctx) {
          return result;
        }

        // Method not found
        let type_name = self.format_type_for_error(&obj_type);
        let member_name = self.get_symbol_name(&ma.member);
        self.add_diagnostic(
          DiagnosticMessage::FieldNotFound {
            field: member_name,
            type_name,
            span: ma.span.clone(),
          }
          .report(),
        );
        self.types.error()
      },
      Type::Instance { generic, args } => {
        // Instance of a generic record - look up method and substitute types
        let rd = if let DefinitionKind::Record(rd) = &self.defs.get(&generic).kind {
          rd.clone()
        } else {
          return self.types.error();
        };

        // Check instance methods
        if let Some(entry) = rd.instance_methods.get(&ma.member) {
          match entry {
            SymbolEntry::Single(method_id) => {
              self.set_import_item_def(&ma.member_span, method_id);
              let method = {
                let method_def = self.defs.get(method_id);
                let DefinitionKind::Method(method) = &method_def.kind else {
                  return self.types.error();
                };
                method.clone()
              };

              // Check if method requires &mut self but receiver is not mutable
              if method.self_mutable {
                let obj_node = self.ast.get(&ma.object);
                if let ASTNode::Expression(obj_expr) = obj_node
                  && !self.is_mutable_expression(obj_expr)
                {
                  let method_name = self.get_symbol_name(&ma.member);
                  let var_name = self.get_var_name_from_expr(obj_expr);
                  self.add_diagnostic(
                    DiagnosticMessage::MutatingMethodOnImmutable {
                      method: method_name,
                      var_name,
                      span: ma.span.clone(),
                    }
                    .report(),
                  );
                }
              }

              // For instance methods, skip the first param (self) when checking explicit args
              let start = self.method_param_start(&method);
              let explicit_params: Vec<_> = method.params[start..].to_vec();

              // Build combined substitution: owner type params + method type params.
              // The owner's params come from the Instance type args; the method's own
              // params come from call-site type args (explicit or inferred).
              let combined_subst = self.build_instance_method_subst(generic, &args, *method_id, &method, call);

              // Get param types and substitute both owner and method type params
              let param_types: Vec<TypeId> = explicit_params
                .iter()
                .map(|p| {
                  let raw_type = self.get_definition_type(p);
                  self.types.substitute(raw_type, &combined_subst)
                })
                .collect();

              // Typecheck arguments
              let arg_types: Vec<TypeId> = call
                .arguments
                .iter()
                .enumerate()
                .map(|(i, arg)| {
                  if let Some(param_type) = param_types.get(i) {
                    let infer = InferContext::expecting(*param_type);
                    self.typecheck_node_with_infer(arg, scope_kind, ctx, &infer)
                  } else {
                    self.typecheck_node(arg, scope_kind, ctx)
                  }
                })
                .collect();

              // If method has type params but no explicit type args, try to infer
              // from argument types (e.g., f: (T) -> U with arg fn(i32) -> i64 → U = i64).
              let combined_subst = if !method.type_params.is_empty() && call.type_args.is_none() {
                self.infer_method_type_params_from_args(generic, &args, *method_id, &explicit_params, &arg_types)
              } else {
                combined_subst
              };

              // Re-substitute param types with potentially-inferred substitution
              let param_types: Vec<TypeId> = if !method.type_params.is_empty() && call.type_args.is_none() {
                explicit_params
                  .iter()
                  .map(|p| {
                    let raw_type = self.get_definition_type(p);
                    self.types.substitute(raw_type, &combined_subst)
                  })
                  .collect()
              } else {
                param_types
              };

              // Check argument count
              let method_name = self.get_symbol_name(&ma.member);
              if arg_types.len() != explicit_params.len() {
                self.add_diagnostic(
                  DiagnosticMessage::ArgumentCountMismatch {
                    expected: explicit_params.len(),
                    got: arg_types.len(),
                    func_name: method_name.clone(),
                    span: call.span.clone(),
                  }
                  .report(),
                );
              }

              // Check argument types
              let check_count = std::cmp::min(arg_types.len(), param_types.len());
              for i in 0..check_count {
                if self.types.is_error(&arg_types[i]) || self.types.is_error(&param_types[i]) {
                  continue;
                }
                if !self.types.is_assignable(&param_types[i], &arg_types[i]) {
                  let expected = self.format_type_for_error(&param_types[i]);
                  let got = self.format_type_for_error(&arg_types[i]);
                  self.add_diagnostic(
                    DiagnosticMessage::ArgumentTypeMismatch {
                      param_idx: i + 1,
                      expected,
                      got,
                      span: self.node_span(&call.arguments[i]).clone(),
                    }
                    .report(),
                  );
                }
              }

              // Substitute return type with combined substitution
              return self.types.substitute(method.return_type, &combined_subst);
            },
            SymbolEntry::Overload(candidates) => {
              let arg_types: Vec<TypeId> = call
                .arguments
                .iter()
                .map(|arg| self.typecheck_node(arg, scope_kind, ctx))
                .collect();

              let resolved_def_id = match self.resolve_overload(candidates, &arg_types, &call.span, None) {
                Ok(def_id) => def_id,
                Err(()) => return self.types.error(),
              };

              self.set_resolved_call(node_id, resolved_def_id);
              self.set_resolved_call(&call.callee, resolved_def_id);
              self.mark_referenced(resolved_def_id);
              self.set_import_item_def(&ma.member_span, &resolved_def_id);

              let method = {
                let method_def = self.defs.get(&resolved_def_id);
                let DefinitionKind::Method(method) = &method_def.kind else {
                  return self.types.error();
                };
                method.clone()
              };

              let start = self.method_param_start(&method);
              let explicit_params: Vec<_> = method.params[start..].to_vec();

              // Build combined substitution for overloaded method on generic instance
              let combined_subst = self.build_instance_method_subst(generic, &args, resolved_def_id, &method, call);

              let param_types: Vec<TypeId> = explicit_params
                .iter()
                .map(|p| {
                  let raw_type = self.get_definition_type(p);
                  self.types.substitute(raw_type, &combined_subst)
                })
                .collect();

              let method_name = self.get_symbol_name(&ma.member);
              if arg_types.len() != explicit_params.len() {
                self.add_diagnostic(
                  DiagnosticMessage::ArgumentCountMismatch {
                    expected: explicit_params.len(),
                    got: arg_types.len(),
                    func_name: method_name.clone(),
                    span: call.span.clone(),
                  }
                  .report(),
                );
              }

              let check_count = std::cmp::min(arg_types.len(), param_types.len());
              for i in 0..check_count {
                if self.types.is_error(&arg_types[i]) || self.types.is_error(&param_types[i]) {
                  continue;
                }
                if !self.types.is_assignable(&param_types[i], &arg_types[i]) {
                  let expected = self.format_type_for_error(&param_types[i]);
                  let got = self.format_type_for_error(&arg_types[i]);
                  self.add_diagnostic(
                    DiagnosticMessage::ArgumentTypeMismatch {
                      param_idx: i + 1,
                      expected,
                      got,
                      span: self.node_span(&call.arguments[i]).clone(),
                    }
                    .report(),
                  );
                }
              }

              return self.types.substitute(method.return_type, &combined_subst);
            },
          }
        }

        // Not a method - check if it's a field being called (error)
        if rd.fields.iter().any(|f| f.name == ma.member) {
          let type_name = self.format_type_for_error(&obj_type);
          let member_name = self.get_symbol_name(&ma.member);
          self.add_diagnostic(
            DiagnosticMessage::NotCallable {
              type_name: format!("{}.{}", type_name, member_name),
              span: call.span.clone(),
            }
            .report(),
          );
          return self.types.error();
        }

        if let Some(result) = self.try_resolve_extension_method(node_id, &obj_type, ma, call, scope_kind, ctx) {
          return result;
        }

        // Method not found
        let type_name = self.format_type_for_error(&obj_type);
        let member_name = self.get_symbol_name(&ma.member);
        self.add_diagnostic(
          DiagnosticMessage::FieldNotFound {
            field: member_name,
            type_name,
            span: ma.span.clone(),
          }
          .report(),
        );
        self.types.error()
      },
      Type::Enum(def_id) => {
        let ed = if let DefinitionKind::Enum(ed) = &self.defs.get(&def_id).kind {
          ed.clone()
        } else {
          return self.types.error();
        };

        if let Some(entry) = ed.instance_methods.get(&ma.member) {
          match entry {
            SymbolEntry::Single(method_id) => {
              self.set_import_item_def(&ma.member_span, method_id);
              let method = {
                let method_def = self.defs.get(method_id);
                let DefinitionKind::Method(method) = &method_def.kind else {
                  return self.types.error();
                };
                method.clone()
              };

              if method.self_mutable {
                let obj_node = self.ast.get(&ma.object);
                if let ASTNode::Expression(obj_expr) = obj_node
                  && !self.is_mutable_expression(obj_expr)
                {
                  let method_name = self.get_symbol_name(&ma.member);
                  let var_name = self.get_var_name_from_expr(obj_expr);
                  self.add_diagnostic(
                    DiagnosticMessage::MutatingMethodOnImmutable {
                      method: method_name,
                      var_name,
                      span: ma.span.clone(),
                    }
                    .report(),
                  );
                }
              }

              let start = self.method_param_start(&method);
              let explicit_params: Vec<_> = method.params[start..].to_vec();

              let (param_types, return_type) =
                if let Some((subst_params, subst_ret, _)) = self.instantiate_callee_signature(method_id, call) {
                  (subst_params[start..].to_vec(), subst_ret)
                } else {
                  let raw_params: Vec<TypeId> = explicit_params.iter().map(|p| self.get_definition_type(p)).collect();
                  (raw_params, method.return_type)
                };

              let arg_types: Vec<TypeId> = call
                .arguments
                .iter()
                .enumerate()
                .map(|(i, arg)| {
                  if let Some(param_type) = param_types.get(i) {
                    let infer = InferContext::expecting(*param_type);
                    self.typecheck_node_with_infer(arg, scope_kind, ctx, &infer)
                  } else {
                    self.typecheck_node(arg, scope_kind, ctx)
                  }
                })
                .collect();

              let method_name = self.get_symbol_name(&ma.member);
              if arg_types.len() != explicit_params.len() {
                self.add_diagnostic(
                  DiagnosticMessage::ArgumentCountMismatch {
                    expected: explicit_params.len(),
                    got: arg_types.len(),
                    func_name: method_name.clone(),
                    span: call.span.clone(),
                  }
                  .report(),
                );
              }

              let check_count = std::cmp::min(arg_types.len(), param_types.len());
              for i in 0..check_count {
                if self.types.is_error(&arg_types[i]) || self.types.is_error(&param_types[i]) {
                  continue;
                }
                if !self.types.is_assignable(&param_types[i], &arg_types[i]) {
                  let expected = self.format_type_for_error(&param_types[i]);
                  let got = self.format_type_for_error(&arg_types[i]);
                  self.add_diagnostic(
                    DiagnosticMessage::ArgumentTypeMismatch {
                      param_idx: i + 1,
                      expected,
                      got,
                      span: self.node_span(&call.arguments[i]).clone(),
                    }
                    .report(),
                  );
                }
              }

              return return_type;
            },
            SymbolEntry::Overload(candidates) => {
              let arg_types: Vec<TypeId> = call
                .arguments
                .iter()
                .map(|arg| self.typecheck_node(arg, scope_kind, ctx))
                .collect();

              let resolved_def_id = match self.resolve_overload(candidates, &arg_types, &call.span, None) {
                Ok(def_id) => def_id,
                Err(()) => return self.types.error(),
              };

              self.set_resolved_call(node_id, resolved_def_id);
              self.set_resolved_call(&call.callee, resolved_def_id);
              self.mark_referenced(resolved_def_id);

              let method = {
                let method_def = self.defs.get(&resolved_def_id);
                let DefinitionKind::Method(method) = &method_def.kind else {
                  return self.types.error();
                };
                method.clone()
              };

              let start = self.method_param_start(&method);
              let explicit_params: Vec<_> = method.params[start..].to_vec();

              let (param_types, return_type) =
                if let Some((subst_params, subst_ret, _)) = self.instantiate_callee_signature(&resolved_def_id, call) {
                  (subst_params[start..].to_vec(), subst_ret)
                } else {
                  let raw_params: Vec<TypeId> = explicit_params.iter().map(|p| self.get_definition_type(p)).collect();
                  (raw_params, method.return_type)
                };

              let method_name = self.get_symbol_name(&ma.member);
              if arg_types.len() != explicit_params.len() {
                self.add_diagnostic(
                  DiagnosticMessage::ArgumentCountMismatch {
                    expected: explicit_params.len(),
                    got: arg_types.len(),
                    func_name: method_name.clone(),
                    span: call.span.clone(),
                  }
                  .report(),
                );
              }

              let check_count = std::cmp::min(arg_types.len(), param_types.len());
              for i in 0..check_count {
                if self.types.is_error(&arg_types[i]) || self.types.is_error(&param_types[i]) {
                  continue;
                }
                if !self.types.is_assignable(&param_types[i], &arg_types[i]) {
                  let expected = self.format_type_for_error(&param_types[i]);
                  let got = self.format_type_for_error(&arg_types[i]);
                  self.add_diagnostic(
                    DiagnosticMessage::ArgumentTypeMismatch {
                      param_idx: i + 1,
                      expected,
                      got,
                      span: self.node_span(&call.arguments[i]).clone(),
                    }
                    .report(),
                  );
                }
              }

              return return_type;
            },
          }
        }

        if let Some(result) = self.try_resolve_extension_method(node_id, &obj_type, ma, call, scope_kind, ctx) {
          return result;
        }

        let type_name = self.format_type_for_error(&obj_type);
        let member_name = self.get_symbol_name(&ma.member);
        self.add_diagnostic(
          DiagnosticMessage::FieldNotFound {
            field: member_name,
            type_name,
            span: ma.span.clone(),
          }
          .report(),
        );
        self.types.error()
      },
      _ => {
        if let Some(result) = self.try_resolve_extension_method(node_id, &obj_type, ma, call, scope_kind, ctx) {
          return result;
        }

        let type_name = self.format_type_for_error(&obj_type);
        self.add_diagnostic(
          DiagnosticMessage::DotAccessOnNonRecord {
            type_name,
            span: ma.span.clone(),
          }
          .report(),
        );
        self.types.error()
      },
    }
  }

  /// Typecheck a static method call: Type::method(args)
  fn typecheck_static_method_call(
    &mut self,
    node_id: &NodeId,
    ma: &ASTMemberAccess,
    call: &ASTCallExpression,
    scope_kind: ScopeKind,
    ctx: &TypecheckContext,
    infer: &InferContext,
  ) -> TypeId {
    let def_id = self.resolve_type_expression(&ma.object, scope_kind, ctx);

    let Some(def_id) = def_id else {
      self.add_diagnostic(DiagnosticMessage::StaticAccessOnNonType { span: ma.span.clone() }.report());
      return self.types.error();
    };

    self.register_type_expression_span(&ma.object, &def_id);

    match &self.defs.get(&def_id).kind.clone() {
      DefinitionKind::Record(rd) => {
        // Static method call
        if let Some(entry) = rd.static_methods.get(&ma.member) {
          match entry {
            SymbolEntry::Single(method_id) => {
              return self.typecheck_static_method_or_function_call(method_id, call, scope_kind, ctx);
            },
            SymbolEntry::Overload(candidates) => {
              let arg_types: Vec<TypeId> = call
                .arguments
                .iter()
                .map(|arg| self.typecheck_node(arg, scope_kind, ctx))
                .collect();

              let resolved_def_id = match self.resolve_overload(candidates, &arg_types, &call.span, None) {
                Ok(def_id) => def_id,
                Err(()) => return self.types.error(),
              };

              self.set_resolved_call(node_id, resolved_def_id);
              self.set_resolved_call(&call.callee, resolved_def_id);
              self.mark_referenced(resolved_def_id);

              let method = {
                let method_def = self.defs.get(&resolved_def_id);
                let DefinitionKind::Method(method) = &method_def.kind else {
                  return self.types.error();
                };
                method.clone()
              };

              let start = self.method_param_start(&method);
              let explicit_params: Vec<_> = method.params[start..].to_vec();

              // Get param types, applying substitution if explicit type args provided
              let (param_types, return_type) =
                if let Some((subst_params, subst_ret, _)) = self.instantiate_callee_signature(&resolved_def_id, call) {
                  (subst_params[start..].to_vec(), subst_ret)
                } else {
                  let raw_params: Vec<TypeId> = explicit_params.iter().map(|p| self.get_definition_type(p)).collect();
                  (raw_params, method.return_type)
                };

              let method_name = self.get_symbol_name(&ma.member);
              if arg_types.len() != explicit_params.len() {
                self.add_diagnostic(
                  DiagnosticMessage::ArgumentCountMismatch {
                    expected: explicit_params.len(),
                    got: arg_types.len(),
                    func_name: method_name.clone(),
                    span: call.span.clone(),
                  }
                  .report(),
                );
              }

              let check_count = std::cmp::min(arg_types.len(), param_types.len());
              for i in 0..check_count {
                if self.types.is_error(&arg_types[i]) || self.types.is_error(&param_types[i]) {
                  continue;
                }
                if !self.types.is_assignable(&param_types[i], &arg_types[i]) {
                  let expected = self.format_type_for_error(&param_types[i]);
                  let got = self.format_type_for_error(&arg_types[i]);
                  self.add_diagnostic(
                    DiagnosticMessage::ArgumentTypeMismatch {
                      param_idx: i + 1,
                      expected,
                      got,
                      span: self.node_span(&call.arguments[i]).clone(),
                    }
                    .report(),
                  );
                }
              }

              return return_type;
            },
          }
        }

        let type_name = self.get_symbol_name(&self.defs.get(&def_id).name);
        let member_name = self.get_symbol_name(&ma.member);
        self.add_diagnostic(
          DiagnosticMessage::StaticMemberNotFound {
            member: member_name,
            type_name,
            span: ma.span.clone(),
          }
          .report(),
        );
        self.types.error()
      },
      DefinitionKind::Enum(ed) => {
        // Enum variant with payload
        if let Some(&tag) = ed.variants_by_name.get(&ma.member) {
          let variant = &ed.variants[tag as usize];

          if variant.payload.is_empty() {
            // Unit variant being called - error
            let variant_name = self.get_symbol_name(&ma.member);
            self.add_diagnostic(
              DiagnosticMessage::NotCallable {
                type_name: variant_name,
                span: call.span.clone(),
              }
              .report(),
            );
            return self.types.error();
          }

          // Infer type args for generic enums from expected type
          let type_args: Vec<TypeId> = if !ed.type_params.is_empty() {
            if let Some(expected) = &infer.expected {
              match self.types.get(expected).clone() {
                Type::Instance { generic, args } if generic == def_id => args,
                _ => vec![],
              }
            } else {
              vec![]
            }
          } else {
            vec![]
          };

          // Build substitution for generic enums
          let subst = if !ed.type_params.is_empty() && type_args.len() == ed.type_params.len() {
            Substitution::for_generic(def_id, &type_args)
          } else {
            Substitution::new()
          };

          // Typecheck payload arguments with substituted types
          let arg_types: Vec<TypeId> = call
            .arguments
            .iter()
            .enumerate()
            .map(|(i, arg)| {
              if let Some(&payload_type) = variant.payload.get(i) {
                let substituted_type = self.types.substitute(payload_type, &subst);
                let infer = InferContext::expecting(substituted_type);
                self.typecheck_node_with_infer(arg, scope_kind, ctx, &infer)
              } else {
                self.typecheck_node(arg, scope_kind, ctx)
              }
            })
            .collect();

          // Check argument count
          let variant_name = self.get_symbol_name(&ma.member);
          if arg_types.len() != variant.payload.len() {
            self.add_diagnostic(
              DiagnosticMessage::EnumVariantRequiresPayload {
                variant: variant_name.clone(),
                expected: variant.payload.len(),
                span: call.span.clone(),
              }
              .report(),
            );
          }

          // Check argument types against substituted payload types
          let check_count = std::cmp::min(arg_types.len(), variant.payload.len());
          for (i, arg_type) in arg_types.iter().enumerate().take(check_count) {
            if self.types.is_error(arg_type) {
              continue;
            }

            let expected_type = self.types.substitute(variant.payload[i], &subst);
            if !self.types.is_assignable(&expected_type, arg_type) {
              let expected = self.format_type_for_error(&expected_type);
              let got = self.format_type_for_error(arg_type);
              self.add_diagnostic(
                DiagnosticMessage::ArgumentTypeMismatch {
                  param_idx: i + 1,
                  expected,
                  got,
                  span: self.node_span(&call.arguments[i]).clone(),
                }
                .report(),
              );
            }
          }

          // Return Type::Instance for generic enums, otherwise the base type
          if !type_args.is_empty() {
            return self.types.instance(def_id, type_args);
          }
          return ed.type_id;
        }

        // Static method call on enum
        if let Some(entry) = ed.static_methods.get(&ma.member) {
          match entry {
            SymbolEntry::Single(method_id) => {
              return self.typecheck_static_method_or_function_call(method_id, call, scope_kind, ctx);
            },
            SymbolEntry::Overload(candidates) => {
              let arg_types: Vec<TypeId> = call
                .arguments
                .iter()
                .map(|arg| self.typecheck_node(arg, scope_kind, ctx))
                .collect();

              let resolved_def_id = match self.resolve_overload(candidates, &arg_types, &call.span, None) {
                Ok(def_id) => def_id,
                Err(()) => return self.types.error(),
              };

              self.set_resolved_call(node_id, resolved_def_id);
              self.set_resolved_call(&call.callee, resolved_def_id);
              self.mark_referenced(resolved_def_id);

              let method = {
                let method_def = self.defs.get(&resolved_def_id);
                let DefinitionKind::Method(method) = &method_def.kind else {
                  return self.types.error();
                };
                method.clone()
              };

              let start = self.method_param_start(&method);
              let explicit_params: Vec<_> = method.params[start..].to_vec();

              // Get param types, applying substitution if explicit type args provided
              let (param_types, return_type) =
                if let Some((subst_params, subst_ret, _)) = self.instantiate_callee_signature(&resolved_def_id, call) {
                  (subst_params[start..].to_vec(), subst_ret)
                } else {
                  let raw_params: Vec<TypeId> = explicit_params.iter().map(|p| self.get_definition_type(p)).collect();
                  (raw_params, method.return_type)
                };

              let method_name = self.get_symbol_name(&ma.member);
              if arg_types.len() != explicit_params.len() {
                self.add_diagnostic(
                  DiagnosticMessage::ArgumentCountMismatch {
                    expected: explicit_params.len(),
                    got: arg_types.len(),
                    func_name: method_name.clone(),
                    span: call.span.clone(),
                  }
                  .report(),
                );
              }

              let check_count = std::cmp::min(arg_types.len(), param_types.len());
              for i in 0..check_count {
                if self.types.is_error(&arg_types[i]) || self.types.is_error(&param_types[i]) {
                  continue;
                }
                if !self.types.is_assignable(&param_types[i], &arg_types[i]) {
                  let expected = self.format_type_for_error(&param_types[i]);
                  let got = self.format_type_for_error(&arg_types[i]);
                  self.add_diagnostic(
                    DiagnosticMessage::ArgumentTypeMismatch {
                      param_idx: i + 1,
                      expected,
                      got,
                      span: self.node_span(&call.arguments[i]).clone(),
                    }
                    .report(),
                  );
                }
              }

              return return_type;
            },
          }
        }

        let type_name = self.get_symbol_name(&self.defs.get(&def_id).name);
        let member_name = self.get_symbol_name(&ma.member);
        self.add_diagnostic(
          DiagnosticMessage::StaticMemberNotFound {
            member: member_name,
            type_name,
            span: ma.span.clone(),
          }
          .report(),
        );
        self.types.error()
      },
      _ => {
        self.add_diagnostic(DiagnosticMessage::StaticAccessOnNonType { span: ma.span.clone() }.report());
        self.types.error()
      },
    }
  }

  /// Typecheck a call where the callee is a path (e.g., Type::method or Enum::Variant)
  fn typecheck_path_call(
    &mut self,
    node_id: &NodeId,
    path: &ignis_ast::expressions::path::ASTPath,
    call: &ASTCallExpression,
    scope_kind: ScopeKind,
    ctx: &TypecheckContext,
    infer: &InferContext,
  ) -> Option<TypeId> {
    if path.segments.len() < 2 {
      return None;
    }

    self.mark_path_prefix_referenced(&path.segments);

    match self.resolve_qualified_path(&path.segments) {
      Some(ResolvedPath::Entry(entry)) => match entry {
        SymbolEntry::Single(def_id) => {
          self.set_resolved_call(node_id, def_id);
          self.set_resolved_call(&call.callee, def_id);
          self.mark_referenced(def_id);
          Some(self.typecheck_static_method_or_function_call(&def_id, call, scope_kind, ctx))
        },
        SymbolEntry::Overload(candidates) => {
          if candidates.len() == 1 {
            let def_id = candidates[0];
            self.set_resolved_call(node_id, def_id);
            self.set_resolved_call(&call.callee, def_id);
            self.mark_referenced(def_id);
            return Some(self.typecheck_static_method_or_function_call(&def_id, call, scope_kind, ctx));
          }

          let arg_types: Vec<TypeId> = call
            .arguments
            .iter()
            .map(|arg| self.typecheck_node(arg, scope_kind, ctx))
            .collect();

          let resolved_def_id = match self.resolve_overload(&candidates, &arg_types, &call.span, None) {
            Ok(def_id) => def_id,
            Err(()) => return Some(self.types.error()),
          };

          self.set_resolved_call(node_id, resolved_def_id);
          self.set_resolved_call(&call.callee, resolved_def_id);
          self.mark_referenced(resolved_def_id);

          let def = self.defs.get(&resolved_def_id);
          let (params, raw_return_type, is_variadic) = match &def.kind {
            DefinitionKind::Method(m) => (m.params.clone(), m.return_type, false),
            DefinitionKind::Function(f) => (f.params.clone(), f.return_type, f.is_variadic),
            _ => return Some(self.types.error()),
          };
          let func_name = self.get_symbol_name(&def.name);

          // Get param types, applying substitution if explicit type args provided
          let (param_types, return_type) =
            if let Some((subst_params, subst_ret, _)) = self.instantiate_callee_signature(&resolved_def_id, call) {
              (subst_params, subst_ret)
            } else {
              let raw_params: Vec<TypeId> = params.iter().map(|p| self.get_definition_type(p)).collect();
              (raw_params, raw_return_type)
            };

          if is_variadic {
            if arg_types.len() < params.len() {
              self.add_diagnostic(
                DiagnosticMessage::ArgumentCountMismatch {
                  expected: params.len(),
                  got: arg_types.len(),
                  func_name: func_name.clone(),
                  span: call.span.clone(),
                }
                .report(),
              );
            }
          } else if arg_types.len() != params.len() {
            self.add_diagnostic(
              DiagnosticMessage::ArgumentCountMismatch {
                expected: params.len(),
                got: arg_types.len(),
                func_name: func_name.clone(),
                span: call.span.clone(),
              }
              .report(),
            );
          }

          let check_count = std::cmp::min(arg_types.len(), params.len());
          for i in 0..check_count {
            if self.types.is_error(&arg_types[i]) || self.types.is_error(&param_types[i]) {
              continue;
            }

            if !self.types.is_assignable(&param_types[i], &arg_types[i]) {
              let expected = self.format_type_for_error(&param_types[i]);
              let got = self.format_type_for_error(&arg_types[i]);
              self.add_diagnostic(
                DiagnosticMessage::ArgumentTypeMismatch {
                  param_idx: i + 1,
                  expected,
                  got,
                  span: self.node_span(&call.arguments[i]).clone(),
                }
                .report(),
              );
            }
          }

          Some(return_type)
        },
      },
      Some(ResolvedPath::EnumVariant {
        enum_def,
        variant_index,
      }) => {
        // Handle Enum variant call (with payload)
        let ed = if let DefinitionKind::Enum(ed) = &self.defs.get(&enum_def).kind {
          ed.clone()
        } else {
          return Some(self.types.error());
        };

        let variant = &ed.variants[variant_index as usize];

        if variant.payload.is_empty() {
          // Unit variant being called - error
          let variant_name = self.get_symbol_name(&variant.name);
          self.add_diagnostic(
            DiagnosticMessage::NotCallable {
              type_name: variant_name,
              span: call.span.clone(),
            }
            .report(),
          );
          return Some(self.types.error());
        }

        // Infer type args for generic enums from expected type
        let type_args: Vec<TypeId> = if !ed.type_params.is_empty() {
          if let Some(expected) = &infer.expected {
            match self.types.get(expected).clone() {
              Type::Instance { generic, args } if generic == enum_def => args,
              _ => vec![],
            }
          } else {
            vec![]
          }
        } else {
          vec![]
        };

        // Build substitution for generic enums
        let subst = if !ed.type_params.is_empty() && type_args.len() == ed.type_params.len() {
          Substitution::for_generic(enum_def, &type_args)
        } else {
          Substitution::new()
        };

        // Typecheck payload arguments with substituted types
        let arg_types: Vec<TypeId> = call
          .arguments
          .iter()
          .enumerate()
          .map(|(i, arg)| {
            if let Some(&payload_type) = variant.payload.get(i) {
              let substituted_type = self.types.substitute(payload_type, &subst);
              let infer = InferContext::expecting(substituted_type);
              self.typecheck_node_with_infer(arg, scope_kind, ctx, &infer)
            } else {
              self.typecheck_node(arg, scope_kind, ctx)
            }
          })
          .collect();

        // Check argument count
        let variant_name = self.get_symbol_name(&variant.name);
        if arg_types.len() != variant.payload.len() {
          self.add_diagnostic(
            DiagnosticMessage::EnumVariantRequiresPayload {
              variant: variant_name.clone(),
              expected: variant.payload.len(),
              span: call.span.clone(),
            }
            .report(),
          );
        }

        // Check argument types against substituted payload types
        let check_count = std::cmp::min(arg_types.len(), variant.payload.len());
        for (i, arg_type) in arg_types.iter().enumerate().take(check_count) {
          if self.types.is_error(arg_type) {
            continue;
          }

          let expected_type = self.types.substitute(variant.payload[i], &subst);
          if !self.types.is_assignable(&expected_type, arg_type) {
            let expected = self.format_type_for_error(&expected_type);
            let got = self.format_type_for_error(arg_type);
            self.add_diagnostic(
              DiagnosticMessage::ArgumentTypeMismatch {
                param_idx: i + 1,
                expected,
                got,
                span: self.node_span(&call.arguments[i]).clone(),
              }
              .report(),
            );
          }
        }

        // Return Type::Instance for generic enums, otherwise the base type
        if !type_args.is_empty() {
          return Some(self.types.instance(enum_def, type_args));
        }
        Some(ed.type_id)
      },
      _ => None,
    }
  }

  /// Helper to typecheck a static method or function call given its definition ID
  fn typecheck_static_method_or_function_call(
    &mut self,
    def_id: &ignis_type::definition::DefinitionId,
    call: &ASTCallExpression,
    scope_kind: ScopeKind,
    ctx: &TypecheckContext,
  ) -> TypeId {
    let def = self.defs.get(def_id);
    let (params, raw_return_type, is_variadic, type_params) = match &def.kind {
      DefinitionKind::Method(m) => (m.params.clone(), m.return_type, false, m.type_params.clone()),
      DefinitionKind::Function(f) => (f.params.clone(), f.return_type, f.is_variadic, f.type_params.clone()),
      _ => return self.types.error(),
    };
    let func_name = self.get_symbol_name(&def.name);

    // Get raw param types for initial inference context
    let raw_params: Vec<TypeId> = params.iter().map(|p| self.get_definition_type(p)).collect();

    // Try to instantiate with explicit type arguments first
    let (param_types, return_type) =
      if let Some((subst_params, subst_ret, _)) = self.instantiate_callee_signature(def_id, call) {
        // Explicit type args provided - use substituted types
        (subst_params, subst_ret)
      } else if !type_params.is_empty() && call.type_args.is_none() {
        // Generic callee with no explicit type args - infer from arguments
        // First typecheck arguments without inference hints to get their types
        let arg_types: Vec<TypeId> = call
          .arguments
          .iter()
          .map(|arg| self.typecheck_node(arg, scope_kind, ctx))
          .collect();

        // Infer type arguments by unifying param types with arg types
        let mut inferred = Substitution::new();
        for (i, &arg_ty) in arg_types.iter().enumerate() {
          if let Some(&param_ty) = raw_params.get(i) {
            self.types.unify_for_inference(param_ty, arg_ty, &mut inferred);
          }
        }

        // Build substituted param types using inferred substitution
        let subst_params: Vec<TypeId> = raw_params
          .iter()
          .map(|&ty| self.types.substitute(ty, &inferred))
          .collect();
        let subst_return = self.types.substitute(raw_return_type, &inferred);

        // Now do the argument count and type checking
        if is_variadic {
          if arg_types.len() < params.len() {
            self.add_diagnostic(
              DiagnosticMessage::ArgumentCountMismatch {
                expected: params.len(),
                got: arg_types.len(),
                func_name: func_name.clone(),
                span: call.span.clone(),
              }
              .report(),
            );
          }
        } else if arg_types.len() != params.len() {
          self.add_diagnostic(
            DiagnosticMessage::ArgumentCountMismatch {
              expected: params.len(),
              got: arg_types.len(),
              func_name: func_name.clone(),
              span: call.span.clone(),
            }
            .report(),
          );
        }

        let check_count = std::cmp::min(arg_types.len(), params.len());
        for i in 0..check_count {
          if self.types.is_error(&arg_types[i]) || self.types.is_error(&subst_params[i]) {
            continue;
          }

          if !self.types.is_assignable(&subst_params[i], &arg_types[i]) {
            let expected = self.format_type_for_error(&subst_params[i]);
            let got = self.format_type_for_error(&arg_types[i]);
            self.add_diagnostic(
              DiagnosticMessage::ArgumentTypeMismatch {
                param_idx: i + 1,
                expected,
                got,
                span: self.node_span(&call.arguments[i]).clone(),
              }
              .report(),
            );
          }
        }

        return subst_return;
      } else {
        // Non-generic or error case - use raw types
        (raw_params.clone(), raw_return_type)
      };

    // Typecheck arguments (for non-inferred case)
    let arg_types: Vec<TypeId> = call
      .arguments
      .iter()
      .enumerate()
      .map(|(i, arg)| {
        if let Some(param_type) = param_types.get(i) {
          let infer = InferContext::expecting(*param_type);
          self.typecheck_node_with_infer(arg, scope_kind, ctx, &infer)
        } else {
          self.typecheck_node(arg, scope_kind, ctx)
        }
      })
      .collect();

    // Check argument count
    if is_variadic {
      if arg_types.len() < params.len() {
        self.add_diagnostic(
          DiagnosticMessage::ArgumentCountMismatch {
            expected: params.len(),
            got: arg_types.len(),
            func_name: func_name.clone(),
            span: call.span.clone(),
          }
          .report(),
        );
      }
    } else if arg_types.len() != params.len() {
      self.add_diagnostic(
        DiagnosticMessage::ArgumentCountMismatch {
          expected: params.len(),
          got: arg_types.len(),
          func_name: func_name.clone(),
          span: call.span.clone(),
        }
        .report(),
      );
    }

    // Check argument types
    let check_count = std::cmp::min(arg_types.len(), params.len());
    for i in 0..check_count {
      if self.types.is_error(&arg_types[i]) || self.types.is_error(&param_types[i]) {
        continue;
      }

      if !self.types.is_assignable(&param_types[i], &arg_types[i]) {
        let expected = self.format_type_for_error(&param_types[i]);
        let got = self.format_type_for_error(&arg_types[i]);
        self.add_diagnostic(
          DiagnosticMessage::ArgumentTypeMismatch {
            param_idx: i + 1,
            expected,
            got,
            span: self.node_span(&call.arguments[i]).clone(),
          }
          .report(),
        );
      }
    }

    return_type
  }

  fn typecheck_typeof_builtin(
    &mut self,
    call: &ASTCallExpression,
    scope_kind: ScopeKind,
    ctx: &TypecheckContext,
  ) -> TypeId {
    if call.arguments.len() != 1 {
      self.add_diagnostic(
        DiagnosticMessage::ArgumentCountMismatch {
          expected: 1,
          got: call.arguments.len(),
          func_name: "typeOf".to_string(),
          span: call.span.clone(),
        }
        .report(),
      );
      return self.types.error();
    }

    self.typecheck_node(&call.arguments[0], scope_kind, ctx);
    self.types.u32()
  }

  fn typecheck_sizeof_builtin(
    &mut self,
    call: &ASTCallExpression,
    _scope_kind: ScopeKind,
    _ctx: &TypecheckContext,
  ) -> TypeId {
    let type_args = match &call.type_args {
      Some(args) => args,
      None => {
        self.add_diagnostic(
          DiagnosticMessage::WrongNumberOfTypeArgs {
            expected: 1,
            got: 0,
            type_name: "sizeOf".to_string(),
            span: call.span.clone(),
          }
          .report(),
        );
        return self.types.error();
      },
    };

    if type_args.len() != 1 {
      self.add_diagnostic(
        DiagnosticMessage::WrongNumberOfTypeArgs {
          expected: 1,
          got: type_args.len(),
          type_name: "sizeOf".to_string(),
          span: call.span.clone(),
        }
        .report(),
      );
      return self.types.error();
    }

    if !call.arguments.is_empty() {
      self.add_diagnostic(
        DiagnosticMessage::ArgumentCountMismatch {
          expected: 0,
          got: call.arguments.len(),
          func_name: "sizeOf".to_string(),
          span: call.span.clone(),
        }
        .report(),
      );
      return self.types.error();
    }

    let value_type = self.resolve_type_syntax_impl(&type_args[0], Some(&call.span));

    // Allow type parameters for generic support
    if matches!(self.types.get(&value_type), ignis_type::types::Type::Param { .. }) {
      return self.types.u64();
    }

    self.types.u64()
  }

  fn typecheck_alignof_builtin(
    &mut self,
    call: &ASTCallExpression,
    _scope_kind: ScopeKind,
    _ctx: &TypecheckContext,
  ) -> TypeId {
    let type_args = match &call.type_args {
      Some(args) => args,
      None => {
        self.add_diagnostic(
          DiagnosticMessage::WrongNumberOfTypeArgs {
            expected: 1,
            got: 0,
            type_name: "alignOf".to_string(),
            span: call.span.clone(),
          }
          .report(),
        );
        return self.types.error();
      },
    };

    if type_args.len() != 1 {
      self.add_diagnostic(
        DiagnosticMessage::WrongNumberOfTypeArgs {
          expected: 1,
          got: type_args.len(),
          type_name: "alignOf".to_string(),
          span: call.span.clone(),
        }
        .report(),
      );
      return self.types.error();
    }

    if !call.arguments.is_empty() {
      self.add_diagnostic(
        DiagnosticMessage::ArgumentCountMismatch {
          expected: 0,
          got: call.arguments.len(),
          func_name: "alignOf".to_string(),
          span: call.span.clone(),
        }
        .report(),
      );
      return self.types.error();
    }

    let value_type = self.resolve_type_syntax_impl(&type_args[0], Some(&call.span));

    // Allow type parameters for generic support
    if matches!(self.types.get(&value_type), ignis_type::types::Type::Param { .. }) {
      return self.types.u64();
    }

    self.types.u64()
  }

  fn typecheck_builtin_read(
    &mut self,
    bc: &ASTBuiltinCall,
    scope_kind: ScopeKind,
    ctx: &TypecheckContext,
  ) -> TypeId {
    let type_args = match &bc.type_args {
      Some(args) => args,
      None => {
        self.add_diagnostic(
          DiagnosticMessage::WrongNumberOfTypeArgs {
            expected: 1,
            got: 0,
            type_name: "@read".to_string(),
            span: bc.span.clone(),
          }
          .report(),
        );
        return self.types.error();
      },
    };

    if type_args.len() != 1 {
      self.add_diagnostic(
        DiagnosticMessage::WrongNumberOfTypeArgs {
          expected: 1,
          got: type_args.len(),
          type_name: "@read".to_string(),
          span: bc.span.clone(),
        }
        .report(),
      );
      return self.types.error();
    }

    let value_type = self.resolve_type_syntax_impl(&type_args[0], Some(&bc.span));

    // Allow type parameters for generic support
    if matches!(self.types.get(&value_type), ignis_type::types::Type::Param { .. }) {
      for arg in &bc.args {
        self.typecheck_node(arg, scope_kind, ctx);
      }
      return value_type;
    }

    if self.types.is_infer(&value_type) {
      self.add_diagnostic(
        DiagnosticMessage::CannotInferTypeParam {
          param_name: "T".to_string(),
          func_name: "@read".to_string(),
          span: bc.span.clone(),
        }
        .report(),
      );
      return self.types.error();
    }

    if bc.args.len() != 1 {
      self.add_diagnostic(
        DiagnosticMessage::BuiltinArgCount {
          name: "read".to_string(),
          expected: 1,
          got: bc.args.len(),
          span: bc.span.clone(),
        }
        .report(),
      );
      return self.types.error();
    }

    let pointer_type = self.types.pointer(value_type, true);
    let infer = InferContext::expecting(pointer_type);
    let arg_type = self.typecheck_node_with_infer(&bc.args[0], scope_kind, ctx, &infer);

    if !self.types.is_assignable(&arg_type, &pointer_type) {
      let expected = self.format_type_for_error(&pointer_type);
      let got = self.format_type_for_error(&arg_type);
      self.add_diagnostic(
        DiagnosticMessage::ArgumentTypeMismatch {
          param_idx: 1,
          expected,
          got,
          span: self.node_span(&bc.args[0]).clone(),
        }
        .report(),
      );
      return self.types.error();
    }

    if matches!(self.const_eval_expression_node(&bc.args[0], scope_kind), Some(ConstValue::Null)) {
      self.add_diagnostic(
        DiagnosticMessage::NullDereference {
          span: self.node_span(&bc.args[0]).clone(),
        }
        .report(),
      );

      self.set_type(&bc.args[0], &self.types.error());
      return self.types.error();
    }

    value_type
  }

  fn typecheck_builtin_write(
    &mut self,
    bc: &ASTBuiltinCall,
    scope_kind: ScopeKind,
    ctx: &TypecheckContext,
  ) -> TypeId {
    let type_args = match &bc.type_args {
      Some(args) => args,
      None => {
        self.add_diagnostic(
          DiagnosticMessage::WrongNumberOfTypeArgs {
            expected: 1,
            got: 0,
            type_name: "@write".to_string(),
            span: bc.span.clone(),
          }
          .report(),
        );
        return self.types.error();
      },
    };

    if type_args.len() != 1 {
      self.add_diagnostic(
        DiagnosticMessage::WrongNumberOfTypeArgs {
          expected: 1,
          got: type_args.len(),
          type_name: "@write".to_string(),
          span: bc.span.clone(),
        }
        .report(),
      );
      return self.types.error();
    }

    let value_type = self.resolve_type_syntax_impl(&type_args[0], Some(&bc.span));

    // Allow type parameters for generic support
    if matches!(self.types.get(&value_type), ignis_type::types::Type::Param { .. }) {
      for arg in &bc.args {
        self.typecheck_node(arg, scope_kind, ctx);
      }
      return self.types.void();
    }

    if self.types.is_infer(&value_type) {
      self.add_diagnostic(
        DiagnosticMessage::CannotInferTypeParam {
          param_name: "T".to_string(),
          func_name: "@write".to_string(),
          span: bc.span.clone(),
        }
        .report(),
      );
      return self.types.error();
    }

    if bc.args.len() != 2 {
      self.add_diagnostic(
        DiagnosticMessage::BuiltinArgCount {
          name: "write".to_string(),
          expected: 2,
          got: bc.args.len(),
          span: bc.span.clone(),
        }
        .report(),
      );
      return self.types.error();
    }

    let pointer_type = self.types.pointer(value_type, true);
    let ptr_infer = InferContext::expecting(pointer_type);
    let ptr_type = self.typecheck_node_with_infer(&bc.args[0], scope_kind, ctx, &ptr_infer);

    if !self.types.is_assignable(&ptr_type, &pointer_type) {
      let expected = self.format_type_for_error(&pointer_type);
      let got = self.format_type_for_error(&ptr_type);
      self.add_diagnostic(
        DiagnosticMessage::ArgumentTypeMismatch {
          param_idx: 1,
          expected,
          got,
          span: self.node_span(&bc.args[0]).clone(),
        }
        .report(),
      );
      return self.types.error();
    }

    let value_infer = InferContext::expecting(value_type);
    let arg_type = self.typecheck_node_with_infer(&bc.args[1], scope_kind, ctx, &value_infer);

    if !self.types.is_assignable(&arg_type, &value_type) {
      let expected = self.format_type_for_error(&value_type);
      let got = self.format_type_for_error(&arg_type);
      self.add_diagnostic(
        DiagnosticMessage::ArgumentTypeMismatch {
          param_idx: 2,
          expected,
          got,
          span: self.node_span(&bc.args[1]).clone(),
        }
        .report(),
      );
      return self.types.error();
    }

    if matches!(self.const_eval_expression_node(&bc.args[0], scope_kind), Some(ConstValue::Null)) {
      self.add_diagnostic(
        DiagnosticMessage::NullDereference {
          span: self.node_span(&bc.args[0]).clone(),
        }
        .report(),
      );

      self.set_type(&bc.args[0], &self.types.error());
      return self.types.error();
    }

    self.types.void()
  }

  fn typecheck_builtin_drop_in_place(
    &mut self,
    bc: &ASTBuiltinCall,
    scope_kind: ScopeKind,
    ctx: &TypecheckContext,
  ) -> TypeId {
    let type_args = match &bc.type_args {
      Some(args) => args,
      None => {
        self.add_diagnostic(
          DiagnosticMessage::WrongNumberOfTypeArgs {
            expected: 1,
            got: 0,
            type_name: "@dropInPlace".to_string(),
            span: bc.span.clone(),
          }
          .report(),
        );
        return self.types.error();
      },
    };

    if type_args.len() != 1 {
      self.add_diagnostic(
        DiagnosticMessage::WrongNumberOfTypeArgs {
          expected: 1,
          got: type_args.len(),
          type_name: "@dropInPlace".to_string(),
          span: bc.span.clone(),
        }
        .report(),
      );
      return self.types.error();
    }

    let value_type = self.resolve_type_syntax_impl(&type_args[0], Some(&bc.span));

    // Allow type parameters for generic support
    if matches!(self.types.get(&value_type), ignis_type::types::Type::Param { .. }) {
      for arg in &bc.args {
        self.typecheck_node(arg, scope_kind, ctx);
      }
      return self.types.void();
    }

    if self.types.is_infer(&value_type) {
      self.add_diagnostic(
        DiagnosticMessage::CannotInferTypeParam {
          param_name: "T".to_string(),
          func_name: "@dropInPlace".to_string(),
          span: bc.span.clone(),
        }
        .report(),
      );
      return self.types.error();
    }

    if bc.args.len() != 1 {
      self.add_diagnostic(
        DiagnosticMessage::BuiltinArgCount {
          name: "dropInPlace".to_string(),
          expected: 1,
          got: bc.args.len(),
          span: bc.span.clone(),
        }
        .report(),
      );
      return self.types.error();
    }

    let pointer_type = self.types.pointer(value_type, true);
    let infer = InferContext::expecting(pointer_type);
    let arg_type = self.typecheck_node_with_infer(&bc.args[0], scope_kind, ctx, &infer);

    if !self.types.is_assignable(&arg_type, &pointer_type) {
      let expected = self.format_type_for_error(&pointer_type);
      let got = self.format_type_for_error(&arg_type);
      self.add_diagnostic(
        DiagnosticMessage::ArgumentTypeMismatch {
          param_idx: 1,
          expected,
          got,
          span: self.node_span(&bc.args[0]).clone(),
        }
        .report(),
      );
      return self.types.error();
    }

    self.types.void()
  }

  fn typecheck_builtin_drop_glue(
    &mut self,
    bc: &ASTBuiltinCall,
  ) -> TypeId {
    let type_args = match &bc.type_args {
      Some(args) => args,
      None => {
        self.add_diagnostic(
          DiagnosticMessage::WrongNumberOfTypeArgs {
            expected: 1,
            got: 0,
            type_name: "@dropGlue".to_string(),
            span: bc.span.clone(),
          }
          .report(),
        );
        return self.types.error();
      },
    };

    if type_args.len() != 1 {
      self.add_diagnostic(
        DiagnosticMessage::WrongNumberOfTypeArgs {
          expected: 1,
          got: type_args.len(),
          type_name: "@dropGlue".to_string(),
          span: bc.span.clone(),
        }
        .report(),
      );
      return self.types.error();
    }

    let value_type = self.resolve_type_syntax_impl(&type_args[0], Some(&bc.span));

    // Allow type parameters for generic support
    if matches!(self.types.get(&value_type), ignis_type::types::Type::Param { .. }) {
      let u8_type = self.types.u8();
      let u8_ptr = self.types.pointer(u8_type, true);
      let void_type = self.types.void();
      return self.types.function(vec![u8_ptr], void_type, false);
    }

    if self.types.is_infer(&value_type) {
      self.add_diagnostic(
        DiagnosticMessage::CannotInferTypeParam {
          param_name: "T".to_string(),
          func_name: "@dropGlue".to_string(),
          span: bc.span.clone(),
        }
        .report(),
      );
      return self.types.error();
    }

    if !bc.args.is_empty() {
      self.add_diagnostic(
        DiagnosticMessage::BuiltinArgCount {
          name: "dropGlue".to_string(),
          expected: 0,
          got: bc.args.len(),
          span: bc.span.clone(),
        }
        .report(),
      );
      return self.types.error();
    }

    // Returns a function pointer: (*mut u8) -> void
    let u8_type = self.types.u8();
    let u8_ptr = self.types.pointer(u8_type, true);
    let void_type = self.types.void();
    self.types.function(vec![u8_ptr], void_type, false)
  }

  fn typecheck_maxof_builtin(
    &mut self,
    call: &ASTCallExpression,
    _scope_kind: ScopeKind,
    _ctx: &TypecheckContext,
  ) -> TypeId {
    self.typecheck_minmax_builtin(call, "maxOf")
  }

  fn typecheck_minof_builtin(
    &mut self,
    call: &ASTCallExpression,
    _scope_kind: ScopeKind,
    _ctx: &TypecheckContext,
  ) -> TypeId {
    self.typecheck_minmax_builtin(call, "minOf")
  }

  fn typecheck_minmax_builtin(
    &mut self,
    call: &ASTCallExpression,
    func_name: &str,
  ) -> TypeId {
    let type_args = match &call.type_args {
      Some(args) => args,
      None => {
        self.add_diagnostic(
          DiagnosticMessage::WrongNumberOfTypeArgs {
            expected: 1,
            got: 0,
            type_name: func_name.to_string(),
            span: call.span.clone(),
          }
          .report(),
        );
        return self.types.error();
      },
    };

    if type_args.len() != 1 {
      self.add_diagnostic(
        DiagnosticMessage::WrongNumberOfTypeArgs {
          expected: 1,
          got: type_args.len(),
          type_name: func_name.to_string(),
          span: call.span.clone(),
        }
        .report(),
      );
      return self.types.error();
    }

    if !call.arguments.is_empty() {
      self.add_diagnostic(
        DiagnosticMessage::ArgumentCountMismatch {
          expected: 0,
          got: call.arguments.len(),
          func_name: func_name.to_string(),
          span: call.span.clone(),
        }
        .report(),
      );
      return self.types.error();
    }

    let value_type = self.resolve_type_syntax_impl(&type_args[0], Some(&call.span));

    // Allow type parameters for generic support
    if self.types.contains_type_param(&value_type) {
      return value_type;
    }

    // Check that the type is numeric
    let ty = self.types.get(&value_type);
    let is_numeric = matches!(
      ty,
      Type::I8
        | Type::I16
        | Type::I32
        | Type::I64
        | Type::U8
        | Type::U16
        | Type::U32
        | Type::U64
        | Type::F32
        | Type::F64
    );

    if !is_numeric {
      self.add_diagnostic(
        DiagnosticMessage::InvalidMinMaxType {
          func_name: func_name.to_string(),
          got: self.format_type_for_error(&value_type),
          span: call.span.clone(),
        }
        .report(),
      );
      return self.types.error();
    }

    value_type
  }

  // ========================================================================
  // @builtin(...) Typechecking
  // ========================================================================

  fn typecheck_builtin_call(
    &mut self,
    _node_id: &NodeId,
    bc: &ASTBuiltinCall,
    scope_kind: ScopeKind,
    ctx: &TypecheckContext,
  ) -> TypeId {
    let name = self.get_symbol_name(&bc.name);

    match name.as_str() {
      "configFlag" => self.typecheck_builtin_config_flag(bc),
      "compileError" => self.typecheck_builtin_compile_error(bc),
      "sizeOf" => {
        let ty = self.types.u64();
        self.typecheck_builtin_type_arg(bc, "sizeOf", ty)
      },
      "alignOf" => {
        let ty = self.types.u64();
        self.typecheck_builtin_type_arg(bc, "alignOf", ty)
      },
      "typeName" => {
        let ty = self.types.str();
        self.typecheck_builtin_type_arg(bc, "typeName", ty)
      },
      "bitCast" => self.typecheck_builtin_bitcast(bc, scope_kind, ctx),
      "pointerCast" => self.typecheck_builtin_pointer_cast(bc, scope_kind, ctx),
      "integerFromPointer" => self.typecheck_builtin_integer_from_pointer(bc, scope_kind, ctx),
      "pointerFromInteger" => self.typecheck_builtin_pointer_from_integer(bc, scope_kind, ctx),
      "read" => self.typecheck_builtin_read(bc, scope_kind, ctx),
      "write" => self.typecheck_builtin_write(bc, scope_kind, ctx),
      "dropInPlace" => self.typecheck_builtin_drop_in_place(bc, scope_kind, ctx),
      "dropGlue" => self.typecheck_builtin_drop_glue(bc),
      "panic" => self.typecheck_builtin_panic(bc),
      "trap" => self.typecheck_builtin_no_args(bc, "trap"),
      "unreachable" => self.typecheck_builtin_no_args(bc, "unreachable"),
      _ => {
        self.add_diagnostic(
          DiagnosticMessage::UnknownBuiltin {
            name,
            span: bc.span.clone(),
          }
          .report(),
        );
        self.types.error()
      },
    }
  }

  fn typecheck_builtin_config_flag(
    &mut self,
    bc: &ASTBuiltinCall,
  ) -> TypeId {
    if bc.type_args.is_some() {
      self.add_diagnostic(
        DiagnosticMessage::WrongNumberOfTypeArgs {
          expected: 0,
          got: bc.type_args.as_ref().map_or(0, |ta| ta.len()),
          type_name: "@configFlag".to_string(),
          span: bc.span.clone(),
        }
        .report(),
      );
      return self.types.error();
    }

    if bc.args.len() != 1 {
      self.add_diagnostic(
        DiagnosticMessage::BuiltinArgCount {
          name: "configFlag".to_string(),
          expected: 1,
          got: bc.args.len(),
          span: bc.span.clone(),
        }
        .report(),
      );
      return self.types.error();
    }

    if self.extract_string_literal(&bc.args[0]).is_none() {
      self.add_diagnostic(
        DiagnosticMessage::BuiltinExpectedStringLiteral {
          name: "configFlag".to_string(),
          span: bc.span.clone(),
        }
        .report(),
      );
      return self.types.error();
    }

    self.types.boolean()
  }

  fn typecheck_builtin_compile_error(
    &mut self,
    bc: &ASTBuiltinCall,
  ) -> TypeId {
    if bc.type_args.is_some() {
      self.add_diagnostic(
        DiagnosticMessage::WrongNumberOfTypeArgs {
          expected: 0,
          got: bc.type_args.as_ref().map_or(0, |ta| ta.len()),
          type_name: "@compileError".to_string(),
          span: bc.span.clone(),
        }
        .report(),
      );
      return self.types.error();
    }

    if bc.args.len() != 1 {
      self.add_diagnostic(
        DiagnosticMessage::BuiltinArgCount {
          name: "compileError".to_string(),
          expected: 1,
          got: bc.args.len(),
          span: bc.span.clone(),
        }
        .report(),
      );
      return self.types.error();
    }

    let Some(msg) = self.extract_string_literal(&bc.args[0]) else {
      self.add_diagnostic(
        DiagnosticMessage::BuiltinExpectedStringLiteral {
          name: "compileError".to_string(),
          span: bc.span.clone(),
        }
        .report(),
      );
      return self.types.error();
    };

    self.add_diagnostic(
      DiagnosticMessage::CompileError {
        message: msg,
        span: bc.span.clone(),
      }
      .report(),
    );

    self.types.never()
  }

  fn typecheck_builtin_type_arg(
    &mut self,
    bc: &ASTBuiltinCall,
    name: &str,
    result_type: TypeId,
  ) -> TypeId {
    let type_args = match &bc.type_args {
      Some(ta) => ta,
      None => {
        self.add_diagnostic(
          DiagnosticMessage::WrongNumberOfTypeArgs {
            expected: 1,
            got: 0,
            type_name: format!("@{}", name),
            span: bc.span.clone(),
          }
          .report(),
        );
        return self.types.error();
      },
    };

    if type_args.len() != 1 {
      self.add_diagnostic(
        DiagnosticMessage::WrongNumberOfTypeArgs {
          expected: 1,
          got: type_args.len(),
          type_name: format!("@{}", name),
          span: bc.span.clone(),
        }
        .report(),
      );
      return self.types.error();
    }

    if !bc.args.is_empty() {
      self.add_diagnostic(
        DiagnosticMessage::BuiltinArgCount {
          name: name.to_string(),
          expected: 0,
          got: bc.args.len(),
          span: bc.span.clone(),
        }
        .report(),
      );
      return self.types.error();
    }

    self.resolve_type_syntax_with_span(&type_args[0], &bc.span);
    result_type
  }

  fn typecheck_builtin_cast_1_type_1_expr(
    &mut self,
    bc: &ASTBuiltinCall,
    name: &str,
    scope_kind: ScopeKind,
    ctx: &TypecheckContext,
  ) -> Option<(TypeId, TypeId)> {
    let type_args = match &bc.type_args {
      Some(ta) => ta,
      None => {
        self.add_diagnostic(
          DiagnosticMessage::WrongNumberOfTypeArgs {
            expected: 1,
            got: 0,
            type_name: format!("@{}", name),
            span: bc.span.clone(),
          }
          .report(),
        );
        return None;
      },
    };

    if type_args.len() != 1 {
      self.add_diagnostic(
        DiagnosticMessage::WrongNumberOfTypeArgs {
          expected: 1,
          got: type_args.len(),
          type_name: format!("@{}", name),
          span: bc.span.clone(),
        }
        .report(),
      );
      return None;
    }

    if bc.args.len() != 1 {
      self.add_diagnostic(
        DiagnosticMessage::BuiltinArgCount {
          name: name.to_string(),
          expected: 1,
          got: bc.args.len(),
          span: bc.span.clone(),
        }
        .report(),
      );
      return None;
    }

    let resolved = self.resolve_type_syntax_with_span(&type_args[0], &bc.span);
    let expr_ty = self.typecheck_node(&bc.args[0], scope_kind, ctx);

    Some((resolved, expr_ty))
  }

  fn typecheck_builtin_bitcast(
    &mut self,
    bc: &ASTBuiltinCall,
    scope_kind: ScopeKind,
    ctx: &TypecheckContext,
  ) -> TypeId {
    let Some((target_ty, _expr_ty)) = self.typecheck_builtin_cast_1_type_1_expr(bc, "bitCast", scope_kind, ctx) else {
      return self.types.error();
    };

    target_ty
  }

  fn typecheck_builtin_pointer_cast(
    &mut self,
    bc: &ASTBuiltinCall,
    scope_kind: ScopeKind,
    ctx: &TypecheckContext,
  ) -> TypeId {
    let Some((target_ty, expr_ty)) = self.typecheck_builtin_cast_1_type_1_expr(bc, "pointerCast", scope_kind, ctx)
    else {
      return self.types.error();
    };

    if !self.types.is_pointer(&target_ty) {
      self.add_diagnostic(
        DiagnosticMessage::BuiltinTypeConstraint {
          name: "pointerCast".to_string(),
          constraint: "expected pointer type argument".to_string(),
          span: bc.span.clone(),
        }
        .report(),
      );
      return self.types.error();
    }

    if !self.types.is_pointer(&expr_ty) && !self.types.is_null_ptr(&expr_ty) {
      self.add_diagnostic(
        DiagnosticMessage::BuiltinTypeConstraint {
          name: "pointerCast".to_string(),
          constraint: "expected pointer argument".to_string(),
          span: bc.span.clone(),
        }
        .report(),
      );
      return self.types.error();
    }

    target_ty
  }

  fn typecheck_builtin_integer_from_pointer(
    &mut self,
    bc: &ASTBuiltinCall,
    scope_kind: ScopeKind,
    ctx: &TypecheckContext,
  ) -> TypeId {
    if bc.type_args.is_some() {
      self.add_diagnostic(
        DiagnosticMessage::WrongNumberOfTypeArgs {
          expected: 0,
          got: bc.type_args.as_ref().map_or(0, |ta| ta.len()),
          type_name: "@integerFromPointer".to_string(),
          span: bc.span.clone(),
        }
        .report(),
      );
      return self.types.error();
    }

    if bc.args.len() != 1 {
      self.add_diagnostic(
        DiagnosticMessage::BuiltinArgCount {
          name: "integerFromPointer".to_string(),
          expected: 1,
          got: bc.args.len(),
          span: bc.span.clone(),
        }
        .report(),
      );
      return self.types.error();
    }

    let expr_ty = self.typecheck_node(&bc.args[0], scope_kind, ctx);

    if !self.types.is_pointer(&expr_ty) && !self.types.is_null_ptr(&expr_ty) {
      self.add_diagnostic(
        DiagnosticMessage::BuiltinTypeConstraint {
          name: "integerFromPointer".to_string(),
          constraint: "expected pointer argument".to_string(),
          span: bc.span.clone(),
        }
        .report(),
      );
      return self.types.error();
    }

    self.types.u64()
  }

  fn typecheck_builtin_pointer_from_integer(
    &mut self,
    bc: &ASTBuiltinCall,
    scope_kind: ScopeKind,
    ctx: &TypecheckContext,
  ) -> TypeId {
    let Some((target_ty, expr_ty)) =
      self.typecheck_builtin_cast_1_type_1_expr(bc, "pointerFromInteger", scope_kind, ctx)
    else {
      return self.types.error();
    };

    if !self.types.is_pointer(&target_ty) {
      self.add_diagnostic(
        DiagnosticMessage::BuiltinTypeConstraint {
          name: "pointerFromInteger".to_string(),
          constraint: "expected pointer type argument".to_string(),
          span: bc.span.clone(),
        }
        .report(),
      );
      return self.types.error();
    }

    if !self.types.is_integer(&expr_ty) {
      self.add_diagnostic(
        DiagnosticMessage::BuiltinTypeConstraint {
          name: "pointerFromInteger".to_string(),
          constraint: "expected integer argument".to_string(),
          span: bc.span.clone(),
        }
        .report(),
      );
      return self.types.error();
    }

    target_ty
  }

  fn typecheck_builtin_panic(
    &mut self,
    bc: &ASTBuiltinCall,
  ) -> TypeId {
    if bc.type_args.is_some() {
      self.add_diagnostic(
        DiagnosticMessage::WrongNumberOfTypeArgs {
          expected: 0,
          got: bc.type_args.as_ref().map_or(0, |ta| ta.len()),
          type_name: "@panic".to_string(),
          span: bc.span.clone(),
        }
        .report(),
      );
      return self.types.error();
    }

    if bc.args.len() != 1 {
      self.add_diagnostic(
        DiagnosticMessage::BuiltinArgCount {
          name: "panic".to_string(),
          expected: 1,
          got: bc.args.len(),
          span: bc.span.clone(),
        }
        .report(),
      );
      return self.types.error();
    }

    if self.extract_string_literal(&bc.args[0]).is_none() {
      self.add_diagnostic(
        DiagnosticMessage::BuiltinExpectedStringLiteral {
          name: "panic".to_string(),
          span: bc.span.clone(),
        }
        .report(),
      );
      return self.types.error();
    }

    self.types.never()
  }

  fn typecheck_builtin_no_args(
    &mut self,
    bc: &ASTBuiltinCall,
    name: &str,
  ) -> TypeId {
    if bc.type_args.is_some() {
      self.add_diagnostic(
        DiagnosticMessage::WrongNumberOfTypeArgs {
          expected: 0,
          got: bc.type_args.as_ref().map_or(0, |ta| ta.len()),
          type_name: format!("@{}", name),
          span: bc.span.clone(),
        }
        .report(),
      );
      return self.types.error();
    }

    if !bc.args.is_empty() {
      self.add_diagnostic(
        DiagnosticMessage::BuiltinArgCount {
          name: name.to_string(),
          expected: 0,
          got: bc.args.len(),
          span: bc.span.clone(),
        }
        .report(),
      );
      return self.types.error();
    }

    self.types.never()
  }

  fn extract_string_literal(
    &self,
    node_id: &NodeId,
  ) -> Option<String> {
    let node = self.ast.get(node_id);
    if let ASTNode::Expression(ASTExpression::Literal(lit)) = node
      && let IgnisLiteralValue::String(s) = &lit.value
    {
      return Some(s.clone());
    }
    None
  }

  fn is_pointer_type(
    &self,
    ty: &TypeId,
  ) -> bool {
    matches!(self.types.get(ty), ignis_type::types::Type::Pointer { .. })
  }

  fn pointer_inner_type(
    &self,
    ty: &TypeId,
  ) -> Option<TypeId> {
    match self.types.get(ty) {
      ignis_type::types::Type::Pointer { inner, .. } => Some(*inner),
      _ => None,
    }
  }

  fn coerce_null_literal(
    &mut self,
    node_id: &NodeId,
    target: &TypeId,
  ) -> bool {
    let node = self.ast.get(node_id);
    match node {
      ASTNode::Expression(ASTExpression::Literal(lit)) if matches!(lit.value, IgnisLiteralValue::Null) => {
        self.set_type(node_id, target);
        true
      },
      _ => false,
    }
  }

  fn typecheck_binary(
    &mut self,
    binary: &ignis_ast::expressions::ASTBinary,
    scope_kind: ScopeKind,
    ctx: &TypecheckContext,
  ) -> TypeId {
    let left_type = self.typecheck_node(&binary.left, scope_kind, ctx);

    // Still typecheck right side to collect independent errors there,
    // but mark null literals as error to prevent "cannot infer null" errors.
    if self.types.is_error(&left_type) {
      let right_type = self.typecheck_node(&binary.right, scope_kind, ctx);
      if self.types.is_null_ptr(&right_type) {
        self.set_type(&binary.right, &self.types.error());
      }
      return self.types.error();
    }

    let right_type = match binary.operator {
      ASTBinaryOperator::Add | ASTBinaryOperator::Subtract if self.is_pointer_type(&left_type) => {
        let infer = InferContext::expecting(self.types.i64());
        self.typecheck_node_with_infer(&binary.right, scope_kind, ctx, &infer)
      },
      _ if self.types.is_numeric(&left_type) => {
        let infer = InferContext::expecting(left_type);
        self.typecheck_node_with_infer(&binary.right, scope_kind, ctx, &infer)
      },
      _ => self.typecheck_node(&binary.right, scope_kind, ctx),
    };

    if self.types.is_error(&right_type) {
      return self.types.error();
    }

    match binary.operator {
      ASTBinaryOperator::Add | ASTBinaryOperator::Subtract => {
        if self.types.is_numeric(&left_type) && self.types.is_numeric(&right_type) {
          return self.typecheck_common_type(&left_type, &right_type, &binary.span);
        }

        let is_pointer_pair = self.is_pointer_type(&left_type) || self.is_pointer_type(&right_type);

        if self.is_pointer_type(&left_type) && self.types.types_equal(&right_type, &self.types.i64()) {
          return left_type;
        }

        if binary.operator == ASTBinaryOperator::Subtract
          && self.is_pointer_type(&left_type)
          && self.is_pointer_type(&right_type)
        {
          let left_inner = self.pointer_inner_type(&left_type);
          let right_inner = self.pointer_inner_type(&right_type);

          if let (Some(left_base), Some(right_base)) = (left_inner, right_inner) {
            if self.types.types_equal(&left_base, &right_base) {
              return self.types.i64();
            }

            let u8_type = self.types.u8();
            if self.types.types_equal(&left_base, &u8_type) && self.types.types_equal(&right_base, &u8_type) {
              return self.types.i64();
            }
          }
        }

        let operator = format!("{:?}", binary.operator);
        let left = self.format_type_for_error(&left_type);
        let right = self.format_type_for_error(&right_type);

        let diagnostic = if is_pointer_pair {
          DiagnosticMessage::InvalidPointerArithmetic {
            operator,
            left_type: left,
            right_type: right,
            span: binary.span.clone(),
          }
        } else {
          DiagnosticMessage::InvalidBinaryOperandType {
            operator,
            left_type: left,
            right_type: right,
            span: binary.span.clone(),
          }
        };

        self.add_diagnostic(diagnostic.report());
        self.types.error()
      },
      ASTBinaryOperator::Multiply | ASTBinaryOperator::Divide | ASTBinaryOperator::Modulo => {
        if self.types.is_numeric(&left_type) && self.types.is_numeric(&right_type) {
          self.typecheck_common_type(&left_type, &right_type, &binary.span)
        } else {
          let operator = format!("{:?}", binary.operator);
          let left = self.format_type_for_error(&left_type);
          let right = self.format_type_for_error(&right_type);
          self.add_diagnostic(
            DiagnosticMessage::InvalidBinaryOperandType {
              operator,
              left_type: left,
              right_type: right,
              span: binary.span.clone(),
            }
            .report(),
          );
          self.types.error()
        }
      },
      ASTBinaryOperator::Equal | ASTBinaryOperator::NotEqual => {
        if self.types.is_numeric(&left_type) && self.types.is_numeric(&right_type) {
          return self.types.boolean();
        }

        let mut resolved_left = left_type;
        let mut resolved_right = right_type;

        if self.is_pointer_type(&left_type) && self.types.is_null_ptr(&right_type) {
          if self.coerce_null_literal(&binary.right, &left_type) {
            resolved_right = left_type;
          }
        } else if self.is_pointer_type(&right_type) && self.types.is_null_ptr(&left_type) {
          if self.coerce_null_literal(&binary.left, &right_type) {
            resolved_left = right_type;
          }
        } else if self.types.is_null_ptr(&left_type) && self.types.is_null_ptr(&right_type) {
          let void_ptr = self.types.pointer(self.types.void(), true);
          if self.coerce_null_literal(&binary.left, &void_ptr) {
            resolved_left = void_ptr;
          }
          if self.coerce_null_literal(&binary.right, &void_ptr) {
            resolved_right = void_ptr;
          }
        }

        if self.is_pointer_type(&resolved_left) && self.is_pointer_type(&resolved_right) {
          return self.types.boolean();
        }

        // Unit enum comparison: both sides must be the same enum type with no payload variants
        if self.types.types_equal(&left_type, &right_type) && self.types.is_unit_enum(&left_type, &self.defs) {
          return self.types.boolean();
        }

        if matches!(self.types.get(&left_type), Type::Atom) && matches!(self.types.get(&right_type), Type::Atom) {
          return self.types.boolean();
        }

        let operator = format!("{:?}", binary.operator);
        let left = self.format_type_for_error(&left_type);
        let right = self.format_type_for_error(&right_type);

        let diagnostic = if self.is_pointer_type(&left_type) || self.is_pointer_type(&right_type) {
          DiagnosticMessage::InvalidPointerArithmetic {
            operator,
            left_type: left,
            right_type: right,
            span: binary.span.clone(),
          }
        } else {
          DiagnosticMessage::InvalidBinaryOperandType {
            operator,
            left_type: left,
            right_type: right,
            span: binary.span.clone(),
          }
        };

        self.add_diagnostic(diagnostic.report());
        self.types.error()
      },
      ASTBinaryOperator::LessThan
      | ASTBinaryOperator::LessThanOrEqual
      | ASTBinaryOperator::GreaterThan
      | ASTBinaryOperator::GreaterThanOrEqual => {
        if self.types.is_numeric(&left_type) && self.types.is_numeric(&right_type) {
          self.types.boolean()
        } else {
          let operator = format!("{:?}", binary.operator);
          let left = self.format_type_for_error(&left_type);
          let right = self.format_type_for_error(&right_type);
          self.add_diagnostic(
            DiagnosticMessage::InvalidBinaryOperandType {
              operator,
              left_type: left,
              right_type: right,
              span: binary.span.clone(),
            }
            .report(),
          );
          self.types.error()
        }
      },
      ASTBinaryOperator::And => {
        let boolean_type = self.types.boolean();

        let left_span = self.node_span(&binary.left).clone();
        let right_span = self.node_span(&binary.right).clone();

        self.typecheck_assignment(&boolean_type, &left_type, &left_span);
        self.typecheck_assignment(&boolean_type, &right_type, &right_span);

        self.types.boolean()
      },
      ASTBinaryOperator::Or => {
        if self.node_contains_let_condition(&binary.left) || self.node_contains_let_condition(&binary.right) {
          self.add_diagnostic(
            DiagnosticMessage::LetConditionInOrExpression {
              span: binary.span.clone(),
            }
            .report(),
          );
        }

        let boolean_type = self.types.boolean();

        let left_span = self.node_span(&binary.left).clone();
        let right_span = self.node_span(&binary.right).clone();

        self.typecheck_assignment(&boolean_type, &left_type, &left_span);
        self.typecheck_assignment(&boolean_type, &right_type, &right_span);

        self.types.boolean()
      },
      ASTBinaryOperator::BitAnd
      | ASTBinaryOperator::BitOr
      | ASTBinaryOperator::BitXor
      | ASTBinaryOperator::ShiftLeft
      | ASTBinaryOperator::ShiftRight => {
        if self.types.is_integer(&left_type) && self.types.is_integer(&right_type) {
          self.typecheck_common_type(&left_type, &right_type, &binary.span)
        } else {
          let operator = format!("{:?}", binary.operator);
          let left = self.format_type_for_error(&left_type);
          let right = self.format_type_for_error(&right_type);
          self.add_diagnostic(
            DiagnosticMessage::InvalidBinaryOperandType {
              operator,
              left_type: left,
              right_type: right,
              span: binary.span.clone(),
            }
            .report(),
          );
          self.types.error()
        }
      },
    }
  }

  fn node_contains_let_condition(
    &self,
    node_id: &NodeId,
  ) -> bool {
    let ASTNode::Expression(expr) = self.ast.get(node_id) else {
      return false;
    };

    self.expression_contains_let_condition(expr)
  }

  fn expression_contains_let_condition(
    &self,
    expr: &ASTExpression,
  ) -> bool {
    match expr {
      ASTExpression::LetCondition(_) => true,
      ASTExpression::Assignment(assign) => {
        self.node_contains_let_condition(&assign.target) || self.node_contains_let_condition(&assign.value)
      },
      ASTExpression::Binary(binary) => {
        self.node_contains_let_condition(&binary.left) || self.node_contains_let_condition(&binary.right)
      },
      ASTExpression::Ternary(ternary) => {
        self.node_contains_let_condition(&ternary.condition)
          || self.node_contains_let_condition(&ternary.then_expr)
          || self.node_contains_let_condition(&ternary.else_expr)
      },
      ASTExpression::Call(call) => {
        self.node_contains_let_condition(&call.callee)
          || call
            .arguments
            .iter()
            .any(|argument| self.node_contains_let_condition(argument))
      },
      ASTExpression::Cast(cast) => self.node_contains_let_condition(&cast.expression),
      ASTExpression::Dereference(deref) => self.node_contains_let_condition(&deref.inner),
      ASTExpression::Grouped(grouped) => self.node_contains_let_condition(&grouped.expression),
      ASTExpression::Reference(reference) => self.node_contains_let_condition(&reference.inner),
      ASTExpression::Unary(unary) => self.node_contains_let_condition(&unary.operand),
      ASTExpression::Vector(vector) => vector.items.iter().any(|item| self.node_contains_let_condition(item)),
      ASTExpression::VectorAccess(access) => {
        self.node_contains_let_condition(&access.name) || self.node_contains_let_condition(&access.index)
      },
      ASTExpression::PostfixIncrement { expr, .. } | ASTExpression::PostfixDecrement { expr, .. } => {
        self.node_contains_let_condition(expr)
      },
      ASTExpression::MemberAccess(member_access) => self.node_contains_let_condition(&member_access.object),
      ASTExpression::RecordInit(record_init) => record_init
        .fields
        .iter()
        .any(|field| self.node_contains_let_condition(&field.value)),
      ASTExpression::BuiltinCall(builtin_call) => builtin_call
        .args
        .iter()
        .any(|argument| self.node_contains_let_condition(argument)),
      ASTExpression::Match(match_expr) => {
        self.node_contains_let_condition(&match_expr.scrutinee)
          || match_expr.arms.iter().any(|arm| {
            arm
              .guard
              .as_ref()
              .is_some_and(|guard| self.node_contains_let_condition(guard))
              || self.node_contains_let_condition(&arm.body)
          })
      },
      ASTExpression::CaptureOverride(co) => self.node_contains_let_condition(&co.inner),
      ASTExpression::Pipe { lhs, rhs, .. } => {
        self.node_contains_let_condition(lhs) || self.node_contains_let_condition(rhs)
      },
      ASTExpression::Literal(_)
      | ASTExpression::Variable(_)
      | ASTExpression::Path(_)
      | ASTExpression::Lambda(_)
      | ASTExpression::PipePlaceholder { .. } => false,
    }
  }

  fn typecheck_unary(
    &mut self,
    unary: &ignis_ast::expressions::ASTUnary,
    scope_kind: ScopeKind,
    ctx: &TypecheckContext,
  ) -> TypeId {
    let operand_type = self.typecheck_node(&unary.operand, scope_kind, ctx);

    if self.types.is_error(&operand_type) {
      return self.types.error();
    }

    match unary.operator {
      UnaryOperator::Negate => {
        if self.types.is_numeric(&operand_type) {
          operand_type
        } else {
          let operator = "-".to_string();
          let type_name = self.format_type_for_error(&operand_type);
          self.add_diagnostic(
            DiagnosticMessage::InvalidUnaryOperandType {
              operator,
              operand_type: type_name,
              span: unary.span.clone(),
            }
            .report(),
          );
          self.types.error()
        }
      },
      UnaryOperator::Not => {
        let boolean_type = self.types.boolean();
        if !self.types.types_equal(&boolean_type, &operand_type) {
          let operator = "!".to_string();
          let type_name = self.format_type_for_error(&operand_type);
          self.add_diagnostic(
            DiagnosticMessage::InvalidUnaryOperandType {
              operator,
              operand_type: type_name,
              span: unary.span.clone(),
            }
            .report(),
          );
        }
        self.types.boolean()
      },
      UnaryOperator::BitNot => {
        if self.types.is_integer(&operand_type) {
          operand_type
        } else {
          let operator = "~".to_string();
          let type_name = self.format_type_for_error(&operand_type);
          self.add_diagnostic(
            DiagnosticMessage::InvalidUnaryOperandType {
              operator,
              operand_type: type_name,
              span: unary.span.clone(),
            }
            .report(),
          );
          self.types.error()
        }
      },
      UnaryOperator::Increment | UnaryOperator::Decrement => {
        if self.types.is_numeric(&operand_type) {
          operand_type
        } else {
          let operator = match unary.operator {
            UnaryOperator::Increment => "++",
            UnaryOperator::Decrement => "--",
            _ => unreachable!(),
          };
          let type_name = self.format_type_for_error(&operand_type);
          self.add_diagnostic(
            DiagnosticMessage::InvalidUnaryOperandType {
              operator: operator.to_string(),
              operand_type: type_name,
              span: unary.span.clone(),
            }
            .report(),
          );
          self.types.error()
        }
      },
    }
  }

  fn typecheck_assignment_expr(
    &mut self,
    assign: &ignis_ast::expressions::ASTAssignment,
    scope_kind: ScopeKind,
    ctx: &TypecheckContext,
  ) -> TypeId {
    let target_type = self.typecheck_node(&assign.target, scope_kind, ctx);
    let infer = InferContext::expecting(target_type);
    let value_type = self.typecheck_node_with_infer(&assign.value, scope_kind, ctx, &infer);

    let target_node = self.ast.get(&assign.target);
    if let ASTNode::Expression(target_expr) = target_node
      && !self.is_mutable_expression(target_expr)
    {
      let var_name = self.get_var_name_from_expr(target_expr);
      self.add_diagnostic(
        DiagnosticMessage::ImmutableAssignment {
          var_name,
          span: assign.span.clone(),
        }
        .report(),
      );
    }

    match assign.operator {
      ASTAssignmentOperator::Assign => {
        self.typecheck_assignment(&target_type, &value_type, &assign.span);
      },
      _ => {
        if !self.types.is_error(&target_type) && !self.types.is_error(&value_type) {
          if self.types.is_numeric(&target_type) && self.types.is_numeric(&value_type) {
            self.typecheck_common_type(&target_type, &value_type, &assign.span);
          } else {
            let operator_str = format!("{:?}", assign.operator);
            let type_name = self.format_type_for_error(&target_type);
            self.add_diagnostic(
              DiagnosticMessage::CompoundAssignmentNonNumeric {
                operator: operator_str,
                type_name,
                span: assign.span.clone(),
              }
              .report(),
            );
          }
        }
      },
    };

    target_type
  }

  fn typecheck_assignment(
    &mut self,
    target_type: &TypeId,
    value_type: &TypeId,
    span: &Span,
  ) {
    if self.types.is_error(target_type) || self.types.is_error(value_type) {
      return;
    }

    // Type inference for generic calls happens during lowering, so we can't
    // verify types that still contain unsubstituted type parameters.
    if self.types.contains_type_param(target_type) || self.types.contains_type_param(value_type) {
      return;
    }

    if self.types.is_infer_var(target_type) {
      let _ = self
        .infer_ctx
        .unify(*target_type, *value_type, span, ConstraintReason::Assignment, &mut self.types);
      return;
    }

    if self.types.is_null_ptr(value_type) {
      if self.is_pointer_type(target_type) {
        return;
      }

      self.add_diagnostic(DiagnosticMessage::InvalidNullLiteral { span: span.clone() }.report());
      return;
    }

    if !self.types.is_assignable(target_type, value_type) {
      let target_name = self.format_type_for_error(target_type);
      let value_name = self.format_type_for_error(value_type);

      self.add_diagnostic(
        DiagnosticMessage::AssignmentTypeMismatch {
          expected: target_name,
          got: value_name,
          span: span.clone(),
        }
        .report(),
      );
    }
  }

  fn typecheck_cast(
    &mut self,
    expr_type: TypeId,
    target: &IgnisTypeSyntax,
    span: &Span,
  ) {
    let target_type = self.resolve_type_syntax(target);

    if self.types.is_error(&expr_type) || self.types.is_error(&target_type) {
      return;
    }

    let from_type = self.types.get(&expr_type).clone();
    let to_type = self.types.get(&target_type).clone();

    use ignis_type::types::Type;

    let is_valid = match (&from_type, &to_type) {
      (_, _) if self.types.is_numeric(&expr_type) && self.types.is_numeric(&target_type) => true,
      (Type::Boolean, _) if self.types.is_integer(&target_type) => true,
      (_, Type::Boolean) if self.types.is_integer(&expr_type) => true,
      (Type::Char, _) if self.types.is_integer(&target_type) => true,
      (_, Type::Char) if self.types.is_integer(&expr_type) => true,
      (Type::Pointer { .. }, Type::Reference { .. }) => true,
      (Type::Reference { .. }, Type::Pointer { .. }) => true,
      (Type::Pointer { .. }, Type::Pointer { .. }) => true,
      (Type::Reference { .. }, Type::Reference { .. }) => true,
      // Pointer <-> integer casts (for low-level memory manipulation)
      (Type::Pointer { .. }, _) if self.types.is_integer(&target_type) => true,
      (_, Type::Pointer { .. }) if self.types.is_integer(&expr_type) => true,
      (Type::Infer, _) => true,
      (Type::Reference { inner, .. }, _) if self.types.is_infer(inner) => true,
      (_, _) if self.types.types_equal(&expr_type, &target_type) => true,
      _ => false,
    };

    if !is_valid {
      let from_str = self.format_type_for_error(&expr_type);
      let to_str = self.format_type_for_error(&target_type);

      self.add_diagnostic(
        DiagnosticMessage::InvalidCast {
          from: from_str,
          to: to_str,
          span: span.clone(),
        }
        .report(),
      );
    }
  }

  fn typecheck_common_type(
    &self,
    a: &TypeId,
    b: &TypeId,
    _span: &Span,
  ) -> TypeId {
    if self.types.types_equal(a, b) {
      *a
    } else if matches!(self.types.get(a), Type::Never) {
      *b
    } else if matches!(self.types.get(b), Type::Never) {
      *a
    } else if self.types.is_numeric(a) && self.types.is_numeric(b) {
      if self.types.is_float(a) || self.types.is_float(b) {
        if matches!(self.types.get(a), ignis_type::types::Type::F64)
          || matches!(self.types.get(b), ignis_type::types::Type::F64)
        {
          self.types.f64()
        } else {
          self.types.f32()
        }
      } else if self.types.is_float(a) {
        *a
      } else {
        *b
      }
    } else {
      self.types.error()
    }
  }

  pub fn resolve_type_syntax(
    &mut self,
    ty: &IgnisTypeSyntax,
  ) -> TypeId {
    self.resolve_type_syntax_impl(ty, None)
  }

  pub fn resolve_type_syntax_with_span(
    &mut self,
    ty: &IgnisTypeSyntax,
    span: &Span,
  ) -> TypeId {
    self.resolve_type_syntax_impl(ty, Some(span))
  }

  fn resolve_type_syntax_impl(
    &mut self,
    ty: &IgnisTypeSyntax,
    span: Option<&Span>,
  ) -> TypeId {
    match ty {
      IgnisTypeSyntax::I8 => self.types.i8(),
      IgnisTypeSyntax::I16 => self.types.i16(),
      IgnisTypeSyntax::I32 => self.types.i32(),
      IgnisTypeSyntax::I64 => self.types.i64(),
      IgnisTypeSyntax::U8 => self.types.u8(),
      IgnisTypeSyntax::U16 => self.types.u16(),
      IgnisTypeSyntax::U32 => self.types.u32(),
      IgnisTypeSyntax::U64 => self.types.u64(),
      IgnisTypeSyntax::F32 => self.types.f32(),
      IgnisTypeSyntax::F64 => self.types.f64(),
      IgnisTypeSyntax::Str => self.types.str(),
      IgnisTypeSyntax::Boolean => self.types.boolean(),
      IgnisTypeSyntax::Atom => self.types.atom(),
      IgnisTypeSyntax::Void => self.types.void(),
      IgnisTypeSyntax::Char => self.types.char(),
      IgnisTypeSyntax::Implicit => self.types.infer(),
      IgnisTypeSyntax::Null => self.types.error(),
      IgnisTypeSyntax::Vector(inner, size) => {
        let inner_type = self.resolve_type_syntax_impl(inner, span);
        match size {
          Some(n) => self.types.vector(inner_type, *n),
          None => {
            if let Some(s) = span {
              self.add_diagnostic(DiagnosticMessage::DynamicVectorsNotSupported { span: s.clone() }.report());
            }
            self.types.error()
          },
        }
      },
      IgnisTypeSyntax::Tuple(elements) => {
        let element_types: Vec<_> = elements
          .iter()
          .map(|e| self.resolve_type_syntax_impl(e, span))
          .collect();
        self.types.tuple(element_types)
      },
      IgnisTypeSyntax::Callable(params, ret) => {
        let param_types: Vec<_> = params.iter().map(|p| self.resolve_type_syntax_impl(p, span)).collect();
        let ret_type = self.resolve_type_syntax_impl(ret, span);
        self.types.function(param_types, ret_type, false)
      },
      IgnisTypeSyntax::Pointer { inner, mutable } => {
        let inner_type = self.resolve_type_syntax_impl(inner, span);
        self.types.pointer(inner_type, *mutable)
      },
      IgnisTypeSyntax::Reference { inner, mutable } => {
        let inner_type = self.resolve_type_syntax_impl(inner, span);
        self.types.reference(inner_type, *mutable)
      },
      IgnisTypeSyntax::Named {
        symbol,
        span: name_span,
      } => {
        if let Some(def_id) = self.scopes.lookup_def(symbol).cloned() {
          self.mark_referenced(def_id);

          // Register span for hover/goto-definition on type references
          self.set_import_item_def(name_span, &def_id);

          // Check for type alias cycle
          if self.resolving_type_aliases.contains(&def_id) {
            let name = self.symbols.borrow().get(symbol).to_string();
            if let Some(s) = span {
              self.add_diagnostic(DiagnosticMessage::TypeAliasCycle { name, span: s.clone() }.report());
            }
            return self.types.error();
          }

          // Handle type parameter definitions - return Type::Param
          if let DefinitionKind::TypeParam(tp) = &self.defs.get(&def_id).kind.clone() {
            return self.types.param(tp.owner, tp.index);
          }

          // Handle type aliases
          if let DefinitionKind::TypeAlias(alias_def) = &self.defs.get(&def_id).kind.clone() {
            // Already resolved - return the target type
            if !self.types.is_error(&alias_def.target) {
              return alias_def.target;
            }

            // Not yet resolved - try to resolve inline using saved AST
            if let Some(syntax) = self.type_alias_syntax.get(&def_id).cloned() {
              self.resolving_type_aliases.insert(def_id);
              let resolved = self.resolve_type_syntax_impl(&syntax, span);

              // Update the definition with resolved type
              if let DefinitionKind::TypeAlias(ad) = &mut self.defs.get_mut(&def_id).kind {
                ad.target = resolved;
              }

              self.resolving_type_aliases.remove(&def_id);
              self.type_alias_syntax.remove(&def_id);
              return resolved;
            }

            // No AST available - shouldn't happen in normal flow
            return self.types.error();
          }

          *self.type_of(&def_id)
        } else {
          let name = self.symbols.borrow().get(symbol).to_string();
          if let Some(s) = span {
            self.add_diagnostic(Diagnostic::new(
              Severity::Error,
              format!("Undefined type '{}'", name),
              "I0043".to_string(),
              s.clone(),
            ));
          }
          self.types.error()
        }
      },
      IgnisTypeSyntax::Path { segments, args, .. } => {
        if segments.is_empty() {
          return self.types.error();
        }

        // Resolve the path to a definition
        let def_id = self.resolve_type_path_to_def(segments);
        let Some(def_id) = def_id else {
          let name = segments
            .iter()
            .map(|(sym, _)| self.symbols.borrow().get(sym).to_string())
            .collect::<Vec<_>>()
            .join("::");
          if let Some(s) = span {
            self.add_diagnostic(Diagnostic::new(
              Severity::Error,
              format!("Undefined type '{}'", name),
              "I0043".to_string(),
              s.clone(),
            ));
          }
          return self.types.error();
        };

        // Register spans for all segments for hover/goto-definition
        self.track_type_path_segment_spans(segments);

        // Check for type alias cycle
        if self.resolving_type_aliases.contains(&def_id) {
          let name = segments
            .iter()
            .map(|(sym, _)| self.symbols.borrow().get(sym).to_string())
            .collect::<Vec<_>>()
            .join("::");
          if let Some(s) = span {
            self.add_diagnostic(DiagnosticMessage::TypeAliasCycle { name, span: s.clone() }.report());
          }
          return self.types.error();
        }

        // Handle type parameter definitions
        if let DefinitionKind::TypeParam(tp) = &self.defs.get(&def_id).kind.clone() {
          // Type params cannot have type arguments
          if !args.is_empty() {
            if let Some(s) = span {
              let name = self.symbols.borrow().get(&self.defs.get(&def_id).name).to_string();
              self.add_diagnostic(DiagnosticMessage::TypeParamCannotHaveArgs { name, span: s.clone() }.report());
            }
            return self.types.error();
          }
          return self.types.param(tp.owner, tp.index);
        }

        // Handle type aliases
        if let DefinitionKind::TypeAlias(alias_def) = &self.defs.get(&def_id).kind.clone() {
          // Type aliases cannot currently have type arguments in Ignis
          if !args.is_empty() {
            if let Some(s) = span {
              let name = self.symbols.borrow().get(&self.defs.get(&def_id).name).to_string();
              self.add_diagnostic(DiagnosticMessage::TypeAliasCannotHaveArgs { name, span: s.clone() }.report());
            }
            return self.types.error();
          }

          // Already resolved - return the target type
          if !self.types.is_error(&alias_def.target) {
            return alias_def.target;
          }

          // Not yet resolved - try to resolve inline using saved AST
          if let Some(syntax) = self.type_alias_syntax.get(&def_id).cloned() {
            self.resolving_type_aliases.insert(def_id);
            let resolved = self.resolve_type_syntax_impl(&syntax, span);

            // Update the definition with resolved type
            if let DefinitionKind::TypeAlias(ad) = &mut self.defs.get_mut(&def_id).kind {
              ad.target = resolved;
            }

            self.resolving_type_aliases.remove(&def_id);
            self.type_alias_syntax.remove(&def_id);
            return resolved;
          }

          // No AST available - shouldn't happen in normal flow
          return self.types.error();
        }

        // Handle generic record/enum types with type arguments
        self.resolve_generic_type_with_args(def_id, args, span)
      },
      IgnisTypeSyntax::Applied { base, args } => {
        // Handle Named base directly to avoid prematurely expanding type aliases
        if let IgnisTypeSyntax::Named {
          symbol,
          span: name_span,
        } = base.as_ref()
          && let Some(def_id) = self.scopes.lookup_def(symbol).cloned()
        {
          // Register span for hover/goto-definition on type references
          self.set_import_item_def(name_span, &def_id);

          match &self.defs.get(&def_id).kind {
            DefinitionKind::TypeAlias(_) | DefinitionKind::Record(_) | DefinitionKind::Enum(_) => {
              return self.resolve_generic_type_with_args(def_id, args, span);
            },
            _ => {},
          }
        }

        let base_type = self.resolve_type_syntax_impl(base, span);

        if self.types.is_error(&base_type) {
          return self.types.error();
        }

        // The base must resolve to a Record or Enum type
        match self.types.get(&base_type).clone() {
          Type::Record(def_id) => self.resolve_generic_type_with_args(def_id, args, span),
          Type::Enum(def_id) => self.resolve_generic_type_with_args(def_id, args, span),
          _ => {
            if let Some(s) = span {
              self.add_diagnostic(
                DiagnosticMessage::TypeCannotBeParameterized {
                  type_name: self.format_type_for_error(&base_type),
                  span: s.clone(),
                }
                .report(),
              );
            }
            self.types.error()
          },
        }
      },
      _ => self.types.error(),
    }
  }

  fn is_mutable_expression(
    &self,
    expr: &ASTExpression,
  ) -> bool {
    match expr {
      ASTExpression::Variable(var) => {
        if let Some(def_id) = self.scopes.lookup_def(&var.name) {
          match &self.defs.get(def_id).kind {
            DefinitionKind::Variable(var_def) => var_def.mutable,
            DefinitionKind::Parameter(param_def) => param_def.mutable,
            _ => false,
          }
        } else {
          false
        }
      },
      ASTExpression::Dereference(deref) => {
        let node_id = &deref.inner;
        let node = self.ast.get(node_id);
        if let ASTNode::Expression(inner_expr) = node {
          if let Some(type_id) = self.lookup_type(node_id) {
            match self.types.get(type_id) {
              ignis_type::types::Type::Reference { mutable, .. } => return *mutable,
              ignis_type::types::Type::Pointer { mutable, .. } => return *mutable,
              _ => {},
            }
          }
          self.is_mutable_expression(inner_expr)
        } else {
          false
        }
      },
      ASTExpression::VectorAccess(access) => {
        let node = self.ast.get(&access.name);
        if let ASTNode::Expression(base_expr) = node {
          self.is_mutable_expression(base_expr)
        } else {
          false
        }
      },
      ASTExpression::Path(path) => {
        if let Some(last) = path.segments.last() {
          if let Some(def_id) = self.scopes.lookup_def(&last.name) {
            match &self.defs.get(def_id).kind {
              DefinitionKind::Variable(var_def) => var_def.mutable,
              DefinitionKind::Parameter(param_def) => param_def.mutable,
              _ => false,
            }
          } else {
            false
          }
        } else {
          false
        }
      },
      ASTExpression::MemberAccess(ma) => {
        // For field access (dot operator), check if the base object is mutable
        if ma.op == ignis_ast::expressions::member_access::ASTAccessOp::Dot {
          let node = self.ast.get(&ma.object);
          if let ASTNode::Expression(base_expr) = node {
            // Check if base is a mutable reference or pointer
            if let Some(type_id) = self.lookup_type(&ma.object) {
              match self.types.get(type_id) {
                ignis_type::types::Type::Reference { mutable, .. } => return *mutable,
                ignis_type::types::Type::Pointer { mutable, .. } => return *mutable,
                _ => {},
              }
            }
            // Otherwise, check if the base expression itself is mutable
            return self.is_mutable_expression(base_expr);
          }
        }
        false
      },
      ASTExpression::Grouped(grouped) => {
        let inner_node = self.ast.get(&grouped.expression);
        if let ASTNode::Expression(inner_expr) = inner_node {
          self.is_mutable_expression(inner_expr)
        } else {
          false
        }
      },
      _ => false,
    }
  }

  fn find_first_symbol_usage(
    &self,
    node_id: NodeId,
    symbol: SymbolId,
  ) -> Option<Span> {
    match self.ast.get(&node_id) {
      ASTNode::Expression(expr) => self.find_first_symbol_usage_in_expr(expr, symbol),
      ASTNode::Statement(stmt) => self.find_first_symbol_usage_in_stmt(stmt, symbol),
    }
  }

  fn find_first_symbol_usage_in_stmt(
    &self,
    stmt: &ASTStatement,
    symbol: SymbolId,
  ) -> Option<Span> {
    match stmt {
      ASTStatement::Expression(expr) => self.find_first_symbol_usage_in_expr(expr, symbol),
      ASTStatement::Variable(var) => var.value.and_then(|value| self.find_first_symbol_usage(value, symbol)),
      ASTStatement::Constant(const_) => const_
        .value
        .and_then(|value| self.find_first_symbol_usage(value, symbol)),
      ASTStatement::Block(block) => block
        .statements
        .iter()
        .find_map(|stmt_id| self.find_first_symbol_usage(*stmt_id, symbol)),
      ASTStatement::If(if_stmt) => self
        .find_first_symbol_usage(if_stmt.condition, symbol)
        .or_else(|| self.find_first_symbol_usage(if_stmt.then_block, symbol))
        .or_else(|| {
          if_stmt
            .else_block
            .and_then(|else_branch| self.find_first_symbol_usage(else_branch, symbol))
        }),
      ASTStatement::While(while_stmt) => self
        .find_first_symbol_usage(while_stmt.condition, symbol)
        .or_else(|| self.find_first_symbol_usage(while_stmt.body, symbol)),
      ASTStatement::For(for_stmt) => self
        .find_first_symbol_usage(for_stmt.initializer, symbol)
        .or_else(|| self.find_first_symbol_usage(for_stmt.condition, symbol))
        .or_else(|| self.find_first_symbol_usage(for_stmt.increment, symbol))
        .or_else(|| self.find_first_symbol_usage(for_stmt.body, symbol)),
      ASTStatement::ForOf(for_of) => self
        .find_first_symbol_usage(for_of.iter, symbol)
        .or_else(|| self.find_first_symbol_usage(for_of.body, symbol)),
      ASTStatement::Return(ret) => ret
        .expression
        .and_then(|expr| self.find_first_symbol_usage(expr, symbol)),
      ASTStatement::Function(func) => func
        .body
        .and_then(|body_id| self.find_first_symbol_usage(body_id, symbol)),
      ASTStatement::Extern(extern_stmt) => extern_stmt
        .items
        .iter()
        .find_map(|item| self.find_first_symbol_usage(*item, symbol)),
      ASTStatement::Namespace(ns) => ns
        .items
        .iter()
        .find_map(|item| self.find_first_symbol_usage(*item, symbol)),
      ASTStatement::Export(ignis_ast::statements::ASTExport::Declaration { decl, .. }) => {
        self.find_first_symbol_usage(*decl, symbol)
      },
      _ => None,
    }
  }

  fn find_first_symbol_usage_in_expr(
    &self,
    expr: &ASTExpression,
    symbol: SymbolId,
  ) -> Option<Span> {
    match expr {
      ASTExpression::Variable(var) => {
        if var.name == symbol {
          Some(var.span.clone())
        } else {
          None
        }
      },
      ASTExpression::Call(call) => self.find_first_symbol_usage(call.callee, symbol).or_else(|| {
        call
          .arguments
          .iter()
          .find_map(|arg| self.find_first_symbol_usage(*arg, symbol))
      }),
      ASTExpression::Binary(binary) => self
        .find_first_symbol_usage(binary.left, symbol)
        .or_else(|| self.find_first_symbol_usage(binary.right, symbol)),
      ASTExpression::Ternary(ternary) => self
        .find_first_symbol_usage(ternary.condition, symbol)
        .or_else(|| self.find_first_symbol_usage(ternary.then_expr, symbol))
        .or_else(|| self.find_first_symbol_usage(ternary.else_expr, symbol)),
      ASTExpression::Unary(unary) => self.find_first_symbol_usage(unary.operand, symbol),
      ASTExpression::Assignment(assign) => self
        .find_first_symbol_usage(assign.target, symbol)
        .or_else(|| self.find_first_symbol_usage(assign.value, symbol)),
      ASTExpression::Cast(cast) => self.find_first_symbol_usage(cast.expression, symbol),
      ASTExpression::Reference(reference) => self.find_first_symbol_usage(reference.inner, symbol),
      ASTExpression::Dereference(deref) => self.find_first_symbol_usage(deref.inner, symbol),
      ASTExpression::VectorAccess(access) => self
        .find_first_symbol_usage(access.name, symbol)
        .or_else(|| self.find_first_symbol_usage(access.index, symbol)),
      ASTExpression::Grouped(grouped) => self.find_first_symbol_usage(grouped.expression, symbol),
      ASTExpression::Vector(vector) => vector
        .items
        .iter()
        .find_map(|item| self.find_first_symbol_usage(*item, symbol)),
      ASTExpression::PostfixIncrement { expr, .. } | ASTExpression::PostfixDecrement { expr, .. } => {
        self.find_first_symbol_usage(*expr, symbol)
      },
      ASTExpression::MemberAccess(access) => self.find_first_symbol_usage(access.object, symbol),
      ASTExpression::RecordInit(record_init) => record_init
        .fields
        .iter()
        .find_map(|field| self.find_first_symbol_usage(field.value, symbol)),
      ASTExpression::BuiltinCall(builtin_call) => builtin_call
        .args
        .iter()
        .find_map(|arg| self.find_first_symbol_usage(*arg, symbol)),
      ASTExpression::Match(match_expr) => self.find_first_symbol_usage(match_expr.scrutinee, symbol).or_else(|| {
        match_expr.arms.iter().find_map(|arm| {
          arm
            .guard
            .and_then(|guard| self.find_first_symbol_usage(guard, symbol))
            .or_else(|| self.find_first_symbol_usage(arm.body, symbol))
        })
      }),
      ASTExpression::LetCondition(let_condition) => self.find_first_symbol_usage(let_condition.value, symbol),
      ASTExpression::Lambda(lambda) => {
        let body_id = match &lambda.body {
          ignis_ast::expressions::lambda::LambdaBody::Expression(id) => *id,
          ignis_ast::expressions::lambda::LambdaBody::Block(id) => *id,
        };
        self.find_first_symbol_usage(body_id, symbol)
      },
      ASTExpression::CaptureOverride(co) => self.find_first_symbol_usage(co.inner, symbol),
      ASTExpression::Pipe { lhs, rhs, .. } => self
        .find_first_symbol_usage(*lhs, symbol)
        .or_else(|| self.find_first_symbol_usage(*rhs, symbol)),
      ASTExpression::Literal(_) | ASTExpression::Path(_) | ASTExpression::PipePlaceholder { .. } => None,
    }
  }

  fn get_var_name_from_expr(
    &self,
    expr: &ASTExpression,
  ) -> String {
    match expr {
      ASTExpression::Variable(var) => self.get_symbol_name(&var.name),
      ASTExpression::Path(path) => {
        if let Some(last) = path.segments.last() {
          self.get_symbol_name(&last.name)
        } else {
          "<unknown>".to_string()
        }
      },
      ASTExpression::Dereference(deref) => {
        let inner_node = self.ast.get(&deref.inner);
        if let ASTNode::Expression(inner_expr) = inner_node {
          format!("(*{})", self.get_var_name_from_expr(inner_expr))
        } else {
          "<dereferenced value>".to_string()
        }
      },
      ASTExpression::VectorAccess(_) => "<vector element>".to_string(),
      ASTExpression::MemberAccess(ma) => {
        let base_node = self.ast.get(&ma.object);
        if let ASTNode::Expression(base_expr) = base_node {
          format!(
            "{}.{}",
            self.get_var_name_from_expr(base_expr),
            self.get_symbol_name(&ma.member)
          )
        } else {
          format!("<object>.{}", self.get_symbol_name(&ma.member))
        }
      },
      ASTExpression::Grouped(grouped) => {
        let inner_node = self.ast.get(&grouped.expression);
        if let ASTNode::Expression(inner_expr) = inner_node {
          self.get_var_name_from_expr(inner_expr)
        } else {
          "<grouped expression>".to_string()
        }
      },
      _ => "<expression>".to_string(),
    }
  }

  fn is_lvalue(
    &self,
    expr: &ASTExpression,
  ) -> bool {
    matches!(
      expr,
      ASTExpression::Variable(_)
        | ASTExpression::Path(_)
        | ASTExpression::Dereference(_)
        | ASTExpression::VectorAccess(_)
    )
  }

  fn format_type_for_error(
    &self,
    type_id: &ignis_type::types::TypeId,
  ) -> String {
    use ignis_type::types::Type;

    let ty = self.types.get(type_id);
    match ty {
      Type::I8 => "i8".to_string(),
      Type::I16 => "i16".to_string(),
      Type::I32 => "i32".to_string(),
      Type::I64 => "i64".to_string(),
      Type::U8 => "u8".to_string(),
      Type::U16 => "u16".to_string(),
      Type::U32 => "u32".to_string(),
      Type::U64 => "u64".to_string(),
      Type::F32 => "f32".to_string(),
      Type::F64 => "f64".to_string(),
      Type::Boolean => "bool".to_string(),
      Type::Char => "char".to_string(),
      Type::Str => "str".to_string(),
      Type::Atom => "atom".to_string(),
      Type::Void => "void".to_string(),
      Type::Never => "never".to_string(),
      Type::Infer => "infer".to_string(),
      Type::NullPtr => "null".to_string(),
      Type::Error => "error".to_string(),
      Type::Pointer { inner, mutable } => {
        if *mutable {
          format!("*mut {}", self.format_type_for_error(inner))
        } else {
          format!("*{}", self.format_type_for_error(inner))
        }
      },
      Type::Reference { inner, mutable } => {
        if *mutable {
          format!("&mut {}", self.format_type_for_error(inner))
        } else {
          format!("&{}", self.format_type_for_error(inner))
        }
      },
      Type::Vector { element, size } => {
        format!("[{}; {}]", self.format_type_for_error(element), size)
      },
      Type::Tuple(elements) => {
        let elem_strs: Vec<_> = elements.iter().map(|e| self.format_type_for_error(e)).collect();
        format!("({})", elem_strs.join(", "))
      },
      Type::Function {
        params,
        ret,
        is_variadic,
      } => {
        let param_strs: Vec<_> = params.iter().map(|p| self.format_type_for_error(p)).collect();
        let variadic = if *is_variadic { ", ..." } else { "" };
        format!(
          "fn({}{}) -> {}",
          param_strs.join(", "),
          variadic,
          self.format_type_for_error(ret)
        )
      },
      Type::Record(def_id) => {
        let def = self.defs.get(def_id);
        self.symbols.borrow().get(&def.name).to_string()
      },
      Type::Enum(def_id) => {
        let def = self.defs.get(def_id);
        self.symbols.borrow().get(&def.name).to_string()
      },
      Type::Param { owner, index } => {
        // Try to get the type param name from the owner's definition
        let owner_def = self.defs.get(owner);
        let type_params = match &owner_def.kind {
          DefinitionKind::Function(fd) => &fd.type_params,
          DefinitionKind::Record(rd) => &rd.type_params,
          DefinitionKind::Method(md) => &md.type_params,
          DefinitionKind::Enum(ed) => &ed.type_params,
          _ => return format!("T{}", index),
        };
        if let Some(param_def_id) = type_params.get(*index as usize) {
          self.symbols.borrow().get(&self.defs.get(param_def_id).name).to_string()
        } else {
          format!("T{}", index)
        }
      },
      Type::Instance { generic, args } => {
        let name = self.symbols.borrow().get(&self.defs.get(generic).name).to_string();
        let arg_strs: Vec<_> = args.iter().map(|a| self.format_type_for_error(a)).collect();
        format!("{}<{}>", name, arg_strs.join(", "))
      },
      Type::InferVar(id) => format!("?{}", id.0),
      Type::Unknown => "unknown".to_string(),
    }
  }

  pub(crate) fn get_definition_type(
    &mut self,
    def_id: &ignis_type::definition::DefinitionId,
  ) -> TypeId {
    match &self.defs.get(def_id).kind {
      DefinitionKind::Function(func_def) => {
        let param_types: Vec<TypeId> = func_def.params.iter().map(|param_id| *self.type_of(param_id)).collect();

        self
          .types
          .function(param_types, func_def.return_type, func_def.is_variadic)
      },
      _ => {
        let ty = *self.type_of(def_id);
        if self.types.is_infer_var(&ty) {
          self.infer_ctx.resolve(ty, &mut self.types)
        } else {
          ty
        }
      },
    }
  }

  fn resolve_for_of_binding_type(
    &mut self,
    binding: &ignis_ast::statements::ForOfBinding,
    element_type: TypeId,
    iter_node: &NodeId,
  ) -> TypeId {
    match &binding.type_annotation {
      None => {
        if self.types.is_copy(&element_type) {
          element_type
        } else {
          self.add_diagnostic(
            DiagnosticMessage::ForOfRequiresCopyOrRef {
              element_type: self.format_type_for_error(&element_type),
              span: binding.span.clone(),
            }
            .report(),
          );
          self.types.error()
        }
      },
      Some(IgnisTypeSyntax::Reference { inner, mutable }) => {
        let inner_type = self.resolve_type_syntax(inner);

        if !self.types.is_error(&inner_type)
          && !self.types.is_error(&element_type)
          && !self.types.types_equal(&inner_type, &element_type)
        {
          self.add_diagnostic(
            DiagnosticMessage::AssignmentTypeMismatch {
              expected: self.format_type_for_error(&element_type),
              got: self.format_type_for_error(&inner_type),
              span: binding.span.clone(),
            }
            .report(),
          );
        }

        if *mutable {
          self.validate_mutable_iter_for_mut_ref(iter_node, &binding.span);
        }

        self.types.reference(element_type, *mutable)
      },
      Some(type_syntax) => {
        // Annotated by value: T
        let annotated_type = self.resolve_type_syntax(type_syntax);

        if !self.types.is_error(&annotated_type)
          && !self.types.is_error(&element_type)
          && !self.types.types_equal(&annotated_type, &element_type)
        {
          self.add_diagnostic(
            DiagnosticMessage::AssignmentTypeMismatch {
              expected: self.format_type_for_error(&element_type),
              got: self.format_type_for_error(&annotated_type),
              span: binding.span.clone(),
            }
            .report(),
          );
        }

        if !self.types.is_copy(&element_type) {
          self.add_diagnostic(
            DiagnosticMessage::ForOfRequiresCopyOrRef {
              element_type: self.format_type_for_error(&element_type),
              span: binding.span.clone(),
            }
            .report(),
          );
        }

        element_type
      },
    }
  }

  /// Checks whether a record type is iterable for `for..of`.
  ///
  /// A record is iterable if it has:
  /// - A `data` field of pointer type (`*T` or `*mut T`)
  /// - A `length` field of type `u64`
  ///
  /// Returns the element type `T` from the data pointer, or `None`.
  fn extract_record_iterable_element_type(
    &self,
    def_id: &DefinitionId,
  ) -> Option<TypeId> {
    let rd = match &self.defs.get(def_id).kind {
      DefinitionKind::Record(rd) => rd.clone(),
      _ => return None,
    };

    let sym = self.symbols.borrow();
    let data_sym = *sym.map.get("data")?;
    let length_sym = *sym.map.get("length")?;

    let mut data_element_type = None;
    let mut has_length = false;

    for field in &rd.fields {
      if field.name == data_sym {
        if let Type::Pointer { inner, .. } = self.types.get(&field.type_id) {
          data_element_type = Some(*inner);
        }
      } else if field.name == length_sym && self.types.types_equal(&field.type_id, &self.types.u64()) {
        has_length = true;
      }
    }

    if has_length { data_element_type } else { None }
  }

  fn validate_mutable_iter_for_mut_ref(
    &mut self,
    iter_node: &NodeId,
    span: &Span,
  ) {
    let node = self.ast.get(iter_node);
    if let ASTNode::Expression(expr) = node
      && !self.is_mutable_expression(expr)
    {
      self.add_diagnostic(DiagnosticMessage::ForOfMutRequiresMutableIter { span: span.clone() }.report());
    }
  }

  // ========================================================================
  // Generic Type Resolution Helpers
  // ========================================================================

  /// Resolve a type path (list of segments) to a definition ID.
  fn resolve_type_path_to_def(
    &self,
    segments: &[(ignis_type::symbol::SymbolId, Span)],
  ) -> Option<ignis_type::definition::DefinitionId> {
    if segments.is_empty() {
      return None;
    }

    // Start with first segment in scope
    let (first_sym, _) = &segments[0];
    let mut current_def = self.scopes.lookup_def(first_sym).cloned()?;

    // Walk through remaining segments
    for (segment_sym, _) in segments.iter().skip(1) {
      match &self.defs.get(&current_def).kind {
        DefinitionKind::Namespace(ns_def) => {
          if let Some(entry) = self.namespaces.lookup_def(ns_def.namespace_id, segment_sym) {
            if let Some(def_id) = entry.as_single() {
              current_def = *def_id;
            } else {
              // Overloaded functions cannot be used as types
              return None;
            }
          } else {
            return None;
          }
        },
        _ => return None,
      }
    }

    Some(current_def)
  }

  /// Register spans for each segment in a type path for hover/goto-definition.
  fn track_type_path_segment_spans(
    &mut self,
    segments: &[(ignis_type::symbol::SymbolId, Span)],
  ) {
    if segments.is_empty() {
      return;
    }

    // Register first segment
    let (first_sym, first_span) = &segments[0];
    let Some(first_def) = self.scopes.lookup_def(first_sym).cloned() else {
      return;
    };
    self.mark_referenced(first_def);
    self.set_import_item_def(first_span, &first_def);

    let mut current_def = first_def;

    // Walk through remaining segments
    for (segment_sym, segment_span) in segments.iter().skip(1) {
      let def_kind = self.defs.get(&current_def).kind.clone();

      match def_kind {
        DefinitionKind::Namespace(ns_def) => {
          let entry = self.namespaces.lookup_def(ns_def.namespace_id, segment_sym).cloned();

          if let Some(entry) = entry {
            if let Some(def_id) = entry.as_single() {
              self.mark_referenced(*def_id);
              self.set_import_item_def(segment_span, def_id);
              current_def = *def_id;
            } else {
              return;
            }
          } else {
            return;
          }
        },
        _ => return,
      }
    }
  }

  /// Resolve a generic type with type arguments.
  /// Returns Type::Instance for records/enums, or substituted target for type aliases.
  fn resolve_generic_type_with_args(
    &mut self,
    def_id: ignis_type::definition::DefinitionId,
    args: &[IgnisTypeSyntax],
    span: Option<&Span>,
  ) -> TypeId {
    let def = self.defs.get(&def_id);
    let type_name = self.symbols.borrow().get(&def.name).to_string();

    let (type_params_len, is_type_alias) = match &def.kind {
      DefinitionKind::Record(rd) => (rd.type_params.len(), false),
      DefinitionKind::Enum(ed) => (ed.type_params.len(), false),
      DefinitionKind::TypeAlias(ta) => (ta.type_params.len(), true),
      _ => {
        // Not a generic-capable type
        if !args.is_empty() {
          if let Some(s) = span {
            self.add_diagnostic(
              DiagnosticMessage::TypeCannotBeParameterized {
                type_name,
                span: s.clone(),
              }
              .report(),
            );
          }
          return self.types.error();
        }
        return *self.type_of(&def_id);
      },
    };

    // Check if type arguments are needed
    if type_params_len == 0 {
      // Non-generic type
      if !args.is_empty() {
        if let Some(s) = span {
          self.add_diagnostic(
            DiagnosticMessage::WrongNumberOfTypeArgs {
              expected: 0,
              got: args.len(),
              type_name,
              span: s.clone(),
            }
            .report(),
          );
        }
        return self.types.error();
      }
      return *self.type_of(&def_id);
    }

    // Generic type - resolve arguments
    if args.is_empty() {
      // Generic type used without type arguments
      // This might be valid in some contexts (e.g., within the generic definition itself)
      // For now, return the base type - monomorphization will catch unresolved instances
      return *self.type_of(&def_id);
    }

    // Check arity
    if args.len() != type_params_len {
      if let Some(s) = span {
        self.add_diagnostic(
          DiagnosticMessage::WrongNumberOfTypeArgs {
            expected: type_params_len,
            got: args.len(),
            type_name,
            span: s.clone(),
          }
          .report(),
        );
      }
      return self.types.error();
    }

    let resolved_args: Vec<TypeId> = args
      .iter()
      .map(|arg| self.resolve_type_syntax_impl(arg, span))
      .collect();

    if is_type_alias {
      let target_type = *self.type_of(&def_id);
      let subst = Substitution::for_generic(def_id, &resolved_args);
      return self.types.substitute(target_type, &subst);
    }

    self.types.instance(def_id, resolved_args)
  }

  // ========================================================================
  // Type Parameter Scope Management (Typeck)
  // ========================================================================

  /// Pushes a generic scope and registers type params for an owner definition.
  /// Used when typechecking function bodies to make type params visible.
  fn enter_type_params_scope(
    &mut self,
    owner_def_id: &ignis_type::definition::DefinitionId,
  ) {
    let type_params = match &self.defs.get(owner_def_id).kind {
      DefinitionKind::Record(rd) => rd.type_params.clone(),
      DefinitionKind::Enum(ed) => ed.type_params.clone(),
      DefinitionKind::Function(fd) => fd.type_params.clone(),
      DefinitionKind::Method(md) => md.type_params.clone(),
      DefinitionKind::TypeAlias(ta) => ta.type_params.clone(),
      _ => Vec::new(),
    };

    if type_params.is_empty() {
      return;
    }

    self.scopes.push(ScopeKind::Generic);

    for param_id in &type_params {
      let name = self.defs.get(param_id).name;
      let _ = self.scopes.define(&name, param_id, false);
    }
  }

  /// Pops the generic scope if the owner definition has type params.
  fn exit_type_params_scope(
    &mut self,
    owner_def_id: &ignis_type::definition::DefinitionId,
  ) {
    let has_type_params = match &self.defs.get(owner_def_id).kind {
      DefinitionKind::Record(rd) => !rd.type_params.is_empty(),
      DefinitionKind::Enum(ed) => !ed.type_params.is_empty(),
      DefinitionKind::Function(fd) => !fd.type_params.is_empty(),
      DefinitionKind::Method(md) => !md.type_params.is_empty(),
      DefinitionKind::TypeAlias(ta) => !ta.type_params.is_empty(),
      _ => false,
    };

    if has_type_params {
      self.scopes.pop();
    }
  }

  // ========================================================================
  // Lang Trait Validation
  // ========================================================================

  fn validate_lang_trait_methods(
    &mut self,
    type_def_id: &DefinitionId,
    type_span: &Span,
  ) {
    let type_def = self.defs.get(type_def_id);
    let type_name = self.get_symbol_name(&type_def.name);

    // Clone to release borrow on self.defs
    let (lang_traits, instance_methods, type_id, is_enum, fields, variants) = match &type_def.kind {
      DefinitionKind::Record(rd) => {
        let fields: Vec<_> = rd.fields.iter().map(|f| (f.name, f.type_id, f.span.clone())).collect();

        // Generic records need Instance(def, [Param(0), ...]) so clone/drop signatures match
        let effective_type = if rd.type_params.is_empty() {
          rd.type_id
        } else {
          let params: Vec<TypeId> = rd
            .type_params
            .iter()
            .map(|&tp_def| {
              let tp_kind = &self.defs.get(&tp_def).kind;
              if let DefinitionKind::TypeParam(tp) = tp_kind {
                self.types.param(tp.owner, tp.index)
              } else {
                panic!("expected TypeParam in record type_params");
              }
            })
            .collect();
          self.types.instance(*type_def_id, params)
        };

        (
          rd.lang_traits,
          Some(rd.instance_methods.clone()),
          effective_type,
          false,
          fields,
          Vec::new(),
        )
      },
      DefinitionKind::Enum(ed) => {
        let effective_type = if ed.type_params.is_empty() {
          ed.type_id
        } else {
          let params: Vec<TypeId> = ed
            .type_params
            .iter()
            .map(|&tp_def| {
              let tp_kind = &self.defs.get(&tp_def).kind;
              if let DefinitionKind::TypeParam(tp) = tp_kind {
                self.types.param(tp.owner, tp.index)
              } else {
                panic!("expected TypeParam in enum type_params");
              }
            })
            .collect();
          self.types.instance(*type_def_id, params)
        };

        (
          ed.lang_traits,
          Some(ed.instance_methods.clone()),
          effective_type,
          true,
          Vec::new(),
          ed.variants.clone(),
        )
      },
      _ => return,
    };

    if !lang_traits.drop && !lang_traits.clone && !lang_traits.copy {
      return;
    }

    if lang_traits.drop {
      self.validate_lang_trait_method(
        instance_methods.as_ref(),
        "Drop",
        "drop",
        true,
        type_id,
        true,
        &type_name,
        type_span,
      );
    }

    if lang_traits.clone {
      self.validate_lang_trait_method(
        instance_methods.as_ref(),
        "Clone",
        "clone",
        false,
        type_id,
        false,
        &type_name,
        type_span,
      );
    }

    if lang_traits.copy {
      if is_enum {
        self.validate_copy_structural_enum(&type_name, &variants, type_span);
      } else {
        self.validate_copy_structural(&type_name, &fields, type_span);
      }
    }
  }

  fn validate_copy_structural(
    &mut self,
    type_name: &str,
    fields: &[(ignis_type::symbol::SymbolId, ignis_type::types::TypeId, Span)],
    type_span: &Span,
  ) {
    for (field_sym, field_ty, _field_span) in fields {
      if !self.types.is_copy_with_defs(field_ty, &self.defs) {
        let field_name = self.get_symbol_name(field_sym);
        let field_type = self.format_type_for_error(field_ty);

        self.add_diagnostic(
          DiagnosticMessage::CopyOnNonCopyField {
            type_name: type_name.to_string(),
            field_name,
            field_type,
            span: type_span.clone(),
          }
          .report(),
        );
      }
    }
  }

  fn validate_copy_structural_enum(
    &mut self,
    type_name: &str,
    variants: &[ignis_type::definition::EnumVariantDef],
    type_span: &Span,
  ) {
    for variant in variants {
      for (i, &payload_ty) in variant.payload.iter().enumerate() {
        if !self.types.is_copy_with_defs(&payload_ty, &self.defs) {
          let variant_name = self.get_symbol_name(&variant.name);
          let payload_type = self.format_type_for_error(&payload_ty);

          self.add_diagnostic(
            DiagnosticMessage::CopyOnNonCopyVariantPayload {
              type_name: type_name.to_string(),
              variant_name,
              payload_type,
              payload_index: i,
              span: type_span.clone(),
            }
            .report(),
          );
        }
      }
    }
  }

  #[allow(clippy::too_many_arguments)]
  fn validate_lang_trait_method(
    &mut self,
    instance_methods: Option<&HashMap<ignis_type::symbol::SymbolId, SymbolEntry>>,
    trait_name: &str,
    method_name: &str,
    expect_mut_self: bool,
    type_id: TypeId,
    expect_void_return: bool,
    type_name: &str,
    type_span: &Span,
  ) {
    let method_sym = self.symbols.borrow_mut().intern(method_name);

    let Some(methods) = instance_methods else {
      self.add_diagnostic(
        DiagnosticMessage::LangTraitMissingMethod {
          trait_name: trait_name.to_string(),
          method_name: method_name.to_string(),
          type_name: type_name.to_string(),
          span: type_span.clone(),
        }
        .report(),
      );
      return;
    };

    let Some(entry) = methods.get(&method_sym) else {
      self.add_diagnostic(
        DiagnosticMessage::LangTraitMissingMethod {
          trait_name: trait_name.to_string(),
          method_name: method_name.to_string(),
          type_name: type_name.to_string(),
          span: type_span.clone(),
        }
        .report(),
      );
      return;
    };

    // Only checks the first overload; trait methods should not be overloaded
    let method_def_id = match entry {
      SymbolEntry::Single(id) => *id,
      SymbolEntry::Overload(group) => {
        if let Some(id) = group.first().copied() {
          id
        } else {
          return;
        }
      },
    };

    let method_def = self.defs.get(&method_def_id);
    let method_span = method_def.span.clone();

    let DefinitionKind::Method(md) = &method_def.kind else {
      return;
    };

    let self_mutable = md.self_mutable;
    let params = md.params.clone();
    let return_type = md.return_type;

    // md.params includes self at index 0 after typechecking
    let has_wrong_self_mut = self_mutable != expect_mut_self;
    let has_extra_params = params.len() > 1;

    let return_ok = if expect_void_return {
      return_type == self.types.void()
    } else {
      return_type == type_id
    };

    if has_wrong_self_mut || has_extra_params || !return_ok {
      let self_prefix = if expect_mut_self { "&mut self" } else { "&self" };

      let return_str = if expect_void_return {
        "void".to_string()
      } else {
        type_name.to_string()
      };

      let expected = format!("{}({}): {}", method_name, self_prefix, return_str);

      let got_self = if self_mutable { "&mut self" } else { "&self" };
      let got_return = self.format_type_for_error(&return_type);

      let extra_params = if params.is_empty() { &[][..] } else { &params[1..] };
      let got_params = if extra_params.is_empty() {
        got_self.to_string()
      } else {
        let param_types: Vec<String> = extra_params
          .iter()
          .map(|p| {
            let pdef = self.defs.get(p);
            if let DefinitionKind::Parameter(pd) = &pdef.kind {
              self.format_type_for_error(&pd.type_id)
            } else {
              "?".to_string()
            }
          })
          .collect();
        format!("{}, {}", got_self, param_types.join(", "))
      };

      let got = format!("{}({}): {}", method_name, got_params, got_return);

      self.add_diagnostic(
        DiagnosticMessage::LangTraitInvalidSignature {
          trait_name: trait_name.to_string(),
          method_name: method_name.to_string(),
          expected,
          got,
          span: method_span,
        }
        .report(),
      );
    }
  }

  // ========================================================================
  // Overload Resolution
  // ========================================================================

  fn resolve_overload(
    &mut self,
    candidates: &[DefinitionId],
    arg_types: &[TypeId],
    span: &Span,
    subst: Option<&Substitution>,
  ) -> Result<DefinitionId, ()> {
    if candidates.is_empty() {
      return Err(());
    }

    if candidates.len() == 1 {
      return Ok(candidates[0]);
    }

    let arity = arg_types.len();

    let arity_matches: Vec<_> = candidates
      .iter()
      .filter(|&&def_id| self.check_arity(def_id, arity))
      .copied()
      .collect();

    let mut scored: Vec<(DefinitionId, u32)> = Vec::new();

    for &def_id in &arity_matches {
      if let Some(score) = self.try_match_signature(def_id, arg_types, subst) {
        scored.push((def_id, score));
      }
    }

    if scored.is_empty() {
      self.emit_no_overload_error(candidates, arg_types, span);
      return Err(());
    }

    scored.sort_by_key(|(_, score)| *score);

    let best_score = scored[0].1;
    let best_matches: Vec<_> = scored
      .iter()
      .filter(|(_, score)| *score == best_score)
      .map(|(def_id, _)| *def_id)
      .collect();

    if best_matches.len() > 1 {
      self.emit_ambiguous_overload_error(&best_matches, arg_types, span);
      return Err(());
    }

    Ok(best_matches[0])
  }

  fn check_arity(
    &self,
    def_id: DefinitionId,
    arg_count: usize,
  ) -> bool {
    match &self.defs.get(&def_id).kind {
      DefinitionKind::Function(fd) => fd.params.len() == arg_count,
      DefinitionKind::Method(md) => {
        let start = self.method_param_start(md);
        let param_count = md.params.len().saturating_sub(start);
        param_count == arg_count
      },
      _ => false,
    }
  }

  fn try_match_signature(
    &mut self,
    def_id: DefinitionId,
    arg_types: &[TypeId],
    subst: Option<&Substitution>,
  ) -> Option<u32> {
    let param_types = self.get_param_types(def_id, subst);

    let has_generics = self.has_type_params(def_id);
    let mut inferred = Substitution::new();

    let mut coercion_cost: u32 = 0;

    for (param_ty, arg_ty) in param_types.iter().zip(arg_types.iter()) {
      if self.types.types_equal(param_ty, arg_ty) {
        continue;
      }

      // Non-equal but assignable (future: implicit coercions) — match with extra cost.
      if self.types.is_assignable(param_ty, arg_ty) {
        coercion_cost += 2;
        continue;
      }

      if has_generics {
        if !self.try_infer_type_param(param_ty, arg_ty, &mut inferred) {
          return None;
        }
      } else {
        return None;
      }
    }

    let base_cost = if inferred.is_empty() { 0 } else { 1 };
    Some(base_cost + coercion_cost)
  }

  fn get_param_types(
    &mut self,
    def_id: DefinitionId,
    subst: Option<&Substitution>,
  ) -> Vec<TypeId> {
    match &self.defs.get(&def_id).kind {
      DefinitionKind::Function(fd) => fd
        .params
        .iter()
        .map(|p| {
          let ty = *self.defs.type_of(p);
          subst.map_or(ty, |s| self.types.substitute(ty, s))
        })
        .collect(),
      DefinitionKind::Method(md) => {
        let start = self.method_param_start(md);
        md.params[start..]
          .iter()
          .map(|p| {
            let ty = *self.defs.type_of(p);
            subst.map_or(ty, |s| self.types.substitute(ty, s))
          })
          .collect()
      },
      _ => Vec::new(),
    }
  }

  fn has_type_params(
    &self,
    def_id: DefinitionId,
  ) -> bool {
    match &self.defs.get(&def_id).kind {
      DefinitionKind::Function(fd) => !fd.type_params.is_empty(),
      DefinitionKind::Method(md) => !md.type_params.is_empty(),
      _ => false,
    }
  }

  fn try_infer_type_param(
    &self,
    param_ty: &TypeId,
    arg_ty: &TypeId,
    subst: &mut Substitution,
  ) -> bool {
    self.types.unify_for_inference(*param_ty, *arg_ty, subst)
  }

  fn method_param_start(
    &self,
    md: &ignis_type::definition::MethodDefinition,
  ) -> usize {
    if md.is_static {
      return 0;
    }

    let Some(first_param) = md.params.first() else {
      return 0;
    };

    let symbols = self.symbols.borrow();
    let first_name = symbols.get(&self.defs.get(first_param).name);
    if first_name == "self" { 1 } else { 0 }
  }

  fn emit_no_overload_error(
    &mut self,
    candidates: &[DefinitionId],
    arg_types: &[TypeId],
    span: &Span,
  ) {
    let available_signatures: Vec<String> = candidates.iter().map(|&def_id| self.format_signature(def_id)).collect();

    let arg_types_str: Vec<String> = arg_types.iter().map(|ty| self.format_type_for_error(ty)).collect();

    let name = if let Some(first_def) = candidates.first() {
      self.get_symbol_name(&self.defs.get(first_def).name)
    } else {
      "unknown".to_string()
    };

    self.add_diagnostic(
      DiagnosticMessage::NoOverloadMatches {
        name,
        available_signatures,
        arg_types: arg_types_str,
        span: span.clone(),
      }
      .report(),
    );
  }

  fn emit_ambiguous_overload_error(
    &mut self,
    matching: &[DefinitionId],
    arg_types: &[TypeId],
    span: &Span,
  ) {
    let matching_signatures: Vec<String> = matching.iter().map(|&def_id| self.format_signature(def_id)).collect();

    let arg_types_str: Vec<String> = arg_types.iter().map(|ty| self.format_type_for_error(ty)).collect();

    let name = if let Some(first_def) = matching.first() {
      self.get_symbol_name(&self.defs.get(first_def).name)
    } else {
      "unknown".to_string()
    };

    self.add_diagnostic(
      DiagnosticMessage::AmbiguousOverload {
        name,
        matching_signatures,
        arg_types: arg_types_str,
        span: span.clone(),
      }
      .report(),
    );
  }

  fn format_signature(
    &self,
    def_id: DefinitionId,
  ) -> String {
    match &self.defs.get(&def_id).kind {
      DefinitionKind::Function(fd) => {
        let param_types: Vec<String> = fd
          .params
          .iter()
          .map(|p| self.format_type_for_error(self.defs.type_of(p)))
          .collect();
        let ret_type = self.format_type_for_error(&fd.return_type);
        format!("({}): {}", param_types.join(", "), ret_type)
      },
      DefinitionKind::Method(md) => {
        let start = self.method_param_start(md);
        let param_types: Vec<String> = md.params[start..]
          .iter()
          .map(|p| self.format_type_for_error(self.defs.type_of(p)))
          .collect();
        let ret_type = self.format_type_for_error(&md.return_type);
        format!("({}): {}", param_types.join(", "), ret_type)
      },
      _ => "<unknown>".to_string(),
    }
  }

  fn check_duplicate_signatures(&mut self) {
    let overload_groups: Vec<(ignis_type::symbol::SymbolId, Vec<DefinitionId>)> = self
      .scopes
      .all_scopes()
      .flat_map(|scope| {
        scope.symbols.iter().filter_map(|(name, entry)| {
          if let SymbolEntry::Overload(group) = entry {
            Some((*name, group.clone()))
          } else {
            None
          }
        })
      })
      .collect();

    for (name, group) in overload_groups {
      self.check_overload_group(&name, &group);
    }

    let mut method_groups: Vec<(ignis_type::symbol::SymbolId, Vec<DefinitionId>)> = Vec::new();

    for (_, def) in self.defs.iter() {
      match &def.kind {
        DefinitionKind::Record(rd) => {
          for (name, entry) in &rd.instance_methods {
            if let SymbolEntry::Overload(group) = entry {
              method_groups.push((*name, group.clone()));
            }
          }
          for (name, entry) in &rd.static_methods {
            if let SymbolEntry::Overload(group) = entry {
              method_groups.push((*name, group.clone()));
            }
          }
        },
        DefinitionKind::Enum(ed) => {
          for (name, entry) in &ed.static_methods {
            if let SymbolEntry::Overload(group) = entry {
              method_groups.push((*name, group.clone()));
            }
          }
        },
        _ => {},
      }
    }

    for (name, group) in method_groups {
      self.check_overload_group(&name, &group);
    }
  }

  fn check_overload_group(
    &mut self,
    name: &ignis_type::symbol::SymbolId,
    group: &[DefinitionId],
  ) {
    for (i, &def1) in group.iter().enumerate() {
      for &def2 in group.iter().skip(i + 1) {
        let def1_ns = self.defs.get(&def1).owner_namespace;
        let def2_ns = self.defs.get(&def2).owner_namespace;

        if def1_ns != def2_ns {
          continue;
        }

        if self.signatures_are_identical(def1, def2) {
          let sig = self.format_signature(def1);
          let def2_span = self.defs.get(&def2).span.clone();
          let symbol_name = self.get_symbol_name(name);

          self.add_diagnostic(
            DiagnosticMessage::DuplicateOverload {
              name: symbol_name,
              signature: sig,
              span: def2_span,
            }
            .report(),
          );
        }
      }
    }
  }

  fn signatures_are_identical(
    &self,
    def1: DefinitionId,
    def2: DefinitionId,
  ) -> bool {
    let (params1, ret1, start1) = match &self.defs.get(&def1).kind {
      DefinitionKind::Function(f) => (&f.params, &f.return_type, 0),
      DefinitionKind::Method(m) => (&m.params, &m.return_type, self.method_param_start(m)),
      _ => return false,
    };

    let (params2, ret2, start2) = match &self.defs.get(&def2).kind {
      DefinitionKind::Function(f) => (&f.params, &f.return_type, 0),
      DefinitionKind::Method(m) => (&m.params, &m.return_type, self.method_param_start(m)),
      _ => return false,
    };

    if params1.len().saturating_sub(start1) != params2.len().saturating_sub(start2) {
      return false;
    }

    if !self.types.types_equal(ret1, ret2) {
      return false;
    }

    for (p1, p2) in params1[start1..].iter().zip(params2[start2..].iter()) {
      let t1 = self.defs.type_of(p1);
      let t2 = self.defs.type_of(p2);
      if !self.types.types_equal(t1, t2) {
        return false;
      }
    }

    true
  }

  fn register_extension_method(
    &mut self,
    def_id: &DefinitionId,
    func_span: &Span,
  ) {
    let func_def = match &self.defs.get(def_id).kind {
      DefinitionKind::Function(fd) => fd.clone(),
      _ => return,
    };

    let ext_type_name = match func_def.attrs.iter().find_map(|a| {
      if let FunctionAttr::Extension { type_name, .. } = a {
        Some(type_name.clone())
      } else {
        None
      }
    }) {
      Some(name) => name,
      None => return,
    };

    let target_type_id = match self.types.type_id_from_name(&ext_type_name) {
      Some(id) => id,
      None => {
        self.add_diagnostic(
          DiagnosticMessage::ExtensionInvalidTargetType {
            type_name: ext_type_name,
            span: func_span.clone(),
          }
          .report(),
        );
        return;
      },
    };

    if func_def.params.is_empty() {
      self.add_diagnostic(
        DiagnosticMessage::ExtensionRequiresParameter {
          span: func_span.clone(),
        }
        .report(),
      );
      return;
    }

    let first_param_type = self.get_definition_type(&func_def.params[0]);
    let receiver_ok = self.types.types_equal(&first_param_type, &target_type_id) || {
      match self.types.get(&first_param_type).clone() {
        Type::Reference { inner, .. } => self.types.types_equal(&inner, &target_type_id),
        _ => false,
      }
    };

    if !receiver_ok {
      let expected = self.format_type_for_error(&target_type_id);
      let got = self.format_type_for_error(&first_param_type);
      self.add_diagnostic(
        DiagnosticMessage::ExtensionReceiverTypeMismatch {
          expected,
          got,
          span: func_span.clone(),
        }
        .report(),
      );
      return;
    }

    let func_name = self.defs.get(def_id).name;
    self
      .extension_methods
      .entry(target_type_id)
      .or_default()
      .entry(func_name)
      .or_default()
      .push(*def_id);
  }

  fn try_resolve_extension_method(
    &mut self,
    node_id: &NodeId,
    obj_type: &TypeId,
    ma: &ASTMemberAccess,
    call: &ASTCallExpression,
    scope_kind: ScopeKind,
    ctx: &TypecheckContext,
  ) -> Option<TypeId> {
    let method_map = self.extension_methods.get(obj_type)?.clone();
    let candidates = method_map.get(&ma.member)?.clone();

    if candidates.is_empty() {
      return None;
    }

    let obj_node = self.ast.get(&ma.object);
    if let ASTNode::Expression(obj_expr) = obj_node {
      match obj_expr {
        ASTExpression::Variable(_) | ASTExpression::MemberAccess(_) | ASTExpression::VectorAccess(_) => {},
        ASTExpression::Literal(_) => {
          let method_name = self.get_symbol_name(&ma.member);
          let type_name = self.format_type_for_error(obj_type);
          self.add_diagnostic(
            DiagnosticMessage::ExtensionMethodOnLiteral {
              method: method_name,
              type_name,
              span: ma.span.clone(),
            }
            .report(),
          );
          return Some(self.types.error());
        },
        _ => {
          let method_name = self.get_symbol_name(&ma.member);
          let type_name = self.format_type_for_error(obj_type);
          self.add_diagnostic(
            DiagnosticMessage::ExtensionMethodOnTemporary {
              method: method_name,
              type_name,
              span: ma.span.clone(),
            }
            .report(),
          );
          return Some(self.types.error());
        },
      }
    }

    if candidates.len() == 1 {
      let ext_def_id = candidates[0];
      let func_def = match &self.defs.get(&ext_def_id).kind {
        DefinitionKind::Function(fd) => fd.clone(),
        _ => return None,
      };

      let explicit_params: Vec<_> = func_def.params[1..].to_vec();
      let param_types: Vec<TypeId> = explicit_params.iter().map(|p| self.get_definition_type(p)).collect();

      let arg_types: Vec<TypeId> = call
        .arguments
        .iter()
        .enumerate()
        .map(|(i, arg)| {
          if let Some(param_type) = param_types.get(i) {
            let infer = InferContext::expecting(*param_type);
            self.typecheck_node_with_infer(arg, scope_kind, ctx, &infer)
          } else {
            self.typecheck_node(arg, scope_kind, ctx)
          }
        })
        .collect();

      let method_name = self.get_symbol_name(&ma.member);
      if arg_types.len() != explicit_params.len() {
        self.add_diagnostic(
          DiagnosticMessage::ArgumentCountMismatch {
            expected: explicit_params.len(),
            got: arg_types.len(),
            func_name: method_name.clone(),
            span: call.span.clone(),
          }
          .report(),
        );
      }

      let check_count = std::cmp::min(arg_types.len(), param_types.len());
      for i in 0..check_count {
        if self.types.is_error(&arg_types[i]) || self.types.is_error(&param_types[i]) {
          continue;
        }
        if !self.types.is_assignable(&param_types[i], &arg_types[i]) {
          let expected = self.format_type_for_error(&param_types[i]);
          let got = self.format_type_for_error(&arg_types[i]);
          self.add_diagnostic(
            DiagnosticMessage::ArgumentTypeMismatch {
              param_idx: i + 1,
              expected,
              got,
              span: self.node_span(&call.arguments[i]).clone(),
            }
            .report(),
          );
        }
      }

      self.set_resolved_call(node_id, ext_def_id);
      self.mark_referenced(ext_def_id);
      self.check_extension_mutability(&func_def, ma);

      return Some(func_def.return_type);
    }

    // Prepend receiver type: check_arity counts all params including receiver.
    let arg_types: Vec<TypeId> = call
      .arguments
      .iter()
      .map(|arg| self.typecheck_node(arg, scope_kind, ctx))
      .collect();

    let mut full_arg_types = vec![*obj_type];
    full_arg_types.extend_from_slice(&arg_types);

    let resolved_def_id = match self.resolve_overload(&candidates, &full_arg_types, &call.span, None) {
      Ok(def_id) => def_id,
      Err(()) => return Some(self.types.error()),
    };

    self.set_resolved_call(node_id, resolved_def_id);
    self.mark_referenced(resolved_def_id);

    let func_def = match &self.defs.get(&resolved_def_id).kind {
      DefinitionKind::Function(fd) => fd.clone(),
      _ => return Some(self.types.error()),
    };

    self.check_extension_mutability(&func_def, ma);

    Some(func_def.return_type)
  }

  fn check_extension_mutability(
    &mut self,
    func_def: &ignis_type::definition::FunctionDefinition,
    ma: &ASTMemberAccess,
  ) {
    let requires_mut = func_def
      .attrs
      .iter()
      .any(|a| matches!(a, FunctionAttr::Extension { mutable: true, .. }));

    if !requires_mut {
      return;
    }

    let obj_node = self.ast.get(&ma.object);
    if let ASTNode::Expression(obj_expr) = obj_node
      && !self.is_mutable_expression(obj_expr)
    {
      let method_name = self.get_symbol_name(&ma.member);
      let var_name = self.get_var_name_from_expr(obj_expr);
      self.add_diagnostic(
        DiagnosticMessage::MutatingMethodOnImmutable {
          method: method_name,
          var_name,
          span: ma.span.clone(),
        }
        .report(),
      );
    }
  }

  fn zonk_all_inference_vars(&mut self) {
    let all_vars: Vec<(DefinitionId, ignis_type::types::InferVarId)> =
      self.scope_infer_vars.values().flat_map(|v| v.iter().copied()).collect();

    let error_type = self.types.error();

    for (def_id, _infer_var_id) in all_vars {
      let var_type = *self.defs.type_of(&def_id);
      let resolved = self.infer_ctx.resolve(var_type, &mut self.types);

      match self.infer_ctx.zonk(resolved, &mut self.types) {
        Ok(concrete_type) => {
          let def = self.defs.get_mut(&def_id);
          if let DefinitionKind::Variable(ref mut v) = def.kind {
            v.type_id = concrete_type;
          }
        },
        Err(unresolved_var) => {
          let var_name = self.symbols.borrow().get(&self.defs.get(&def_id).name).to_string();
          let def_span = self.defs.get(&def_id).span.clone();

          let origin_span = self.infer_ctx.get_origin(unresolved_var).cloned().unwrap_or(def_span);

          self.add_diagnostic(
            DiagnosticMessage::CannotInferVariableType {
              var_name,
              span: origin_span,
            }
            .report(),
          );

          let def = self.defs.get_mut(&def_id);
          if let DefinitionKind::Variable(ref mut v) = def.kind {
            v.type_id = error_type;
          }
        },
      }
    }

    self.zonk_node_types();
  }

  fn zonk_node_types(&mut self) {
    let node_ids: Vec<NodeId> = self.node_types.keys().copied().collect();

    let error_type = self.types.error();

    for node_id in node_ids {
      let ty = self.node_types[&node_id];
      let resolved = self.infer_ctx.resolve(ty, &mut self.types);

      if resolved == ty {
        continue;
      }

      match self.infer_ctx.zonk(resolved, &mut self.types) {
        Ok(concrete_type) => {
          self.node_types.insert(node_id, concrete_type);
        },
        Err(_) => {
          self.node_types.insert(node_id, error_type);
        },
      }
    }
  }

  fn typecheck_lambda(
    &mut self,
    node_id: &NodeId,
    lambda: &ignis_ast::expressions::lambda::ASTLambda,
    _ictx: &InferContext,
  ) -> TypeId {
    let mut param_types = Vec::new();
    let mut param_defs = Vec::new();

    self.scopes.push(ScopeKind::Function);

    let resolver_param_defs = self.lambda_param_defs.get(node_id).cloned();

    for (i, param) in lambda.params.iter().enumerate() {
      let param_type = self.resolve_type_syntax(&param.type_);

      let param_def_id = if let Some(ref defs) = resolver_param_defs
        && let Some(&existing_id) = defs.get(i)
      {
        let def = self.defs.get_mut(&existing_id);
        if let ignis_type::definition::DefinitionKind::Parameter(pd) = &mut def.kind {
          pd.type_id = param_type;
        }
        let _ = self.scopes.define(&param.name, &existing_id, false);
        existing_id
      } else {
        let new_id = self.defs.alloc(ignis_type::definition::Definition {
          kind: ignis_type::definition::DefinitionKind::Parameter(ignis_type::definition::ParameterDefinition {
            type_id: param_type,
            mutable: false,
            attrs: Vec::new(),
          }),
          name: param.name,
          span: param.span.clone(),
          name_span: param.span.clone(),
          visibility: ignis_type::definition::Visibility::Private,
          owner_module: self.current_module,
          owner_namespace: self.current_namespace,
          doc: None,
        });
        let _ = self.scopes.define(&param.name, &new_id, false);
        new_id
      };

      self.node_types.insert(*node_id, param_type);

      param_types.push(param_type);
      param_defs.push(param_def_id);
    }

    let return_type = self.resolve_type_syntax(&lambda.return_type);

    let old_ctx = std::mem::take(&mut self.scope_infer_vars);
    let body_node_id = match &lambda.body {
      ignis_ast::expressions::lambda::LambdaBody::Expression(id) => *id,
      ignis_ast::expressions::lambda::LambdaBody::Block(id) => *id,
    };

    let tc_ctx = TypecheckContext::with_return(return_type);
    let body_type = self.typecheck_node(&body_node_id, ScopeKind::Function, &tc_ctx);

    if let ignis_ast::expressions::lambda::LambdaBody::Expression(_) = &lambda.body
      && !self.types.types_equal(&body_type, &return_type)
      && !self.types.is_error(&body_type)
      && !self.types.is_error(&return_type)
      && !matches!(self.types.get(&body_type), Type::Never)
    {
      self.add_diagnostic(
        DiagnosticMessage::ReturnTypeMismatch {
          expected: self.format_type_for_error(&return_type),
          got: self.format_type_for_error(&body_type),
          span: lambda.span.clone(),
        }
        .report(),
      );
    }

    self.scope_infer_vars = old_ctx;
    self.scopes.pop();

    self.types.function(param_types, return_type, false)
  }
}
