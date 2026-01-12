use crate::{Analyzer, InferContext, ScopeKind, TypecheckContext};
use ignis_ast::{
  expressions::{ASTCallExpression, ASTExpression, ASTLiteral},
  statements::ASTStatement,
  type_::IgnisTypeSyntax,
  ASTNode, NodeId,
};
use ignis_diagnostics::{
  diagnostic_report::{Diagnostic, Severity},
  message::DiagnosticMessage,
};
use ignis_type::{
  definition::{ConstValue, DefinitionKind},
  span::Span,
  types::{Type, TypeId},
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
  }

  fn typecheck_node(
    &mut self,
    node_id: &NodeId,
    scope_kind: ScopeKind,
    ctx: &TypecheckContext,
  ) -> TypeId {
    self.typecheck_node_with_infer(node_id, scope_kind, ctx, &InferContext::none())
  }

  fn typecheck_node_with_infer(
    &mut self,
    node_id: &NodeId,
    scope_kind: ScopeKind,
    ctx: &TypecheckContext,
    infer: &InferContext,
  ) -> TypeId {
    if let Some(ty) = self.lookup_type(&node_id) {
      return ty.clone();
    }

    let node = self.ast.get(&node_id);
    let ty = match node {
      ASTNode::Statement(stmt) => self.typecheck_statement(node_id, stmt, scope_kind, ctx),
      ASTNode::Expression(expr) => self.typecheck_expression(expr, scope_kind, ctx, infer),
    }
    .clone();

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

        let var_type = if self.types.is_unknown(&declared_type) {
          if let Some(value_id) = &var.value {
            self.typecheck_node(value_id, scope_kind, ctx)
          } else {
            self.types.error()
          }
        } else {
          declared_type.clone()
        };

        let lookedup_def = self.lookup_def(node_id);

        if let Some(def_id) = lookedup_def.cloned() {
          let def = self.defs.get_mut(&def_id);

          def.kind = DefinitionKind::Variable(ignis_type::definition::VariableDefinition {
            type_id: var_type.clone(),
            mutable: var.metadata.is_mutable(),
          });
        }

        if let Some(value_id) = &var.value {
          if !self.types.is_unknown(&declared_type) {
            let infer = InferContext::expecting(var_type.clone());
            let value_type = self.typecheck_node_with_infer(value_id, scope_kind, ctx, &infer);
            self.typecheck_assignment(&var_type, &value_type, &var.span);
          }
        }

        self.define_decl_in_current_scope(node_id);

        self.types.void()
      },
      ASTStatement::Function(func) => {
        let def_id = self.define_decl_in_current_scope(node_id);
        let return_type = self.resolve_type_syntax_with_span(&func.signature.return_type, &func.signature.span);

        let param_ids = def_id
          .as_ref()
          .and_then(|def_id| match &self.defs.get(&def_id).kind {
            DefinitionKind::Function(func_def) => Some(func_def.params.clone()),
            _ => None,
          })
          .unwrap_or_default();

        for (param, param_id) in func.signature.parameters.iter().zip(param_ids.iter()) {
          let param_type = self.resolve_type_syntax_with_span(&param.type_, &param.span);

          if let DefinitionKind::Parameter(param_def) = &mut self.defs.get_mut(&param_id).kind {
            param_def.type_id = param_type.clone();
            param_def.mutable = param.metadata.is_mutable();
          }
        }

        if let Some(def_id) = &def_id {
          if let DefinitionKind::Function(func_def) = &mut self.defs.get_mut(&def_id).kind {
            func_def.return_type = return_type.clone();
          }
        }

        self.scopes.push(ScopeKind::Function);
        if let Some(def_id) = &def_id {
          self.define_function_params_in_scope(def_id);
        }

        let func_ctx = TypecheckContext::with_return(return_type.clone());

        if let Some(body_id) = &func.body {
          self.typecheck_node(body_id, ScopeKind::Function, &func_ctx);
        }

        self.scopes.pop();

        return_type
      },
      ASTStatement::Constant(const_) => {
        let const_type = self.resolve_type_syntax_with_span(&const_.ty, &const_.span);

        if let Some(def_id) = self.lookup_def(node_id).cloned() {
          self.defs.get_mut(&def_id).kind = DefinitionKind::Constant(ignis_type::definition::ConstantDefinition {
            type_id: const_type.clone(),
            value: None,
          });
        }

        if let Some(value_id) = &const_.value {
          let infer = InferContext::expecting(const_type.clone());
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
        let cond_type = self.typecheck_node(&if_stmt.condition, scope_kind, ctx);
        let boolean_type = self.types.boolean();
        let conditional_span = self.node_span(&if_stmt.condition).clone();

        self.typecheck_assignment(&boolean_type, &cond_type, &conditional_span);

        let then_type = self.typecheck_node(&if_stmt.then_block, ScopeKind::Block, ctx);
        let else_type = if let Some(else_branch) = &if_stmt.else_block {
          self.typecheck_node(else_branch, ScopeKind::Block, ctx)
        } else {
          self.types.void()
        };

        self.typecheck_common_type(&then_type, &else_type, &if_stmt.span)
      },
      ASTStatement::While(while_stmt) => {
        self.scopes.push(ScopeKind::Loop);
        let cond_type = self.typecheck_node(&while_stmt.condition, ScopeKind::Loop, ctx);
        let boolean_type = self.types.boolean();
        let conditional_span = self.node_span(&while_stmt.condition).clone();

        self.typecheck_assignment(&boolean_type, &cond_type, &conditional_span);
        self.typecheck_node(&while_stmt.body, ScopeKind::Loop, ctx);

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
      ASTStatement::Return(ret) => {
        if let Some(expected_return_type) = ctx.expected_return.clone() {
          if let Some(value) = &ret.expression {
            let infer = InferContext::expecting(expected_return_type.clone());
            let value_type = self.typecheck_node_with_infer(&value, scope_kind, ctx, &infer);

            if !self.types.types_equal(&expected_return_type, &value_type) {
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
      ASTStatement::Expression(expr) => self.typecheck_expression(expr, scope_kind, ctx, &InferContext::none()),
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
          self.typecheck_node(&decl, scope_kind, ctx);
        }

        self.types.void()
      },
      _ => self.types.void(),
    }
  }

  fn typecheck_expression(
    &mut self,
    expr: &ASTExpression,
    scope_kind: ScopeKind,
    ctx: &TypecheckContext,
    infer: &InferContext,
  ) -> TypeId {
    match expr {
      ASTExpression::Literal(lit) => self.typecheck_literal(lit, infer),
      ASTExpression::Variable(var) => {
        if let Some(def_id) = self.scopes.lookup(&var.name).cloned() {
          self.get_definition_type(&def_id)
        } else {
          self.types.error()
        }
      },
      ASTExpression::Call(call) => self.typecheck_call(call, scope_kind, ctx),
      ASTExpression::Binary(binary) => self.typecheck_binary(binary, scope_kind, ctx),
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
        match self.types.get(&expr_type).clone() {
          ignis_type::types::Type::Pointer(inner) => inner,
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

        if !self.is_integer_type(&index_type) {
          self.add_diagnostic(
            DiagnosticMessage::VectorIndexNonInteger {
              index_type: self.format_type_for_error(&index_type),
              span: access.span.clone(),
            }
            .report(),
          );
        }

        if let ignis_type::types::Type::Vector { element, size } = self.types.get(&base_type).clone() {
          // Compile-time bounds checking for constant indices
          if let Some(array_size) = size {
            if let Some(ConstValue::Int(index_val)) = self.const_eval_expression_node(&access.index, scope_kind) {
              if index_val < 0 || (index_val as usize) >= array_size {
                self.add_diagnostic(
                  DiagnosticMessage::IndexOutOfBounds {
                    index: index_val,
                    size: array_size,
                    span: access.span.clone(),
                  }
                  .report(),
                );
              }
            }
          }
          element
        } else {
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
          .and_then(|e| self.lookup_type(e).map(|t| t.clone()))
          .unwrap_or(self.types.error());

        self.types.vector(elem_type, Some(vector.items.len()))
      },
      ASTExpression::Path(path) => {
        // Resolve the path to get the definition type
        if let Some(def_id) = self.resolve_qualified_path(&path.segments) {
          self.get_definition_type(&def_id)
        } else {
          self.types.error()
        }
      },
      ASTExpression::PostfixIncrement { expr, span } => {
        let expr_type = self.typecheck_node(&expr, scope_kind, ctx);

        let target_node = self.ast.get(expr);
        if let ASTNode::Expression(target_expr) = target_node {
          if !self.is_mutable_expression(target_expr) {
            let var_name = self.get_var_name_from_expr(target_expr);
            self.add_diagnostic(
              DiagnosticMessage::ImmutableAssignment {
                var_name,
                span: span.clone(),
              }
              .report(),
            );
          }
        }

        if self.types.is_numeric(&expr_type) {
          expr_type
        } else {
          self.types.error()
        }
      },
      ASTExpression::PostfixDecrement { expr, span } => {
        let expr_type = self.typecheck_node(&expr, scope_kind, ctx);

        let target_node = self.ast.get(expr);
        if let ASTNode::Expression(target_expr) = target_node {
          if !self.is_mutable_expression(target_expr) {
            let var_name = self.get_var_name_from_expr(target_expr);
            self.add_diagnostic(
              DiagnosticMessage::ImmutableAssignment {
                var_name,
                span: span.clone(),
              }
              .report(),
            );
          }
        }

        if self.types.is_numeric(&expr_type) {
          expr_type
        } else {
          self.types.error()
        }
      },
    }
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
      IgnisLiteralValue::String(_) => self.types.string(),
      IgnisLiteralValue::Null => self.types.error(),
    }
  }

  fn coerce_signed_literal(
    &mut self,
    value: i64,
    default: TypeId,
    infer: &InferContext,
    span: &Span,
  ) -> TypeId {
    if let Some(expected) = &infer.expected {
      if self.is_integer_type(expected) {
        if self.signed_fits_in_type(value, expected) {
          return expected.clone();
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
    if let Some(expected) = &infer.expected {
      if self.is_integer_type(expected) {
        if self.unsigned_fits_in_type(value, expected) {
          return expected.clone();
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
    }
    default
  }

  fn coerce_float_literal(
    &self,
    default: TypeId,
    infer: &InferContext,
  ) -> TypeId {
    if let Some(expected) = &infer.expected {
      if self.is_float_type(expected) {
        return expected.clone();
      }
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
    matches!((self.types.get(from), self.types.get(to)), (Type::Pointer(_), Type::Pointer(_)))
  }

  fn typecheck_call(
    &mut self,
    call: &ASTCallExpression,
    scope_kind: ScopeKind,
    ctx: &TypecheckContext,
  ) -> TypeId {
    // Check for builtin typeOf/sizeOf - they are handled specially
    if let ASTNode::Expression(ASTExpression::Variable(var)) = self.ast.get(&call.callee) {
      let name = self.get_symbol_name(&var.name);
      if name == "typeOf" {
        return self.typecheck_typeof_builtin(call, scope_kind, ctx);
      }
      if name == "sizeOf" {
        return self.typecheck_sizeof_builtin(call, scope_kind, ctx);
      }
    }

    let callee_type = self.typecheck_node(&call.callee, scope_kind, ctx);

    let param_types: Vec<TypeId> = if let Type::Function { params, .. } = self.types.get(&callee_type).clone() {
      params
    } else {
      vec![]
    };

    let arg_types: Vec<TypeId> = call
      .arguments
      .iter()
      .enumerate()
      .map(|(i, arg)| {
        if let Some(param_type) = param_types.get(i) {
          let infer = InferContext::expecting(param_type.clone());
          self.typecheck_node_with_infer(arg, scope_kind, ctx, &infer)
        } else {
          self.typecheck_node(arg, scope_kind, ctx)
        }
      })
      .collect();

    if let Type::Function {
      params,
      ret,
      is_variadic,
    } = self.types.get(&callee_type).clone()
    {
      let func_name = if let ASTNode::Expression(ASTExpression::Variable(var)) = self.ast.get(&call.callee) {
        self.get_symbol_name(&var.name)
      } else {
        "<function>".to_string()
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
        if !self.types.types_equal(&params[i], &arg_types[i]) {
          // Special case: deallocate accepts any *mut T, coercing to *mut u8
          if func_name == "deallocate" && i == 0 {
            if self.is_ptr_coercion(&arg_types[i], &params[i]) {
              continue;
            }
          }

          let expected = self.format_type_for_error(&params[i]);
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

      ret
    } else {
      let type_name = self.format_type_for_error(&callee_type);
      self.add_diagnostic(
        DiagnosticMessage::NotCallable {
          type_name,
          span: call.span.clone(),
        }
        .report(),
      );
      self.types.error()
    }
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
    scope_kind: ScopeKind,
    ctx: &TypecheckContext,
  ) -> TypeId {
    if call.arguments.len() != 1 {
      self.add_diagnostic(
        DiagnosticMessage::ArgumentCountMismatch {
          expected: 1,
          got: call.arguments.len(),
          func_name: "sizeOf".to_string(),
          span: call.span.clone(),
        }
        .report(),
      );
      return self.types.error();
    }

    let arg_type = self.typecheck_node(&call.arguments[0], scope_kind, ctx);
    let base_type = self.unwrap_reference_type(&arg_type);

    if matches!(self.types.get(&base_type), ignis_type::types::Type::Unknown) {
      self.add_diagnostic(
        DiagnosticMessage::InvalidSizeOfOperand {
          span: call.span.clone(),
        }
        .report(),
      );
      return self.types.error();
    }

    self.types.u64()
  }

  pub(crate) fn unwrap_reference_type(
    &self,
    ty: &TypeId,
  ) -> TypeId {
    match self.types.get(ty) {
      ignis_type::types::Type::Reference { inner, .. } => self.unwrap_reference_type(inner),
      _ => ty.clone(),
    }
  }

  fn typecheck_binary(
    &mut self,
    binary: &ignis_ast::expressions::ASTBinary,
    scope_kind: ScopeKind,
    ctx: &TypecheckContext,
  ) -> TypeId {
    let left_type = self.typecheck_node(&binary.left, scope_kind, ctx);

    let right_type = if self.types.is_numeric(&left_type) {
      let infer = InferContext::expecting(left_type.clone());
      self.typecheck_node_with_infer(&binary.right, scope_kind, ctx, &infer)
    } else {
      self.typecheck_node(&binary.right, scope_kind, ctx)
    };

    match binary.operator {
      ASTBinaryOperator::Add
      | ASTBinaryOperator::Subtract
      | ASTBinaryOperator::Multiply
      | ASTBinaryOperator::Divide
      | ASTBinaryOperator::Modulo => {
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
      ASTBinaryOperator::Equal
      | ASTBinaryOperator::NotEqual
      | ASTBinaryOperator::LessThan
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
      ASTBinaryOperator::And | ASTBinaryOperator::Or => {
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

  fn typecheck_unary(
    &mut self,
    unary: &ignis_ast::expressions::ASTUnary,
    scope_kind: ScopeKind,
    ctx: &TypecheckContext,
  ) -> TypeId {
    let operand_type = self.typecheck_node(&unary.operand, scope_kind, ctx);

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
    let infer = InferContext::expecting(target_type.clone());
    let value_type = self.typecheck_node_with_infer(&assign.value, scope_kind, ctx, &infer);

    let target_node = self.ast.get(&assign.target);
    if let ASTNode::Expression(target_expr) = target_node {
      if !self.is_mutable_expression(target_expr) {
        let var_name = self.get_var_name_from_expr(target_expr);
        self.add_diagnostic(
          DiagnosticMessage::ImmutableAssignment {
            var_name,
            span: assign.span.clone(),
          }
          .report(),
        );
      }
    }

    match assign.operator {
      ASTAssignmentOperator::Assign => {
        self.typecheck_assignment(&target_type, &value_type, &assign.span);
      },
      _ => {
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
          self.types.error();
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
    if !self.types.types_equal(target_type, value_type) {
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

    let from_type = self.types.get(&expr_type).clone();
    let to_type = self.types.get(&target_type).clone();

    use ignis_type::types::Type;

    let is_valid = match (&from_type, &to_type) {
      (_, _) if self.types.is_numeric(&expr_type) && self.types.is_numeric(&target_type) => true,
      (Type::Boolean, _) if self.types.is_integer(&target_type) => true,
      (_, Type::Boolean) if self.types.is_integer(&expr_type) => true,
      (Type::Char, _) if self.types.is_integer(&target_type) => true,
      (_, Type::Char) if self.types.is_integer(&expr_type) => true,
      (Type::Pointer(_), Type::Reference { .. }) => true,
      (Type::Reference { .. }, Type::Pointer(_)) => true,
      (Type::Pointer(_), Type::Pointer(_)) => true,
      (Type::Reference { .. }, Type::Reference { .. }) => true,
      (Type::Unknown, _) => true,
      (Type::Reference { inner, .. }, _) if self.types.is_unknown(inner) => true,
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
      a.clone()
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
        a.clone()
      } else {
        b.clone()
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
      IgnisTypeSyntax::String => self.types.string(),
      IgnisTypeSyntax::Boolean => self.types.boolean(),
      IgnisTypeSyntax::Void => self.types.void(),
      IgnisTypeSyntax::Char => self.types.char(),
      IgnisTypeSyntax::Unknown => self.types.unknown(),
      IgnisTypeSyntax::Null => self.types.error(),
      IgnisTypeSyntax::Vector(inner, size) => {
        let inner_type = self.resolve_type_syntax_impl(inner, span);
        self.types.vector(inner_type, *size)
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
      IgnisTypeSyntax::Pointer(inner) => {
        let inner_type = self.resolve_type_syntax_impl(inner, span);
        self.types.pointer(inner_type)
      },
      IgnisTypeSyntax::Reference { inner, mutable } => {
        let inner_type = self.resolve_type_syntax_impl(inner, span);
        self.types.reference(inner_type, *mutable)
      },
      IgnisTypeSyntax::Named(symbol_id) => {
        if let Some(def_id) = self.scopes.lookup(&symbol_id) {
          self.type_of(def_id).clone()
        } else {
          let name = self.symbols.borrow().get(&symbol_id).to_string();
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
      IgnisTypeSyntax::Path { segments, .. } => {
        if let Some(last) = segments.last() {
          if let Some(def_id) = self.scopes.lookup(&last.0) {
            self.type_of(def_id).clone()
          } else {
            let name = self.symbols.borrow().get(&last.0).to_string();
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
        } else {
          self.types.error()
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
        if let Some(def_id) = self.scopes.lookup(&var.name) {
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
            if let ignis_type::types::Type::Reference { mutable, .. } = self.types.get(type_id) {
              return *mutable;
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
          if let Some(def_id) = self.scopes.lookup(last) {
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
      _ => false,
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
          self.get_symbol_name(last)
        } else {
          "<unknown>".to_string()
        }
      },
      ASTExpression::Dereference(_) => "<dereferenced value>".to_string(),
      ASTExpression::VectorAccess(_) => "<vector element>".to_string(),
      _ => "<expression>".to_string(),
    }
  }

  fn is_lvalue(
    &self,
    expr: &ASTExpression,
  ) -> bool {
    match expr {
      ASTExpression::Variable(_) | ASTExpression::Path(_) => true,
      ASTExpression::Dereference(_) => true,
      ASTExpression::VectorAccess(_) => true,
      _ => false,
    }
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
      Type::String => "string".to_string(),
      Type::Void => "void".to_string(),
      Type::Never => "never".to_string(),
      Type::Unknown => "unknown".to_string(),
      Type::Error => "error".to_string(),
      Type::Pointer(inner) => format!("*{}", self.format_type_for_error(inner)),
      Type::Reference { inner, mutable } => {
        if *mutable {
          format!("&mut {}", self.format_type_for_error(inner))
        } else {
          format!("&{}", self.format_type_for_error(inner))
        }
      },
      Type::Vector { element, size } => {
        if let Some(s) = size {
          format!("[{}; {}]", self.format_type_for_error(element), s)
        } else {
          format!("[{}]", self.format_type_for_error(element))
        }
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
    }
  }

  pub(crate) fn get_definition_type(
    &mut self,
    def_id: &ignis_type::definition::DefinitionId,
  ) -> TypeId {
    match &self.defs.get(def_id).kind {
      DefinitionKind::Function(func_def) => {
        let param_types: Vec<TypeId> = func_def
          .params
          .iter()
          .map(|param_id| self.type_of(param_id).clone())
          .collect();

        self
          .types
          .function(param_types, func_def.return_type.clone(), func_def.is_variadic)
      },
      _ => self.type_of(def_id).clone(),
    }
  }
}
