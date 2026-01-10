use crate::{Analyzer, ScopeKind};
use ignis_ast::{ASTNode, NodeId, statements::ASTStatement, expressions::ASTExpression};
use ignis_ast::expressions::binary::ASTBinaryOperator;
use ignis_ast::expressions::assignment::ASTAssignmentOperator;
use ignis_ast::expressions::unary::UnaryOperator;
use ignis_hir::{
  HIR, HIRNode, HIRKind, HIRId,
  operation::{BinaryOperation, UnaryOperation},
  statement::LoopKind,
};
use ignis_type::span::Span;

impl<'a> Analyzer<'a> {
  pub fn lower_to_hir(
    &mut self,
    roots: &[NodeId],
  ) -> HIR {
    let mut hir = HIR::new();

    for root in roots {
      self.lower_node_to_hir(root, &mut hir, ScopeKind::Global);
    }

    hir
  }

  fn lower_node_to_hir(
    &mut self,
    node_id: &NodeId,
    hir: &mut HIR,
    scope_kind: ScopeKind,
  ) -> HIRId {
    let node = self.ast.get(node_id).clone();

    match node {
      ASTNode::Statement(stmt) => self.lower_statement_to_hir(node_id, &stmt, hir, scope_kind),
      ASTNode::Expression(expr) => self.lower_expression_to_hir(node_id, &expr, hir, scope_kind),
    }
  }

  fn lower_statement_to_hir(
    &mut self,
    node_id: &NodeId,
    stmt: &ASTStatement,
    hir: &mut HIR,
    scope_kind: ScopeKind,
  ) -> HIRId {
    match stmt {
      ASTStatement::Variable(var) => {
        let def_id = match self.lookup_def(node_id).cloned() {
          Some(id) => id,
          None => match self.scopes.lookup(&var.name).cloned() {
            Some(id) => id,
            None => {
              return hir.alloc(HIRNode {
                kind: HIRKind::Error,
                span: var.span.clone(),
                type_id: self.types.error(),
              });
            },
          },
        };
        let init = var
          .value
          .as_ref()
          .map(|value_id| self.lower_node_to_hir(value_id, hir, scope_kind));

        if let Some(init_id) = init {
          hir.variables_inits.insert(def_id, init_id);
        }

        self.scopes.define(&var.name, &def_id).ok();

        let hir_node = HIRNode {
          kind: HIRKind::Let {
            name: def_id,
            value: init,
          },
          span: var.span.clone(),
          type_id: self.types.void(),
        };

        hir.alloc(hir_node)
      },
      ASTStatement::Function(func) => {
        let def_id = match self.lookup_def(node_id).cloned() {
          Some(id) => id,
          None => match self.scopes.lookup(&func.signature.name).cloned() {
            Some(id) => id,
            None => {
              return hir.alloc(HIRNode {
                kind: HIRKind::Error,
                span: func.signature.span.clone(),
                type_id: self.types.error(),
              });
            },
          },
        };

        if let Some(body_id) = func.body {
          self.scopes.push(ScopeKind::Function);

          if let Some(def_id_val) = self.lookup_def(node_id).cloned() {
            self.define_function_params_in_scope(&def_id_val);
          }

          let body_hir_id = self.lower_node_to_hir(&body_id, hir, ScopeKind::Function);
          self.scopes.pop();
          hir.function_bodies.insert(def_id, body_hir_id);
        }

        let hir_node = HIRNode {
          kind: HIRKind::Block {
            statements: Vec::new(),
            expression: None,
          },
          span: func.signature.span.clone(),
          type_id: self.types.void(),
        };

        let node_id = hir.alloc(hir_node);
        hir.items.push(def_id);

        let main_symbol = { self.symbols.borrow_mut().get_or_intern("main") };
        if func.signature.name == main_symbol {
          hir.entry_point = Some(def_id);
        }

        node_id
      },
      ASTStatement::Constant(const_) => {
        let def_id = match self.lookup_def(node_id).cloned() {
          Some(id) => id,
          None => match self.scopes.lookup(&const_.name).cloned() {
            Some(id) => id,
            None => {
              return hir.alloc(HIRNode {
                kind: HIRKind::Error,
                span: const_.span.clone(),
                type_id: self.types.error(),
              });
            },
          },
        };

        let init_hir = const_.value.as_ref().map(|value_id| {
          let init = self.lower_node_to_hir(value_id, hir, scope_kind);
          hir.variables_inits.insert(def_id, init);
          init
        });

        let hir_node = HIRNode {
          kind: HIRKind::Let {
            name: def_id,
            value: init_hir,
          },
          span: const_.span.clone(),
          type_id: self.types.void(),
        };

        let node_id = hir.alloc(hir_node);
        hir.items.push(def_id);

        node_id
      },
      ASTStatement::Block(block) => {
        self.scopes.push(ScopeKind::Block);

        let stmt_hir_ids: Vec<_> = block
          .statements
          .iter()
          .map(|stmt_id| self.lower_node_to_hir(stmt_id, hir, ScopeKind::Block))
          .collect();

        self.scopes.pop();

        let block_type = self.lookup_type(node_id).cloned().unwrap_or_else(|| self.types.void());

        let hir_node = HIRNode {
          kind: HIRKind::Block {
            statements: stmt_hir_ids,
            expression: None,
          },
          span: block.span.clone(),
          type_id: block_type,
        };

        hir.alloc(hir_node)
      },
      ASTStatement::If(if_stmt) => {
        let condition_id = self.lower_node_to_hir(&if_stmt.condition, hir, scope_kind);
        let then_id = self.lower_node_to_hir(&if_stmt.then_block, hir, ScopeKind::Block);

        let else_id = if_stmt
          .else_block
          .as_ref()
          .map(|else_branch| self.lower_node_to_hir(else_branch, hir, ScopeKind::Block));

        let if_type = self.lookup_type(node_id).cloned().unwrap_or_else(|| self.types.void());

        let hir_node = HIRNode {
          kind: HIRKind::If {
            condition: condition_id,
            then_branch: then_id,
            else_branch: else_id,
          },
          span: if_stmt.span.clone(),
          type_id: if_type,
        };

        hir.alloc(hir_node)
      },
      ASTStatement::While(while_stmt) => {
        self.scopes.push(ScopeKind::Loop);
        let condition_id = self.lower_node_to_hir(&while_stmt.condition, hir, ScopeKind::Loop);
        let body_id = self.lower_node_to_hir(&while_stmt.body, hir, ScopeKind::Loop);
        self.scopes.pop();

        let hir_node = HIRNode {
          kind: HIRKind::Loop {
            condition: LoopKind::While {
              condition: condition_id,
            },
            body: body_id,
          },
          span: while_stmt.span.clone(),
          type_id: self.types.void(),
        };

        hir.alloc(hir_node)
      },
      ASTStatement::For(for_stmt) => {
        self.scopes.push(ScopeKind::Loop);

        let init = Some(self.lower_node_to_hir(&for_stmt.initializer, hir, ScopeKind::Loop));
        let condition = Some(self.lower_node_to_hir(&for_stmt.condition, hir, ScopeKind::Loop));
        let update = Some(self.lower_node_to_hir(&for_stmt.increment, hir, ScopeKind::Loop));
        let body_id = self.lower_node_to_hir(&for_stmt.body, hir, ScopeKind::Loop);

        self.scopes.pop();

        let hir_node = HIRNode {
          kind: HIRKind::Loop {
            condition: LoopKind::For {
              init,
              condition,
              update,
            },
            body: body_id,
          },
          span: for_stmt.span.clone(),
          type_id: self.types.void(),
        };

        hir.alloc(hir_node)
      },
      ASTStatement::Return(ret) => {
        let value = ret
          .expression
          .as_ref()
          .map(|value_id| self.lower_node_to_hir(value_id, hir, scope_kind));

        let hir_node = HIRNode {
          kind: HIRKind::Return(value),
          span: ret.span.clone(),
          type_id: self.types.never(),
        };

        hir.alloc(hir_node)
      },
      ASTStatement::Break(brk) => {
        let hir_node = HIRNode {
          kind: HIRKind::Break,
          span: brk.span.clone(),
          type_id: self.types.never(),
        };

        hir.alloc(hir_node)
      },
      ASTStatement::Continue(cont) => {
        let hir_node = HIRNode {
          kind: HIRKind::Continue,
          span: cont.span.clone(),
          type_id: self.types.never(),
        };

        hir.alloc(hir_node)
      },
      ASTStatement::Expression(expr) => {
        let expr_hir_id = self.lower_expression_to_hir(node_id, expr, hir, scope_kind);
        let expr_type = self.lookup_type(node_id).cloned().unwrap_or_else(|| self.types.void());

        hir.alloc(HIRNode {
          kind: HIRKind::ExpressionStatement(expr_hir_id),
          span: expr.span().clone(),
          type_id: expr_type,
        })
      },
      ASTStatement::Extern(ext) => self.lower_node_to_hir(&ext.item, hir, scope_kind),
      ASTStatement::Export(exp) => match exp {
        ignis_ast::statements::ASTExport::Declaration { decl, .. } => self.lower_node_to_hir(decl, hir, scope_kind),
        ignis_ast::statements::ASTExport::Name { .. } => hir.alloc(HIRNode {
          kind: HIRKind::Block {
            statements: Vec::new(),
            expression: None,
          },
          span: exp.span().clone(),
          type_id: self.types.void(),
        }),
      },
      _ => hir.alloc(HIRNode {
        kind: HIRKind::Block {
          statements: Vec::new(),
          expression: None,
        },
        span: Span::default(),
        type_id: self.types.void(),
      }),
    }
  }

  fn lower_expression_to_hir(
    &mut self,
    node_id: &NodeId,
    expr: &ASTExpression,
    hir: &mut HIR,
    scope_kind: ScopeKind,
  ) -> HIRId {
    match expr {
      ASTExpression::Literal(lit) => {
        let lit_type = self.lookup_type(node_id).cloned().unwrap_or_else(|| match &lit.value {
          ignis_type::value::IgnisLiteralValue::Int8(_) => self.types.i8(),
          ignis_type::value::IgnisLiteralValue::Int16(_) => self.types.i16(),
          ignis_type::value::IgnisLiteralValue::Int32(_) => self.types.i32(),
          ignis_type::value::IgnisLiteralValue::Int64(_) => self.types.i64(),
          ignis_type::value::IgnisLiteralValue::UnsignedInt8(_) => self.types.u8(),
          ignis_type::value::IgnisLiteralValue::UnsignedInt16(_) => self.types.u16(),
          ignis_type::value::IgnisLiteralValue::UnsignedInt32(_) => self.types.u32(),
          ignis_type::value::IgnisLiteralValue::UnsignedInt64(_) => self.types.u64(),
          ignis_type::value::IgnisLiteralValue::Float32(_) => self.types.f32(),
          ignis_type::value::IgnisLiteralValue::Float64(_) => self.types.f64(),
          ignis_type::value::IgnisLiteralValue::Boolean(_) => self.types.boolean(),
          ignis_type::value::IgnisLiteralValue::Char(_) => self.types.char(),
          ignis_type::value::IgnisLiteralValue::String(_) => self.types.string(),
          ignis_type::value::IgnisLiteralValue::Hex(_) => self.types.u32(),
          ignis_type::value::IgnisLiteralValue::Binary(_) => self.types.u8(),
          ignis_type::value::IgnisLiteralValue::Null => self.types.error(),
        });

        let hir_node = HIRNode {
          kind: HIRKind::Literal(lit.value.clone()),
          span: lit.span.clone(),
          type_id: lit_type,
        };

        hir.alloc(hir_node)
      },
      ASTExpression::Variable(var) => {
        let def_id = match self.scopes.lookup(&var.name) {
          Some(id) => id.clone(),
          None => {
            return hir.alloc(HIRNode {
              kind: HIRKind::Error,
              span: var.span.clone(),
              type_id: self.types.error(),
            });
          },
        };
        let var_type = self.type_of(&def_id).clone();

        let hir_node = HIRNode {
          kind: HIRKind::Variable(def_id),
          span: var.span.clone(),
          type_id: var_type,
        };

        hir.alloc(hir_node)
      },
      ASTExpression::Call(call) => {
        // Get the callee def_id by looking up the variable name
        let callee_node = self.ast.get(&call.callee);
        let callee_def_id = match callee_node {
          ASTNode::Expression(ASTExpression::Variable(var)) => match self.scopes.lookup(&var.name) {
            Some(def_id) => def_id.clone(),
            None => {
              let span = self.node_span(&call.callee).clone();
              return hir.alloc(HIRNode {
                kind: HIRKind::Error,
                span,
                type_id: self.types.error(),
              });
            },
          },
          ASTNode::Expression(ASTExpression::Path(path)) => {
            if let Some(last) = path.segments.last() {
              match self.scopes.lookup(last) {
                Some(def_id) => def_id.clone(),
                None => {
                  let span = self.node_span(&call.callee).clone();
                  return hir.alloc(HIRNode {
                    kind: HIRKind::Error,
                    span,
                    type_id: self.types.error(),
                  });
                },
              }
            } else {
              let span = self.node_span(&call.callee).clone();
              return hir.alloc(HIRNode {
                kind: HIRKind::Error,
                span,
                type_id: self.types.error(),
              });
            }
          },
          _ => {
            let span = self.node_span(&call.callee).clone();
            return hir.alloc(HIRNode {
              kind: HIRKind::Error,
              span,
              type_id: self.types.error(),
            });
          },
        };

        let callee_type = self.get_definition_type(&callee_def_id);

        let args_hir: Vec<_> = call
          .arguments
          .iter()
          .map(|arg| self.lower_node_to_hir(arg, hir, scope_kind))
          .collect();

        let return_type = if let ignis_type::types::Type::Function { ret, .. } = self.types.get(&callee_type).clone() {
          ret
        } else {
          self.types.error()
        };

        let hir_node = HIRNode {
          kind: HIRKind::Call {
            callee: callee_def_id,
            args: args_hir,
          },
          span: call.span.clone(),
          type_id: return_type,
        };

        hir.alloc(hir_node)
      },
      ASTExpression::Binary(binary) => {
        let left_id = self.lower_node_to_hir(&binary.left, hir, scope_kind);
        let right_id = self.lower_node_to_hir(&binary.right, hir, scope_kind);
        let binary_type = self.lookup_type(node_id).cloned().unwrap_or_else(|| self.types.error());

        let hir_node = HIRNode {
          kind: HIRKind::Binary {
            operation: convert_binary_op(&binary.operator),
            left: left_id,
            right: right_id,
          },
          span: binary.span.clone(),
          type_id: binary_type,
        };

        hir.alloc(hir_node)
      },
      ASTExpression::Unary(unary) => {
        let operand_id = self.lower_node_to_hir(&unary.operand, hir, scope_kind);
        let unary_type = self.lookup_type(node_id).cloned().unwrap_or_else(|| self.types.error());

        let hir_node = HIRNode {
          kind: HIRKind::Unary {
            operation: convert_unary_op(&unary.operator),
            operand: operand_id,
          },
          span: unary.span.clone(),
          type_id: unary_type,
        };

        hir.alloc(hir_node)
      },
      ASTExpression::Assignment(assign) => {
        let target_id = self.lower_node_to_hir(&assign.target, hir, scope_kind);
        let value_id = self.lower_node_to_hir(&assign.value, hir, scope_kind);
        let assign_type = self.lookup_type(node_id).cloned().unwrap_or_else(|| self.types.error());

        let op = if assign.operator == ASTAssignmentOperator::Assign {
          None
        } else {
          Some(convert_assignment_op(&assign.operator))
        };

        let hir_node = HIRNode {
          kind: HIRKind::Assign {
            target: target_id,
            value: value_id,
            operation: op,
          },
          span: assign.span.clone(),
          type_id: assign_type,
        };

        hir.alloc(hir_node)
      },
      ASTExpression::Cast(cast) => {
        let expr_id = self.lower_node_to_hir(&cast.expression, hir, scope_kind);
        let target_type = self.resolve_type_syntax(&cast.target_type);

        let hir_node = HIRNode {
          kind: HIRKind::Cast {
            expression: expr_id,
            target: target_type,
          },
          span: cast.span.clone(),
          type_id: target_type,
        };

        hir.alloc(hir_node)
      },
      ASTExpression::Reference(ref_) => {
        let expr_id = self.lower_node_to_hir(&ref_.inner, hir, scope_kind);
        let expr_type = self
          .lookup_type(&ref_.inner)
          .cloned()
          .unwrap_or_else(|| self.types.error());
        let ref_type = self.types.reference(expr_type, ref_.mutable);

        let hir_node = HIRNode {
          kind: HIRKind::Reference {
            expression: expr_id,
            mutable: ref_.mutable,
          },
          span: ref_.span.clone(),
          type_id: ref_type,
        };

        hir.alloc(hir_node)
      },
      ASTExpression::Dereference(deref) => {
        let expr_id = self.lower_node_to_hir(&deref.inner, hir, scope_kind);
        let expr_type = self
          .lookup_type(&deref.inner)
          .cloned()
          .unwrap_or_else(|| self.types.error());

        let deref_type = match self.types.get(&expr_type).clone() {
          ignis_type::types::Type::Pointer(inner) => inner,
          ignis_type::types::Type::Reference { inner, .. } => inner,
          _ => self.types.error(),
        };

        let hir_node = HIRNode {
          kind: HIRKind::Dereference(expr_id),
          span: deref.span.clone(),
          type_id: deref_type,
        };

        hir.alloc(hir_node)
      },
      ASTExpression::VectorAccess(access) => {
        let base_id = self.lower_node_to_hir(&access.name, hir, scope_kind);
        let index_id = self.lower_node_to_hir(&access.index, hir, scope_kind);
        let base_type = self
          .lookup_type(&access.name)
          .cloned()
          .unwrap_or_else(|| self.types.error());

        let elem_type = if let ignis_type::types::Type::Vector { element, .. } = self.types.get(&base_type).clone() {
          element
        } else {
          self.types.error()
        };

        let hir_node = HIRNode {
          kind: HIRKind::Index {
            base: base_id,
            index: index_id,
          },
          span: access.span.clone(),
          type_id: elem_type,
        };

        hir.alloc(hir_node)
      },
      ASTExpression::Grouped(grouped) => self.lower_node_to_hir(&grouped.expression, hir, scope_kind),
      ASTExpression::Vector(vector) => {
        let elem_hirs: Vec<_> = vector
          .items
          .iter()
          .map(|elem| self.lower_node_to_hir(elem, hir, scope_kind))
          .collect();

        let vec_type = self.types.vector(
          vector
            .items
            .first()
            .and_then(|e| self.lookup_type(e).cloned())
            .unwrap_or_else(|| self.types.error()),
          Some(vector.items.len()),
        );

        let hir_node = HIRNode {
          kind: HIRKind::VectorLiteral { elements: elem_hirs },
          span: vector.span.clone(),
          type_id: vec_type,
        };

        hir.alloc(hir_node)
      },
      ASTExpression::Path(path) => {
        let def_id = match path.segments.last().and_then(|name| self.scopes.lookup(name)) {
          Some(id) => id.clone(),
          None => {
            return hir.alloc(HIRNode {
              kind: HIRKind::Error,
              span: path.span.clone(),
              type_id: self.types.error(),
            });
          },
        };
        let path_type = self.type_of(&def_id).clone();

        let hir_node = HIRNode {
          kind: HIRKind::Variable(def_id),
          span: path.span.clone(),
          type_id: path_type,
        };

        hir.alloc(hir_node)
      },
      ASTExpression::PostfixIncrement { expr, span } => {
        let expr_id = self.lower_node_to_hir(expr, hir, scope_kind);
        let expr_type = self.lookup_type(expr).cloned().unwrap_or_else(|| self.types.error());

        let one_lit = hir.alloc(HIRNode {
          kind: HIRKind::Literal(ignis_type::value::IgnisLiteralValue::Int32(1)),
          span: span.clone(),
          type_id: expr_type,
        });

        let hir_node = HIRNode {
          kind: HIRKind::Binary {
            operation: BinaryOperation::Add,
            left: expr_id,
            right: one_lit,
          },
          span: span.clone(),
          type_id: expr_type,
        };

        hir.alloc(hir_node)
      },
      ASTExpression::PostfixDecrement { expr, span } => {
        let expr_id = self.lower_node_to_hir(expr, hir, scope_kind);
        let expr_type = self.lookup_type(expr).cloned().unwrap_or_else(|| self.types.error());

        let one_lit = hir.alloc(HIRNode {
          kind: HIRKind::Literal(ignis_type::value::IgnisLiteralValue::Int32(1)),
          span: span.clone(),
          type_id: expr_type,
        });

        let hir_node = HIRNode {
          kind: HIRKind::Binary {
            operation: BinaryOperation::Sub,
            left: expr_id,
            right: one_lit,
          },
          span: span.clone(),
          type_id: expr_type,
        };

        hir.alloc(hir_node)
      },
    }
  }
}

fn convert_binary_op(op: &ASTBinaryOperator) -> BinaryOperation {
  match op {
    ASTBinaryOperator::Add => BinaryOperation::Add,
    ASTBinaryOperator::Subtract => BinaryOperation::Sub,
    ASTBinaryOperator::Multiply => BinaryOperation::Mul,
    ASTBinaryOperator::Divide => BinaryOperation::Div,
    ASTBinaryOperator::Modulo => BinaryOperation::Mod,
    ASTBinaryOperator::Equal => BinaryOperation::Equal,
    ASTBinaryOperator::NotEqual => BinaryOperation::NotEqual,
    ASTBinaryOperator::LessThan => BinaryOperation::LessThan,
    ASTBinaryOperator::LessThanOrEqual => BinaryOperation::LessEqual,
    ASTBinaryOperator::GreaterThan => BinaryOperation::GreaterThan,
    ASTBinaryOperator::GreaterThanOrEqual => BinaryOperation::GreaterEqual,
    ASTBinaryOperator::And => BinaryOperation::And,
    ASTBinaryOperator::Or => BinaryOperation::Or,
    ASTBinaryOperator::BitAnd => BinaryOperation::BitAnd,
    ASTBinaryOperator::BitOr => BinaryOperation::BitOr,
    ASTBinaryOperator::BitXor => BinaryOperation::BitXor,
    ASTBinaryOperator::ShiftLeft => BinaryOperation::BitShiftLeft,
    ASTBinaryOperator::ShiftRight => BinaryOperation::BitShiftRight,
  }
}

fn convert_unary_op(op: &UnaryOperator) -> UnaryOperation {
  match op {
    UnaryOperator::Not => UnaryOperation::Not,
    UnaryOperator::Negate => UnaryOperation::Neg,
    UnaryOperator::BitNot => UnaryOperation::BitNot,
    _ => UnaryOperation::Neg,
  }
}

fn convert_assignment_op(op: &ASTAssignmentOperator) -> BinaryOperation {
  match op {
    ASTAssignmentOperator::Assign => BinaryOperation::Add,
    ASTAssignmentOperator::AddAssign => BinaryOperation::Add,
    ASTAssignmentOperator::SubAssign => BinaryOperation::Sub,
    ASTAssignmentOperator::MulAssign => BinaryOperation::Mul,
    ASTAssignmentOperator::DivAssign => BinaryOperation::Div,
    ASTAssignmentOperator::ModAssign => BinaryOperation::Mod,
    ASTAssignmentOperator::ShiftLeftAssign => BinaryOperation::BitShiftLeft,
    ASTAssignmentOperator::ShiftRightAssign => BinaryOperation::BitShiftRight,
    _ => BinaryOperation::Add,
  }
}
