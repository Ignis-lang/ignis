use crate::{Analyzer, ScopeKind};
use ignis_ast::{expressions::ASTExpression, statements::ASTStatement, ASTNode, NodeId};
use ignis_ast::expressions::binary::ASTBinaryOperator;
use ignis_ast::expressions::unary::UnaryOperator;
use ignis_type::definition::{ConstValue, DefinitionKind};
use ignis_type::value::IgnisLiteralValue;
use ordered_float::OrderedFloat;

impl<'a> Analyzer<'a> {
  pub fn const_eval_phase(
    &mut self,
    roots: &Vec<NodeId>,
  ) {
    self.reset_scopes(roots);

    for root in roots {
      self.const_eval_node(root, ScopeKind::Global);
    }
  }

  fn const_eval_node(
    &mut self,
    node_id: &NodeId,
    scope_kind: ScopeKind,
  ) {
    let node = self.ast.get(node_id);

    match node {
      ASTNode::Statement(stmt) => self.const_eval_statement(node_id, stmt, scope_kind),
      ASTNode::Expression(_) => {},
    }
  }

  fn const_eval_statement(
    &mut self,
    node_id: &NodeId,
    stmt: &ASTStatement,
    scope_kind: ScopeKind,
  ) {
    match stmt {
      ASTStatement::Constant(const_) => {
        // Only eval if value exists (not for extern const)
        if let Some(value_id) = &const_.value
          && let Some(value) = self.const_eval_expression_node(value_id, scope_kind)
          && let Some(def_id) = self.lookup_def(node_id)
          && let DefinitionKind::Constant(const_def) = &mut self.defs.get_mut(&def_id.clone()).kind
        {
          const_def.value = Some(value);
        }
        self.define_decl_in_current_scope(node_id);
      },
      ASTStatement::Block(block) => {
        self.scopes.push(ScopeKind::Block);

        for stmt_id in &block.statements {
          self.const_eval_node(stmt_id, ScopeKind::Block);
        }

        self.scopes.pop();
      },
      ASTStatement::If(if_stmt) => {
        self.const_eval_node(&if_stmt.condition, scope_kind);
        self.const_eval_node(&if_stmt.then_block, ScopeKind::Block);

        if let Some(else_branch) = &if_stmt.else_block {
          self.const_eval_node(else_branch, ScopeKind::Block);
        }
      },
      ASTStatement::While(while_stmt) => {
        self.scopes.push(ScopeKind::Loop);

        self.const_eval_node(&while_stmt.condition, ScopeKind::Loop);
        self.const_eval_node(&while_stmt.body, ScopeKind::Loop);

        self.scopes.pop();
      },
      ASTStatement::For(for_stmt) => {
        self.scopes.push(ScopeKind::Loop);

        self.const_eval_node(&for_stmt.initializer, ScopeKind::Loop);
        self.const_eval_node(&for_stmt.condition, ScopeKind::Loop);
        self.const_eval_node(&for_stmt.increment, ScopeKind::Loop);
        self.const_eval_node(&for_stmt.body, ScopeKind::Loop);

        self.scopes.pop();
      },
      ASTStatement::ForOf(for_of) => {
        with_for_of_scope!(self, node_id, for_of, {
          self.const_eval_node(&for_of.iter, ScopeKind::Loop);
          self.const_eval_node(&for_of.body, ScopeKind::Loop);
        });
      },
      ASTStatement::Variable(var) => {
        if let Some(value_id) = &var.value {
          self.const_eval_node(value_id, scope_kind);
        }

        self.define_decl_in_current_scope(node_id);
      },
      ASTStatement::Function(func) => {
        if let Some(body_id) = &func.body {
          self.const_eval_node(body_id, ScopeKind::Function);
        }
      },
      ASTStatement::Extern(extern_stmt) => {
        for item in &extern_stmt.items {
          self.const_eval_node(item, scope_kind);
        }
      },
      ASTStatement::Namespace(ns_stmt) => {
        for item in &ns_stmt.items {
          self.const_eval_node(item, scope_kind);
        }
      },
      ASTStatement::Export(ignis_ast::statements::ASTExport::Declaration { decl, .. }) => {
        self.const_eval_node(decl, scope_kind);
      },
      ASTStatement::Record(rec) => {
        self.const_eval_record_static_fields(node_id, rec, scope_kind);
      },
      ASTStatement::Enum(enum_) => {
        self.const_eval_enum_static_fields(node_id, enum_, scope_kind);
      },
      _ => {},
    }
  }

  fn const_eval_record_static_fields(
    &mut self,
    node_id: &NodeId,
    rec: &ignis_ast::statements::record::ASTRecord,
    scope_kind: ScopeKind,
  ) {
    use ignis_ast::statements::record::ASTRecordItem;
    use ignis_diagnostics::message::DiagnosticMessage;

    let record_def_id = match self.lookup_def(node_id) {
      Some(id) => *id,
      None => return,
    };

    let type_name = self.get_symbol_name(&rec.name);

    for item in &rec.items {
      if let ASTRecordItem::Field(field) = item
        && field.is_static()
        && let Some(value_id) = &field.value
      {
        match self.const_eval_expression_node(value_id, scope_kind) {
          Some(value) => {
            // Get the static field's definition from the record
            if let DefinitionKind::Record(rd) = &self.defs.get(&record_def_id).kind
              && let Some(const_def_id) = rd.static_fields.get(&field.name).cloned()
              && let DefinitionKind::Constant(const_def) = &mut self.defs.get_mut(&const_def_id).kind
            {
              const_def.value = Some(value);
            }
          },
          None => {
            let field_name = self.get_symbol_name(&field.name);
            self.add_diagnostic(
              DiagnosticMessage::StaticFieldNotConst {
                field: field_name,
                type_name: type_name.clone(),
                span: field.span.clone(),
              }
              .report(),
            );
          },
        }
      }
    }
  }

  fn const_eval_enum_static_fields(
    &mut self,
    node_id: &NodeId,
    enum_: &ignis_ast::statements::enum_::ASTEnum,
    scope_kind: ScopeKind,
  ) {
    use ignis_ast::statements::enum_::ASTEnumItem;
    use ignis_diagnostics::message::DiagnosticMessage;

    let enum_def_id = match self.lookup_def(node_id) {
      Some(id) => *id,
      None => return,
    };

    let type_name = self.get_symbol_name(&enum_.name);

    for item in &enum_.items {
      if let ASTEnumItem::Field(field) = item
        && let Some(value_id) = &field.value
      {
        match self.const_eval_expression_node(value_id, scope_kind) {
          Some(value) => {
            // Get the static field's definition from the enum
            if let DefinitionKind::Enum(ed) = &self.defs.get(&enum_def_id).kind
              && let Some(const_def_id) = ed.static_fields.get(&field.name).cloned()
              && let DefinitionKind::Constant(const_def) = &mut self.defs.get_mut(&const_def_id).kind
            {
              const_def.value = Some(value);
            }
          },
          None => {
            let field_name = self.get_symbol_name(&field.name);
            self.add_diagnostic(
              DiagnosticMessage::StaticFieldNotConst {
                field: field_name,
                type_name: type_name.clone(),
                span: field.span.clone(),
              }
              .report(),
            );
          },
        }
      }
    }
  }

  pub(crate) fn const_eval_expression_node(
    &self,
    node_id: &NodeId,
    _scope_kind: ScopeKind,
  ) -> Option<ConstValue> {
    let node = self.ast.get(node_id);

    match node {
      ASTNode::Expression(expr) => match expr {
        ASTExpression::Literal(lit) => Some(const_value_from_literal(&lit.value)),
        ASTExpression::Variable(var) => {
          if let Some(def_id) = self.scopes.lookup_def(&var.name) {
            if let DefinitionKind::Constant(const_def) = &self.defs.get(def_id).kind {
              const_def.value.clone()
            } else {
              None
            }
          } else {
            None
          }
        },
        ASTExpression::Path(path) => path
          .segments
          .last()
          .and_then(|last| self.scopes.lookup_def(&last.name))
          .and_then(|def_id| match &self.defs.get(def_id).kind {
            DefinitionKind::Constant(const_def) => const_def.value.clone(),
            _ => None,
          }),
        ASTExpression::Binary(binary) => {
          let left = self.const_eval_expression_node(&binary.left, _scope_kind);
          let right = self.const_eval_expression_node(&binary.right, _scope_kind);
          const_eval_binary(&binary.operator, left, right)
        },
        ASTExpression::Ternary(ternary) => {
          let condition = self.const_eval_expression_node(&ternary.condition, _scope_kind)?;

          match condition {
            ConstValue::Bool(true) => self.const_eval_expression_node(&ternary.then_expr, _scope_kind),
            ConstValue::Bool(false) => self.const_eval_expression_node(&ternary.else_expr, _scope_kind),
            _ => None,
          }
        },
        ASTExpression::Unary(unary) => {
          let operand = self.const_eval_expression_node(&unary.operand, _scope_kind);
          const_eval_unary(&unary.operator, operand)
        },
        ASTExpression::Vector(vector) => {
          let values: Option<Vec<ConstValue>> = vector
            .items
            .iter()
            .map(|item| self.const_eval_expression_node(item, _scope_kind))
            .collect();
          values.map(ConstValue::Array)
        },
        ASTExpression::VectorAccess(access) => {
          let array = self.const_eval_expression_node(&access.name, _scope_kind)?;
          let index = self.const_eval_expression_node(&access.index, _scope_kind)?;

          match (array, index) {
            (ConstValue::Array(arr), ConstValue::Int(i)) => {
              if i < 0 {
                None
              } else {
                let idx = i as usize;
                arr.get(idx).cloned()
              }
            },
            _ => None,
          }
        },
        ASTExpression::Grouped(grouped) => self.const_eval_expression_node(&grouped.expression, _scope_kind),
        _ => None,
      },
      ASTNode::Statement(_) => None,
    }
  }
}

fn const_value_from_literal(value: &IgnisLiteralValue) -> ConstValue {
  match value {
    IgnisLiteralValue::Int8(i) => ConstValue::Int(*i as i64),
    IgnisLiteralValue::Int16(i) => ConstValue::Int(*i as i64),
    IgnisLiteralValue::Int32(i) => ConstValue::Int(*i as i64),
    IgnisLiteralValue::Int64(i) => ConstValue::Int(*i),
    IgnisLiteralValue::UnsignedInt8(i) => ConstValue::Int(*i as i64),
    IgnisLiteralValue::UnsignedInt16(i) => ConstValue::Int(*i as i64),
    IgnisLiteralValue::UnsignedInt32(i) => ConstValue::Int(*i as i64),
    IgnisLiteralValue::UnsignedInt64(i) => ConstValue::Int(*i as i64),
    IgnisLiteralValue::Float32(f) => ConstValue::Float(OrderedFloat::from(f.into_inner() as f64)),
    IgnisLiteralValue::Float64(f) => ConstValue::Float(*f),
    IgnisLiteralValue::Boolean(b) => ConstValue::Bool(*b),
    IgnisLiteralValue::Char(c) => ConstValue::Char(*c),
    IgnisLiteralValue::String(s) => ConstValue::String(s.clone()),
    _ => ConstValue::Null,
  }
}

fn const_eval_binary(
  op: &ASTBinaryOperator,
  left: Option<ConstValue>,
  right: Option<ConstValue>,
) -> Option<ConstValue> {
  match (left, right) {
    (Some(ConstValue::Int(l)), Some(ConstValue::Int(r))) => match op {
      ASTBinaryOperator::Add => l.checked_add(r).map(ConstValue::Int),
      ASTBinaryOperator::Subtract => l.checked_sub(r).map(ConstValue::Int),
      ASTBinaryOperator::Multiply => l.checked_mul(r).map(ConstValue::Int),
      ASTBinaryOperator::Divide => {
        if r != 0 {
          l.checked_div(r).map(ConstValue::Int)
        } else {
          None
        }
      },
      ASTBinaryOperator::Modulo => {
        if r != 0 {
          l.checked_rem(r).map(ConstValue::Int)
        } else {
          None
        }
      },
      ASTBinaryOperator::Equal => Some(ConstValue::Bool(l == r)),
      ASTBinaryOperator::NotEqual => Some(ConstValue::Bool(l != r)),
      ASTBinaryOperator::LessThan => Some(ConstValue::Bool(l < r)),
      ASTBinaryOperator::LessThanOrEqual => Some(ConstValue::Bool(l <= r)),
      ASTBinaryOperator::GreaterThan => Some(ConstValue::Bool(l > r)),
      ASTBinaryOperator::GreaterThanOrEqual => Some(ConstValue::Bool(l >= r)),
      ASTBinaryOperator::BitAnd => Some(ConstValue::Int(l & r)),
      ASTBinaryOperator::BitOr => Some(ConstValue::Int(l | r)),
      ASTBinaryOperator::BitXor => Some(ConstValue::Int(l ^ r)),
      ASTBinaryOperator::ShiftLeft => u32::try_from(r)
        .ok()
        .and_then(|shift| l.checked_shl(shift))
        .map(ConstValue::Int),
      ASTBinaryOperator::ShiftRight => u32::try_from(r)
        .ok()
        .and_then(|shift| l.checked_shr(shift))
        .map(ConstValue::Int),
      _ => None,
    },
    (Some(ConstValue::Float(l)), Some(ConstValue::Float(r))) => {
      let result = match op {
        ASTBinaryOperator::Add => Some(ConstValue::Float(l + r)),
        ASTBinaryOperator::Subtract => Some(ConstValue::Float(l - r)),
        ASTBinaryOperator::Multiply => Some(ConstValue::Float(l * r)),
        ASTBinaryOperator::Divide => {
          if r != 0.0 {
            Some(ConstValue::Float(l / r))
          } else {
            None
          }
        },
        ASTBinaryOperator::Modulo => {
          if r != 0.0 {
            let result = l % r;
            if result.is_nan() {
              None
            } else {
              Some(ConstValue::Float(result))
            }
          } else {
            None
          }
        },
        ASTBinaryOperator::Equal => Some(ConstValue::Bool(l == r)),
        ASTBinaryOperator::NotEqual => Some(ConstValue::Bool(l != r)),
        _ => None,
      };
      result.and_then(|v| match &v {
        ConstValue::Float(f) if f.is_nan() || f.is_infinite() => None,
        _ => Some(v),
      })
    },
    (Some(ConstValue::Bool(l)), Some(ConstValue::Bool(r))) => match op {
      ASTBinaryOperator::And => Some(ConstValue::Bool(l && r)),
      ASTBinaryOperator::Or => Some(ConstValue::Bool(l || r)),
      ASTBinaryOperator::Equal => Some(ConstValue::Bool(l == r)),
      ASTBinaryOperator::NotEqual => Some(ConstValue::Bool(l != r)),
      _ => None,
    },
    _ => None,
  }
}

fn const_eval_unary(
  op: &UnaryOperator,
  operand: Option<ConstValue>,
) -> Option<ConstValue> {
  match operand {
    Some(ConstValue::Int(i)) => match op {
      UnaryOperator::Negate => i.checked_neg().map(ConstValue::Int),
      UnaryOperator::Not => Some(ConstValue::Bool(i == 0)),
      UnaryOperator::BitNot => Some(ConstValue::Int(!i)),
      _ => None,
    },
    Some(ConstValue::Float(f)) => match op {
      UnaryOperator::Negate => {
        let result = -f;
        if result.is_nan() || result.is_infinite() {
          None
        } else {
          Some(ConstValue::Float(result))
        }
      },
      _ => None,
    },
    Some(ConstValue::Bool(b)) => match op {
      UnaryOperator::Not => Some(ConstValue::Bool(!b)),
      _ => None,
    },
    _ => None,
  }
}
