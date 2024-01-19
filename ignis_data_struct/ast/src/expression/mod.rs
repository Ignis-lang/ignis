use std::fmt::{Display, Formatter};

use serde_json::json;

use self::{
  binary::Binary, grouping::Grouping, literal::Literal, unary::Unary, variable::VariableExpression,
  logical::Logical, assign::Assign, ternary::Ternary, call::Call, array::Array, get::Get,
  new::NewExpression, set::Set, method_call::MethodCall, array_access::ArrayAccess,
};

use super::visitor::Visitor;

pub mod get;
pub mod method_call;
pub mod new;
pub mod set;

pub mod array;
pub mod array_access;
pub mod assign;
pub mod binary;
pub mod call;
pub mod grouping;
pub mod literal;
pub mod logical;
pub mod ternary;
pub mod unary;
pub mod variable;

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
  Binary(Binary),
  Grouping(Grouping),
  Literal(Literal),
  Unary(Unary),
  Variable(VariableExpression),
  Assign(Assign),
  Logical(Logical),
  Ternary(Ternary),
  Call(Call),
  Array(Array),
  ArrayAccess(ArrayAccess),
  Get(Get),
  Set(Set),
  New(NewExpression),
  MethodCall(MethodCall),
}

impl Expression {
  pub fn accept<R>(&self, visitor: &mut dyn Visitor<R>) -> R {
    match self {
      Expression::Grouping(grouping) => visitor.visit_grouping_expression(grouping),
      Expression::Binary(binary) => visitor.visit_binary_expression(binary),
      Expression::Literal(literal) => visitor.visit_literal_expression(literal),
      Expression::Unary(unary) => visitor.visit_unary_expression(unary),
      Expression::Variable(variable) => visitor.visit_variable_expression(variable),
      Expression::Assign(assign) => visitor.visit_assign_expression(assign),
      Expression::Logical(logical) => visitor.visit_logical_expression(logical),
      Expression::Ternary(ternary) => visitor.visit_ternary_expression(ternary),
      Expression::Call(call) => visitor.visit_call_expression(call),
      Expression::Array(array) => visitor.visit_array_expression(array),
      Expression::ArrayAccess(array) => visitor.visit_array_access_expression(array),
      Expression::Get(get) => visitor.visit_get_expression(get),
      Expression::New(new) => visitor.visit_new_expression(new),
      Expression::Set(set) => visitor.visit_set_expression(set),
      Expression::MethodCall(method) => visitor.visit_method_call_expression(method),
    }
  }

  pub fn to_json(&self) -> serde_json::Value {
    match self {
      Expression::Binary(binary) => {
        json!({
          "type": "Binary",
          "left": binary.left.to_json(),
          "operator": binary.operator.span.literal,
          "right": binary.right.to_json(),
        })
      }
      Expression::Grouping(grouping) => {
        json!({
          "type": "Grouping",
          "expression": grouping.expression.to_json(),
        })
      }
      Expression::Literal(literal) => {
        json!({
          "type": "Literal",
          "value": literal.value.to_string(),
        })
      }
      Expression::Unary(unary) => {
        json!({
          "type": "Unary",
          "operator": unary.operator.span.literal,
          "right": unary.right.to_json(),
          "data_type": unary.data_type.to_string(),
          "is_prefix": unary.is_prefix
        })
      }
      Expression::Variable(variable) => {
        json!({
          "type": "Variable",
          "name": variable.name.span.literal,
          "data_type": variable.data_type.to_string(),
        })
      }
      Expression::Assign(assign) => {
        json!({
          "type": "Assign",
          "name": assign.name.span.literal,
          "value": assign.value.to_json(),
          "data_type": assign.data_type.to_string(),
        })
      }
      Expression::Logical(logical) => {
        json!({
          "type": "Logical",
          "left": logical.left.to_json(),
          "operator": logical.operator.span.literal,
          "right": logical.right.to_json(),
          "data_type": logical.data_type.to_string(),
        })
      }
      Expression::Ternary(ternary) => {
        json!({
          "type": "Ternary",
          "condition": ternary.condition.to_json(),
          "then_branch": ternary.then_branch.to_json(),
          "else_branch": ternary.else_branch.to_json(),
          "data_type": ternary.data_type.to_string(),
        })
      }
      Expression::Call(call) => {
        json!({
          "type": "Call",
          "callee": call.callee.to_json(),
          "arguments": call.arguments.iter().map(|x| x.to_json()).collect::<Vec<serde_json::Value>>(),
          "return_type": call.return_type.to_string(),
        })
      }
      Expression::Array(array) => {
        json!({
          "type": "Array",
          "elements": array.elements.iter().map(|x| x.to_json()).collect::<Vec<serde_json::Value>>(),
          "data_type": array.data_type.to_string(),
        })
      }
      Expression::ArrayAccess(array) => {
        json!({
          "type": "ArrayAccess",
          "name": array.name.to_json(),
          "index": array.index.to_json(),
        })
      }
      Expression::Get(get) => {
        json!({
          "type": "Get",
          "object": get.object.to_json(),
          "name": get.name.span.literal,
        })
      }
      Expression::New(new) => {
        json!({
          "type": "New",
          "class_name": new.name.span.literal,
          "arguments": new.arguments.iter().map(|x| x.to_json()).collect::<Vec<serde_json::Value>>(),
        })
      }
      Expression::Set(set) => {
        json!({
          "type": "Set",
          "name": set.name.span.literal,
          "value": set.value.to_json(),
          "object": set.object.to_json(),
          "data_type": set.data_type.to_string(),
        })
      }
      Expression::MethodCall(method) => {
        json!({
          "type": "MethodCall",
          "name": method.name.span.literal,
          "arguments": method.calle.to_json(),
          "data_type": method.data_type.to_string(),
          "instance": method.instance.to_json(),
        })
      }
    }
  }
}

impl Display for Expression {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    match self {
      Expression::Binary(Binary {
        left,
        operator,
        right,
        data_type,
      }) => write!(
        f,
        "({} {} {}): {}",
        operator.span.literal, left, right, data_type,
      ),
      Expression::Grouping(Grouping { expression }) => {
        write!(f, "(group {})", *expression)
      }
      Expression::Literal(Literal { value }) => write!(f, "{}", value),
      Expression::Unary(Unary {
        operator,
        right,
        data_type,
        is_prefix,
      }) => {
        let operator_str = operator.span.literal.clone();
        let right_str = (*right).to_string();
        write!(
          f,
          "({} {}): {} | {}",
          operator_str, right_str, data_type, is_prefix
        )
      }
      Expression::Variable(VariableExpression { name, data_type }) => {
        write!(f, "{:?}: {:?}", name, data_type)
      }
      Expression::Assign(Assign { name, value, .. }) => {
        write!(f, "{} = {}", name.span.literal, value)
      }
      Expression::Logical(Logical {
        left,
        operator,
        right,
        ..
      }) => write!(f, "({} {} {})", left, operator.span.literal, right),
      Expression::Ternary(Ternary {
        condition,
        then_branch,
        else_branch,
        ..
      }) => write!(f, "({} ? {} : {})", condition, then_branch, else_branch),
      Expression::Call(call) => write!(
        f,
        "fn {}({})",
        call.callee,
        call
          .arguments
          .iter()
          .map(|x| x.to_string())
          .collect::<Vec<String>>()
          .join(", ")
      ),
      Expression::Array(array) => {
        write!(
          f,
          "[{}]",
          array
            .elements
            .iter()
            .map(|x| x.to_string())
            .collect::<Vec<String>>()
            .join(", ")
        )
      }
      Expression::ArrayAccess(array) => {
        write!(f, "{}[{}]", array.name, array.index)
      }
      Expression::Get(get) => {
        write!(f, "{}.{}", get.object, get.name.span.literal)
      }
      Expression::New(new) => {
        write!(
          f,
          "new {}({})",
          new.name.span.literal,
          new
            .arguments
            .iter()
            .map(|x| x.to_string())
            .collect::<Vec<String>>()
            .join(", ")
        )
      }
      Expression::Set(set) => {
        write!(
          f,
          "{}.{} = {}",
          set.object, set.name.span.literal, set.value
        )
      }
      Expression::MethodCall(method) => {
        write!(f, "{}.{}{}", method.instance, method.name, method.calle,)
      }
    }
  }
}
