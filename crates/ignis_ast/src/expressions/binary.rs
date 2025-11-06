use ignis_type::span::Span;

use crate::NodeId;

#[derive(Debug, PartialEq, Clone)]
pub enum ASTBinaryOperator {
  Add,
  And,
  BitAnd,
  BitOr,
  BitXor,
  Divide,
  Equal,
  GreaterThan,
  GreaterThanOrEqual,
  LessThan,
  LessThanOrEqual,
  Modulo,
  Multiply,
  NotEqual,
  Or,
  ShiftLeft,
  ShiftRight,
  Subtract,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ASTBinary {
  pub left: NodeId,
  pub right: NodeId,
  pub operator: ASTBinaryOperator,
  pub span: Span,
}

impl ASTBinary {
  pub fn new(
    left: NodeId,
    right: NodeId,
    operator: ASTBinaryOperator,
    span: Span,
  ) -> Self {
    Self {
      left,
      right,
      operator,
      span,
    }
  }
}
