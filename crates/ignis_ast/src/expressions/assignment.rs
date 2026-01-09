use ignis_type::span::Span;

use crate::NodeId;

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum ASTAssignmentOperator {
  Assign,
  AddAssign,
  SubAssign,
  MulAssign,
  DivAssign,
  ModAssign,
  ShiftLeftAssign,
  ShiftRightAssign,
  BitAndAssign,
  BitOrAssign,
  BitXorAssign,
  NotAssign,
  AndAssign,
  OrAssign,
}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub struct ASTAssignment {
  pub target: NodeId,
  pub value: NodeId,
  pub operator: ASTAssignmentOperator,
  pub span: Span,
}

impl ASTAssignment {
  pub fn new(
    target: NodeId,
    value: NodeId,
    operator: ASTAssignmentOperator,
    span: Span,
  ) -> Self {
    Self {
      target,
      value,
      operator,
      span,
    }
  }
}
