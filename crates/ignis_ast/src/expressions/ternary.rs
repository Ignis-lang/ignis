use ignis_type::span::Span;

use crate::NodeId;

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct ASTTernary {
  pub condition: NodeId,
  pub then_expr: NodeId,
  pub else_expr: NodeId,
  pub span: Span,
}

impl ASTTernary {
  pub fn new(
    condition: NodeId,
    then_expr: NodeId,
    else_expr: NodeId,
    span: Span,
  ) -> Self {
    Self {
      condition,
      then_expr,
      else_expr,
      span,
    }
  }
}
