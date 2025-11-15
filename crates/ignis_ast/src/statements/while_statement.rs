use ignis_type::span::Span;

use crate::NodeId;

#[derive(Debug, Clone, PartialEq)]
pub struct ASTWhile {
  pub condition: NodeId,
  pub body: NodeId,
  pub span: Span,
}

impl ASTWhile {
  pub fn new(
    condition: NodeId,
    body: NodeId,
    span: Span,
  ) -> Self {
    Self { condition, body, span }
  }
}
