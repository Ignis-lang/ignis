use ignis_type::span::Span;

use crate::NodeId;

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct ASTDefer {
  pub expression: NodeId,
  pub span: Span,
}

impl ASTDefer {
  pub fn new(
    expression: NodeId,
    span: Span,
  ) -> Self {
    Self { expression, span }
  }
}
