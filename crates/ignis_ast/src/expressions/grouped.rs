use ignis_type::span::Span;

use crate::NodeId;

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub struct ASTGrouped {
  pub expression: NodeId,
  pub span: Span,
}

impl ASTGrouped {
  pub fn new(
    expression: NodeId,
    span: Span,
  ) -> Self {
    Self { expression, span }
  }
}
