use ignis_type::span::Span;

use crate::NodeId;

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub struct ASTVector {
  pub span: Span,
  pub items: Vec<NodeId>,
}

impl ASTVector {
  pub fn new(
    span: Span,
    items: Vec<NodeId>,
  ) -> Self {
    Self { span, items }
  }
}
