use ignis_type::span::Span;

use crate::NodeId;

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub struct ASTTuple {
  pub elements: Vec<NodeId>,
  pub span: Span,
}

impl ASTTuple {
  pub fn new(
    elements: Vec<NodeId>,
    span: Span,
  ) -> Self {
    Self { elements, span }
  }
}
