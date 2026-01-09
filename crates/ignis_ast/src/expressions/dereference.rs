use ignis_type::span::Span;

use crate::NodeId;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ASTDereference {
  pub inner: NodeId,
  pub span: Span,
}

impl ASTDereference {
  pub fn new(
    inner: NodeId,
    span: Span,
  ) -> Self {
    ASTDereference { inner, span }
  }
}
