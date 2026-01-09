use ignis_type::span::Span;

use crate::NodeId;

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct ASTReference {
  pub inner: NodeId,
  pub mutable: bool,
  pub span: Span,
}

impl ASTReference {
  pub fn new(
    inner: NodeId,
    mutable: bool,
    span: Span,
  ) -> Self {
    ASTReference { inner, mutable, span }
  }
}
