use ignis_type::span::Span;

use crate::NodeId;

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub struct ASTVectorAccess {
  pub name: NodeId,
  pub span: Span,
  pub index: NodeId,
}

impl ASTVectorAccess {
  pub fn new(
    name: NodeId,
    span: Span,
    index: NodeId,
  ) -> Self {
    Self { name, span, index }
  }
}
