use ignis_type::span::Span;

use crate::NodeId;

/// extern <function|const>
#[derive(Debug, Clone, PartialEq)]
pub struct ASTExtern {
  pub item: NodeId,
  pub span: Span,
}

impl ASTExtern {
  pub fn new(
    item: NodeId,
    span: Span,
  ) -> Self {
    Self { item, span }
  }
}
