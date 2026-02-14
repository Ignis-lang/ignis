use ignis_type::span::Span;

use crate::{NodeId, pattern::ASTPattern};

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct ASTLetElse {
  pub pattern: ASTPattern,
  pub value: NodeId,
  pub else_block: NodeId,
  pub span: Span,
}

impl ASTLetElse {
  pub fn new(
    pattern: ASTPattern,
    value: NodeId,
    else_block: NodeId,
    span: Span,
  ) -> Self {
    Self {
      pattern,
      value,
      else_block,
      span,
    }
  }
}
