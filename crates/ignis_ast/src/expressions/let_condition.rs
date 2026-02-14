use ignis_type::span::Span;

use crate::{NodeId, pattern::ASTPattern};

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct ASTLetCondition {
  pub pattern: ASTPattern,
  pub value: NodeId,
  pub span: Span,
}

impl ASTLetCondition {
  pub fn new(
    pattern: ASTPattern,
    value: NodeId,
    span: Span,
  ) -> Self {
    Self { pattern, value, span }
  }
}
