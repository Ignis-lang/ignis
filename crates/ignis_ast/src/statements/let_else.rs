use ignis_type::span::Span;

use crate::{NodeId, pattern::ASTPattern, type_::IgnisTypeSyntax};

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct ASTLetElse {
  pub pattern: ASTPattern,
  pub binding_type: Option<IgnisTypeSyntax>,
  pub value: NodeId,
  pub else_block: NodeId,
  pub span: Span,
}

impl ASTLetElse {
  pub fn new(
    pattern: ASTPattern,
    binding_type: Option<IgnisTypeSyntax>,
    value: NodeId,
    else_block: NodeId,
    span: Span,
  ) -> Self {
    Self {
      pattern,
      binding_type,
      value,
      else_block,
      span,
    }
  }
}
