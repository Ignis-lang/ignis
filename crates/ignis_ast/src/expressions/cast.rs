use ignis_type::span::Span;

use crate::{NodeId, type_::IgnisTypeSyntax};

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct ASTCast {
  pub span: Span,
  pub target_type: IgnisTypeSyntax,
  pub expression: NodeId,
}

impl ASTCast {
  pub fn new(
    span: Span,
    target_type: IgnisTypeSyntax,
    expression: NodeId,
  ) -> Self {
    Self {
      span,
      target_type,
      expression,
    }
  }
}
