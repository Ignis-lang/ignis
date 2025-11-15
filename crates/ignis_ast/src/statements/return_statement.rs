use ignis_type::span::Span;

use crate::NodeId;

#[derive(Debug, Clone, PartialEq)]
pub struct ASTReturn {
  pub expression: Option<NodeId>,
  pub span: Span,
}

impl ASTReturn {
  pub fn new(expression: Option<NodeId>, span: Span) -> Self {
    Self { expression, span }
  }
}
