use ignis_type::span::Span;

use crate::NodeId;

#[derive(Debug, Clone, PartialEq)]
pub struct ASTBlock {
  pub statements: Vec<NodeId>,
  pub span: Span,
}

impl ASTBlock {
  pub fn new(statements: Vec<NodeId>, span: Span) -> Self {
    Self { statements, span }
  }
}
