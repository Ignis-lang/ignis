use ignis_type::span::Span;

use crate::NodeId;

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct ASTFor {
  pub initializer: NodeId,
  pub condition: NodeId,
  pub increment: NodeId,
  pub body: NodeId,
  pub span: Span,
}

impl ASTFor {
  pub fn new(
    initializer: NodeId,
    condition: NodeId,
    increment: NodeId,
    body: NodeId,
    span: Span,
  ) -> Self {
    Self {
      initializer,
      condition,
      increment,
      body,
      span,
    }
  }
}
