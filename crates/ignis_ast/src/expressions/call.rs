use ignis_type::span::Span;

use crate::NodeId;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ASTCallExpression {
  pub callee: NodeId,
  pub span: Span,
  pub arguments: Vec<NodeId>,
}

impl ASTCallExpression {
  pub fn new(
    callee: NodeId,
    span: Span,
    arguments: Vec<NodeId>,
  ) -> Self {
    ASTCallExpression {
      span,
      callee,
      arguments,
    }
  }
}
