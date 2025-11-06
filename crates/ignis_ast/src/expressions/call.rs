use ignis_type::{span::Span, symbol::SymbolId};

use crate::NodeId;

#[derive(Debug, Clone, PartialEq)]
pub struct ASTFunctionCall {
  pub name: SymbolId,
  pub span: Span,
  pub callee: NodeId,
  pub arguments: Vec<NodeId>,
}

impl ASTFunctionCall {
  pub fn new(
    name: SymbolId,
    span: Span,
    callee: NodeId,
    arguments: Vec<NodeId>,
  ) -> Self {
    ASTFunctionCall {
      name,
      span,
      callee,
      arguments,
    }
  }
}
