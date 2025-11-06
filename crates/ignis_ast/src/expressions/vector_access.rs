use ignis_type::{span::Span, symbol::SymbolId};

use crate::NodeId;

#[derive(Debug, PartialEq, Clone)]
pub struct ASTVectorAccess {
  pub name: SymbolId,
  pub span: Span,
  pub index: NodeId,
}

impl ASTVectorAccess {
  pub fn new(
    name: SymbolId,
    span: Span,
    index: NodeId,
  ) -> Self {
    Self { name, span, index }
  }
}
