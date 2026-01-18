use ignis_type::{span::Span, symbol::SymbolId};

use crate::NodeId;

/// extern path { function ...; const ...; }
#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct ASTExtern {
  pub path: Vec<SymbolId>,
  pub items: Vec<NodeId>,
  pub span: Span,
  pub doc: Option<String>,
}

impl ASTExtern {
  pub fn new(
    path: Vec<SymbolId>,
    items: Vec<NodeId>,
    span: Span,
    doc: Option<String>,
  ) -> Self {
    Self { path, items, span, doc }
  }
}
