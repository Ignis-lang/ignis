use ignis_type::{span::Span, symbol::SymbolId};

use crate::{attribute::ASTAttribute, NodeId};

/// extern path { function ...; const ...; }
#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct ASTExtern {
  pub attrs: Vec<ASTAttribute>,
  pub path: Vec<SymbolId>,
  pub items: Vec<NodeId>,
  pub span: Span,
  pub doc: Option<String>,
}

impl ASTExtern {
  pub fn new(
    attrs: Vec<ASTAttribute>,
    path: Vec<SymbolId>,
    items: Vec<NodeId>,
    span: Span,
    doc: Option<String>,
  ) -> Self {
    Self {
      attrs,
      path,
      items,
      span,
      doc,
    }
  }
}
