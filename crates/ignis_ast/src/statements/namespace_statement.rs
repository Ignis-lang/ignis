use ignis_type::{span::Span, symbol::SymbolId};

use crate::NodeId;

/// namespace Foo::Bar { function ... const ... }
#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct ASTNamespace {
  pub path: Vec<SymbolId>,
  pub items: Vec<NodeId>,
  pub span: Span,
  pub doc: Option<String>,
}

impl ASTNamespace {
  pub fn new(
    path: Vec<SymbolId>,
    items: Vec<NodeId>,
    span: Span,
    doc: Option<String>,
  ) -> Self {
    Self { path, items, span, doc }
  }
}
