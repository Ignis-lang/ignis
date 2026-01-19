use ignis_type::{span::Span, symbol::SymbolId};

use crate::NodeId;

/// namespace Foo::Bar { function ... const ... }
#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct ASTNamespace {
  pub path: Vec<SymbolId>,
  pub items: Vec<NodeId>,
  pub span: Span,
  /// Outer doc comment: `///` before the namespace declaration
  pub doc: Option<String>,
  /// Inner doc comment: `//!` inside the namespace body
  pub inner_doc: Option<String>,
}

impl ASTNamespace {
  pub fn new(
    path: Vec<SymbolId>,
    items: Vec<NodeId>,
    span: Span,
    doc: Option<String>,
    inner_doc: Option<String>,
  ) -> Self {
    Self {
      path,
      items,
      span,
      doc,
      inner_doc,
    }
  }
}
