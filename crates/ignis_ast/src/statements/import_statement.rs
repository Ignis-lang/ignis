use ignis_type::{span::Span, symbol::SymbolId};

/// import a, b, c from "path";
#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct ASTImport {
  pub items: Vec<SymbolId>,
  pub from: String,
  pub span: Span,
}

impl ASTImport {
  pub fn new(
    items: Vec<SymbolId>,
    from: String,
    span: Span,
  ) -> Self {
    Self { items, from, span }
  }
}
