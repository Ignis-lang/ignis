use ignis_type::{span::Span, symbol::SymbolId};

/// A single item in an import statement with its span for hover support.
#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct ASTImportItem {
  pub name: SymbolId,
  pub span: Span,
}

impl ASTImportItem {
  pub fn new(
    name: SymbolId,
    span: Span,
  ) -> Self {
    Self { name, span }
  }
}

/// import a, b, c from "path";
#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct ASTImport {
  pub items: Vec<ASTImportItem>,
  pub from: String,
  pub span: Span,
}

impl ASTImport {
  pub fn new(
    items: Vec<ASTImportItem>,
    from: String,
    span: Span,
  ) -> Self {
    Self { items, from, span }
  }
}
