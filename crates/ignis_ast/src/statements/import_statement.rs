use ignis_type::{span::Span, symbol::SymbolId};

/// Discriminates between a named import (`import X from "..."`) and a
/// discard import (`import _ from "..."`) that loads a module purely for
/// its side effects (e.g. namespace contributions).
#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub enum ImportItemKind {
  Named(SymbolId),
  Discard,
}

/// A single item in an import statement with its span for hover support.
#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct ASTImportItem {
  pub kind: ImportItemKind,
  pub span: Span,
}

impl ASTImportItem {
  pub fn named(
    name: SymbolId,
    span: Span,
  ) -> Self {
    Self {
      kind: ImportItemKind::Named(name),
      span,
    }
  }

  pub fn discard(span: Span) -> Self {
    Self {
      kind: ImportItemKind::Discard,
      span,
    }
  }
}

/// import a, b, c from "path";
#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct ASTImport {
  pub items: Vec<ASTImportItem>,
  pub from: String,
  /// Span of the "path" string literal (for Go to Definition on import path)
  pub from_span: Span,
  pub span: Span,
}

impl ASTImport {
  pub fn new(
    items: Vec<ASTImportItem>,
    from: String,
    from_span: Span,
    span: Span,
  ) -> Self {
    Self {
      items,
      from,
      from_span,
      span,
    }
  }
}
