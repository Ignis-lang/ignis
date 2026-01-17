use ignis_type::{span::Span, symbol::SymbolId};

/// A segment in a path expression with its span for hover support.
#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub struct ASTPathSegment {
  pub name: SymbolId,
  pub span: Span,
}

impl ASTPathSegment {
  pub fn new(
    name: SymbolId,
    span: Span,
  ) -> Self {
    Self { name, span }
  }
}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub struct ASTPath {
  pub segments: Vec<ASTPathSegment>,
  pub span: Span,
}

impl ASTPath {
  pub fn new(
    segments: Vec<ASTPathSegment>,
    span: Span,
  ) -> Self {
    Self { segments, span }
  }
}
