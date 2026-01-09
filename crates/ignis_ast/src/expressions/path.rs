use ignis_type::{span::Span, symbol::SymbolId};

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub struct ASTPath {
  pub segments: Vec<SymbolId>,
  pub span: Span,
}

impl ASTPath {
  pub fn new(
    segments: Vec<SymbolId>,
    span: Span,
  ) -> Self {
    Self { segments, span }
  }
}
