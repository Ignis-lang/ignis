use ignis_type::{span::Span, symbol::SymbolId};

/// A single generic bound path (e.g., `Hash` or `foo::bar::Eq` in `T: Hash & foo::bar::Eq`)
#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct ASTGenericBound {
  pub segments: Vec<SymbolId>,
  pub span: Span,
}

impl ASTGenericBound {
  pub fn new(
    segments: Vec<SymbolId>,
    span: Span,
  ) -> Self {
    Self { segments, span }
  }
}

/// A single generic type parameter (e.g., `T` in `Vec<T>`)
#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct ASTGenericParam {
  pub name: SymbolId,
  pub bounds: Vec<ASTGenericBound>,
  pub span: Span,
}

impl ASTGenericParam {
  pub fn new(
    name: SymbolId,
    bounds: Vec<ASTGenericBound>,
    span: Span,
  ) -> Self {
    Self { name, bounds, span }
  }
}

/// A list of generic type parameters (e.g., `<T, U>` in `function foo<T, U>`)
#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct ASTGenericParams {
  pub params: Vec<ASTGenericParam>,
  pub span: Span,
}

impl ASTGenericParams {
  pub fn new(
    params: Vec<ASTGenericParam>,
    span: Span,
  ) -> Self {
    Self { params, span }
  }

  pub fn is_empty(&self) -> bool {
    self.params.is_empty()
  }

  pub fn len(&self) -> usize {
    self.params.len()
  }
}
