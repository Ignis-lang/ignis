use ignis_type::{span::Span, symbol::SymbolId};

/// A single generic type parameter (e.g., `T` in `Vec<T>`)
#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct ASTGenericParam {
  pub name: SymbolId,
  pub span: Span,
}

impl ASTGenericParam {
  pub fn new(
    name: SymbolId,
    span: Span,
  ) -> Self {
    Self { name, span }
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
