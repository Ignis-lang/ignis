use ignis_type::{span::Span, symbol::SymbolId};

#[derive(Debug, Clone, PartialEq)]
pub struct ASTImport {
  pub path: Span,
  pub module: SymbolId,
  pub alias: Option<SymbolId>,
}

impl ASTImport {
  pub fn new(
    path: Span,
    module: SymbolId,
    alias: Option<SymbolId>,
  ) -> Self {
    Self { path, module, alias }
  }
}
