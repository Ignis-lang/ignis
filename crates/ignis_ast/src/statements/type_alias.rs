use ignis_type::{span::Span, symbol::SymbolId};

use crate::type_::IgnisTypeSyntax;

/// Type alias declaration: `type Name = <type>;`
#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct ASTTypeAlias {
  pub name: SymbolId,
  pub target: IgnisTypeSyntax,
  pub span: Span,
}

impl ASTTypeAlias {
  pub fn new(
    name: SymbolId,
    target: IgnisTypeSyntax,
    span: Span,
  ) -> Self {
    Self { name, target, span }
  }
}
