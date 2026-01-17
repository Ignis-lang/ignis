use ignis_type::{span::Span, symbol::SymbolId};

use crate::{generics::ASTGenericParams, type_::IgnisTypeSyntax};

/// Type alias declaration: `type Name<T, U> = <type>;`
#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct ASTTypeAlias {
  pub name: SymbolId,
  pub type_params: Option<ASTGenericParams>,
  pub target: IgnisTypeSyntax,
  pub span: Span,
  pub doc: Option<String>,
}

impl ASTTypeAlias {
  pub fn new(
    name: SymbolId,
    type_params: Option<ASTGenericParams>,
    target: IgnisTypeSyntax,
    span: Span,
    doc: Option<String>,
  ) -> Self {
    Self {
      name,
      type_params,
      target,
      span,
      doc,
    }
  }
}
