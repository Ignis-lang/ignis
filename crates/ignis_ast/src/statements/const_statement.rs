use ignis_type::{span::Span, symbol::SymbolId};

use crate::{NodeId, type_::IgnisTypeSyntax};

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct ASTConstant {
  pub name: SymbolId,
  pub ty: IgnisTypeSyntax,
  pub value: Option<NodeId>,
  pub span: Span,
}

impl ASTConstant {
  pub fn new(
    name: SymbolId,
    ty: IgnisTypeSyntax,
    value: Option<NodeId>,
    span: Span,
  ) -> Self {
    Self { name, ty, value, span }
  }
}
