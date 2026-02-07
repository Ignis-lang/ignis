use ignis_type::{span::Span, symbol::SymbolId};

use crate::type_::IgnisTypeSyntax;

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub struct ASTBuiltinCall {
  pub name: SymbolId,
  pub type_args: Option<Vec<IgnisTypeSyntax>>,
  pub args: Vec<crate::NodeId>,
  pub span: Span,
}

impl ASTBuiltinCall {
  pub fn new(
    name: SymbolId,
    type_args: Option<Vec<IgnisTypeSyntax>>,
    args: Vec<crate::NodeId>,
    span: Span,
  ) -> Self {
    Self { name, type_args, args, span }
  }
}
