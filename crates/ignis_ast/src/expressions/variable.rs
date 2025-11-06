use ignis_type::{span::Span, symbol::SymbolId};

#[derive(Debug, Clone, PartialEq)]
pub struct ASTVariableExpression {
  pub name: SymbolId,
  pub span: Span,
}

impl ASTVariableExpression {
  pub fn new(
    name: SymbolId,
    span: Span,
  ) -> Self {
    Self { name, span }
  }
}
