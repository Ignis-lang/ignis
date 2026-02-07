use ignis_type::{span::Span, symbol::SymbolId};

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct ASTAttribute {
  pub name: SymbolId,
  pub args: Vec<ASTAttributeArg>,
  pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub enum ASTAttributeArg {
  IntLiteral(i64, Span),
  StringLiteral(String, Span),
}

impl ASTAttributeArg {
  pub fn span(&self) -> &Span {
    match self {
      ASTAttributeArg::IntLiteral(_, span) | ASTAttributeArg::StringLiteral(_, span) => span,
    }
  }
}
