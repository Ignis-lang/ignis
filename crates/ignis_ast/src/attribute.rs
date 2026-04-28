use ignis_type::{span::Span, symbol::SymbolId};

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct ASTAttribute {
  pub name: SymbolId,
  pub path: Vec<SymbolId>,
  pub args: Vec<ASTAttributeArg>,
  pub named_args: Vec<ASTNamedAttributeArg>,
  pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct ASTNamedAttributeArg {
  pub name: SymbolId,
  pub value: ASTAttributeArg,
  pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub enum ASTAttributeArg {
  IntLiteral(i64, Span),
  StringLiteral(String, Span),
  Identifier(SymbolId, Span),
}

impl ASTAttributeArg {
  pub fn span(&self) -> &Span {
    match self {
      ASTAttributeArg::IntLiteral(_, span)
      | ASTAttributeArg::StringLiteral(_, span)
      | ASTAttributeArg::Identifier(_, span) => span,
    }
  }
}
