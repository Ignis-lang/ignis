use ignis_type::{span::Span, symbol::SymbolId, value::IgnisLiteralValue};

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub enum ASTPattern {
  Wildcard {
    span: Span,
  },
  Literal {
    value: IgnisLiteralValue,
    span: Span,
  },
  Path {
    segments: Vec<(SymbolId, Span)>,
    args: Option<Vec<ASTPattern>>,
    span: Span,
  },
  Tuple {
    elements: Vec<ASTPattern>,
    span: Span,
  },
  Or {
    patterns: Vec<ASTPattern>,
    span: Span,
  },
}

impl ASTPattern {
  pub fn span(&self) -> &Span {
    match self {
      ASTPattern::Wildcard { span } => span,
      ASTPattern::Literal { span, .. } => span,
      ASTPattern::Path { span, .. } => span,
      ASTPattern::Tuple { span, .. } => span,
      ASTPattern::Or { span, .. } => span,
    }
  }
}
