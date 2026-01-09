use ignis_type::span::Span;

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct ASTBreak {
  pub span: Span,
}

impl ASTBreak {
  pub fn new(span: Span) -> Self {
    Self { span }
  }
}
