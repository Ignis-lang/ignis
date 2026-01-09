use ignis_type::span::Span;

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct ASTContinue {
  pub span: Span,
}

impl ASTContinue {
  pub fn new(span: Span) -> Self {
    Self { span }
  }
}
