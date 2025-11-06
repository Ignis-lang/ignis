use ignis_type::span::Span;

#[derive(Debug, Clone, PartialEq)]
pub struct ASTComment {
  pub span: Span,
  pub content: String,
}

impl ASTComment {
  pub fn new(
    span: Span,
    content: String,
  ) -> Self {
    Self { span, content }
  }
}
