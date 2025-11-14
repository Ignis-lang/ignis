use ignis_type::{span::Span, value::IgnisLiteralValue};

#[derive(Debug, PartialEq, Clone)]
pub struct ASTLiteral {
  pub value: IgnisLiteralValue,
  pub span: Span,
}

impl ASTLiteral {
  pub fn new(
    value: IgnisLiteralValue,
    span: Span,
  ) -> Self {
    Self { value, span }
  }
}
