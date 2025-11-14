use crate::token_types::TokenType;
use ignis_type::{span::Span, value::IgnisLiteralValue};

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
  pub type_: TokenType,
  pub lexeme: String,
  pub span: Span,
}

impl Default for Token {
  fn default() -> Self {
    Self {
      type_: TokenType::Eof,
      lexeme: String::new(),
      span: Span::default(),
    }
  }
}

impl Token {
  pub fn new(
    type_: TokenType,
    lexeme: String,
    span: Span,
  ) -> Self {
    Self { type_, lexeme, span }
  }
}

impl std::fmt::Display for Token {
  fn fmt(
    &self,
    f: &mut std::fmt::Formatter<'_>,
  ) -> std::fmt::Result {
    write!(f, "(token type: {} lexeme: {} span: {})", self.type_, self.lexeme, self.span,)
  }
}

impl Into<IgnisLiteralValue> for &Token {
  fn into(self) -> IgnisLiteralValue {
    match self.type_ {
      TokenType::Int => IgnisLiteralValue::Int64(self.lexeme.parse().unwrap_or(0)),
      TokenType::Float => IgnisLiteralValue::Float64(self.lexeme.parse().unwrap_or(0.0)),
      TokenType::Char => IgnisLiteralValue::Char(self.lexeme.parse().unwrap_or('\0')),
      TokenType::String => IgnisLiteralValue::String(self.lexeme.clone()),
      TokenType::False | TokenType::True => IgnisLiteralValue::Boolean(self.lexeme.parse().unwrap_or(false)),
      TokenType::Hex => IgnisLiteralValue::Hex(self.lexeme.clone()),
      TokenType::Binary => IgnisLiteralValue::Binary(self.lexeme.clone()),
      _ => IgnisLiteralValue::Null,
    }
  }
}
