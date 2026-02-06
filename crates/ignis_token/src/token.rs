use crate::token_types::TokenType;
use ignis_type::{span::Span, value::IgnisLiteralValue};
use ordered_float::OrderedFloat;

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

impl From<&Token> for IgnisLiteralValue {
  fn from(val: &Token) -> Self {
    match val.type_ {
      TokenType::Int => IgnisLiteralValue::Int32(val.lexeme.parse().unwrap_or(0)),
      TokenType::Float => IgnisLiteralValue::Float64(val.lexeme.parse().unwrap_or(OrderedFloat::default())),
      TokenType::Char => IgnisLiteralValue::Char(val.lexeme.chars().next().unwrap_or('\0')),
      TokenType::String => IgnisLiteralValue::String(val.lexeme.clone()),
      TokenType::False | TokenType::True => IgnisLiteralValue::Boolean(val.lexeme.parse().unwrap_or(false)),
      TokenType::Hex => IgnisLiteralValue::Hex(val.lexeme.clone()),
      TokenType::Binary => IgnisLiteralValue::Binary(val.lexeme.clone()),
      _ => IgnisLiteralValue::Null,
    }
  }
}
