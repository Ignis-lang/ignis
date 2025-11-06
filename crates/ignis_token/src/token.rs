use crate::token_types::TokenType;
use ignis_type::span::Span;

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
    Self {
      type_,
      lexeme,
      span,
    }
  }
}

impl std::fmt::Display for Token {
  fn fmt(
    &self,
    f: &mut std::fmt::Formatter<'_>,
  ) -> std::fmt::Result {
    write!(
      f,
      "(token type: {} lexeme: {} span: {})",
      self.type_, self.lexeme, self.span,
    )
  }
}
