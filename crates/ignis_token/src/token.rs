use crate::token_types::TokenType;
use ignis_type::Span;

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
  pub type_: TokenType,
  pub lexeme: String,
  pub line: usize,
  pub column: usize,
  pub span: Span,
  pub file_name: String,
}

impl Default for Token {
  fn default() -> Self {
    Self {
      type_: TokenType::Eof,
      lexeme: String::new(),
      line: 0,
      column: 0,
      span: Span::default(),
      file_name: String::new(),
    }
  }
}

impl Token {
  pub fn new(
    type_: TokenType,
    lexeme: String,
    line: usize,
    start: usize,
    end: usize,
    column: usize,
    file_name: String,
  ) -> Self {
    Self {
      type_,
      lexeme,
      line,
      column,
      span: Span { start, end },
      file_name,
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
      "(token type: {} lexeme: {} file: {} line: {} column: {} span: {})",
      self.type_, self.lexeme, self.file_name, self.line, self.column, self.span,
    )
  }
}
