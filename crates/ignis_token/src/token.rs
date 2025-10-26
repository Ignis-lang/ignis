use crate::token_types::TokenType;

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
  pub type_: TokenType,
  pub lexeme: String,
  pub line: usize,
  pub start: usize,
  pub column: usize,
  pub file_name: String,
}

impl Default for Token {
  fn default() -> Self {
    Self {
      type_: TokenType::Eof,
      lexeme: String::new(),
      line: 0,
      column: 0,
      start: 0,
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
    column: usize,
    file_name: String,
  ) -> Self {
    Self {
      type_,
      lexeme,
      line,
      start,
      column,
      file_name,
    }
  }

  pub fn vec_to_lisp(tokens: &Vec<Token>) -> Vec<String> {
    let mut result: Vec<String> = vec![];

    tokens.into_iter().for_each(|token: &Token| {
      result.push(format!(
        "(token type: {} lexeme: {} file: {} line: {} start: {} column: {})",
        token.type_, token.lexeme, token.file_name, token.line, token.start, token.start,
      ))
    });

    result
  }
}
