use crate::token_types::TokenType;
use ascii_table::AsciiTable;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Token {
  pub type_: TokenType,
  pub lexeme: String,
  pub line: usize,
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
      file_name: String::new(),
    }
  }
}

impl Token {
  pub fn new(
    type_: TokenType,
    lexeme: String,
    line: usize,
    column: usize,
    file_name: String,
  ) -> Self {
    Self {
      type_,
      lexeme,
      line,
      column,
      file_name,
    }
  }

  pub fn to_ascii_table_row(&self) -> Vec<String> {
    let mut lexeme = self.lexeme.clone();

    if lexeme.contains('\n') {
      lexeme = lexeme.replace('\n', "\\n");
    }

    if lexeme.contains('\r') {
      lexeme = lexeme.replace('\r', "\\r");
    }

    if lexeme.contains('\t') {
      lexeme = lexeme.replace('\t', "\\t");
    }

    vec![
      self.type_.to_string(),
      lexeme,
      self.line.to_string(),
      self.column.to_string(),
      self.file_name.clone(),
    ]
  }

  pub fn vec_to_ascii_table(tokens: &Vec<Token>) -> Vec<Vec<String>> {
    let mut result: Vec<Vec<String>> = vec![];

    for token in tokens {
      result.push(token.to_ascii_table_row());
    }

    result
  }

  pub fn print_ascii_table(tokens: &Vec<Token>) {
    let mut ascii_table = AsciiTable::default();
    ascii_table.column(0).set_header("Type");
    ascii_table.column(1).set_header("Lexeme");
    ascii_table.column(2).set_header("Line");
    ascii_table.column(3).set_header("Column");
    ascii_table.column(4).set_header("File Name");

    ascii_table.print(Self::vec_to_ascii_table(&tokens));
  }
}
