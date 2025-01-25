use ignis_token::token::Token;
use serde::Serialize;

pub type ImportName = Vec<(Token, Option<Token>)>;

#[derive(Debug, Clone, Serialize)]
pub struct HIRImport {
  pub name: ImportName,
  pub path: String,
}

impl HIRImport {
  pub fn new(
    name: ImportName,
    path: String,
  ) -> Self {
    Self { name, path }
  }
}
