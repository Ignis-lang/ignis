use token::token::Token;

pub type ImportName = Vec<(Token, Option<Token>)>;

#[derive(Debug, Clone)]
pub struct IRImport {
  pub name: ImportName,
  pub path: String,
}

impl IRImport {
  pub fn new(name: ImportName, path: String) -> Self {
    Self { name, path }
  }
}
