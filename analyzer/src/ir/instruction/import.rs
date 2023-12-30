use lexer::token::Token;

use super::IRInstructionTrait;

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

impl IRInstructionTrait for IRImport {
  fn to_json(&self) -> serde_json::Value {
    serde_json::json!({
      "type": "IRImport",
      "name": self.name.iter().map(|(x, y)| {
        match y {
          Some(y) => format!("{} as {}", x, y),
          None => x.to_string(),
        }
      }).collect::<Vec<String>>(),
      "path": self.path,
    })
  }
}
