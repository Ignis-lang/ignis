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

  pub fn to_json(&self) -> serde_json::Value {
    serde_json::json!({
      "type": "import",
      "name": self.name.iter().map(|(name, alias)| {
        serde_json::json!({
          "name": name.span.literal.clone(),
          "alias": alias.as_ref().map(|alias| alias.span.literal.clone())
        })
      }).collect::<Vec<serde_json::Value>>(),
      "path": self.path,
    })
  }
}
