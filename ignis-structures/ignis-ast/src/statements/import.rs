use ignis_token::token::Token;
use serde::{Deserialize, Serialize};
use serde_json::json;

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub enum ASTImportSource {
  StandardLibrary,
  FileSystem,
  Package,
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct ASTImportSymbol {
  pub name: Token,
  pub alias: Option<Token>,
}

impl ASTImportSymbol {
  pub fn new(
    name: Token,
    alias: Option<Token>,
  ) -> Self {
    Self { name, alias }
  }

  pub fn to_json(&self) -> serde_json::Value {
    match &self.alias {
      Some(alias) => json!({
        "name": self.name.lexeme,
        "alias": alias.lexeme,
      }),
      None => json!({
        "name": self.name.lexeme,
      }),
    }
  }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ASTImport {
  pub module_path: Token,
  pub symbols: Vec<ASTImportSymbol>,
  pub is_std: bool,
  pub source: ASTImportSource,
}

impl ASTImport {
  pub fn new(
    module_path: Token,
    symbols: Vec<ASTImportSymbol>,
    is_std: bool,
    source: ASTImportSource,
  ) -> Self {
    Self {
      module_path,
      symbols,
      is_std,
      source,
    }
  }
}
