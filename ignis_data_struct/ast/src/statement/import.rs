use token::token::Token;
use serde_json::json;

#[derive(Debug, PartialEq, Clone)]
pub enum ImportSource {
  StandardLibrary,
  FileSystem,
  Package,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ImportSymbol {
  pub name: Token,
  pub alias: Option<Token>,
}

impl ImportSymbol {
  pub fn new(name: Token, alias: Option<Token>) -> Self {
    Self { name, alias }
  }

  pub fn to_json(&self) -> serde_json::Value {
    match &self.alias {
      Some(alias) => json!({
        "name": self.name.span.literal,
        "alias": alias.span.literal,
      }),
      None => json!({
        "name": self.name.span.literal,
      }),
    }
  }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Import {
  pub module_path: Token,
  pub symbols: Vec<ImportSymbol>,
  pub is_std: bool,
  pub source: ImportSource,
}

impl Import {
  pub fn new(
    module_path: Token,
    symbols: Vec<ImportSymbol>,
    is_std: bool,
    source: ImportSource,
  ) -> Self {
    Self {
      module_path,
      symbols,
      is_std,
      source,
    }
  }
}
