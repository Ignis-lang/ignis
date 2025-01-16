use ignis_token::token::Token;
use serde::Serialize;

#[derive(Debug, PartialEq, Clone, Serialize)]
pub enum ASTImportSource {
  StandardLibrary,
  FileSystem,
  Package,
}

#[derive(Debug, PartialEq, Clone, Serialize)]
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
}

#[derive(Debug, PartialEq, Clone, Serialize)]
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
