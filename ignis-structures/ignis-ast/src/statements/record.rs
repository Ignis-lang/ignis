use ignis_token::token::Token;

use crate::metadata::ASTMetadata;

use super::ASTStatement;

#[derive(Debug, Clone, PartialEq)]
pub struct ASTRecord {
  pub name: Token,
  pub items: Vec<ASTStatement>,
  pub metadata: ASTMetadata,
}

impl ASTRecord {
  pub fn new(
    name: Token,
    items: Vec<ASTStatement>,
    metadata: ASTMetadata,
  ) -> Self {
    Self { name, items, metadata }
  }
}
