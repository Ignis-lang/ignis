use ignis_token::token::Token;
use serde::Serialize;

use crate::metadata::ASTMetadata;

use super::ASTStatement;

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct ASTExtern {
  pub name: Token,
  pub body: Vec<ASTStatement>,
  pub metadata: ASTMetadata,
}

impl ASTExtern {
  pub fn new(
    name: Token,
    body: Vec<ASTStatement>,
    metadata: ASTMetadata,
  ) -> Self {
    Self { name, body, metadata }
  }
}
