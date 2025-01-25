use ignis_token::token::Token;
use serde::Serialize;

use crate::metadata::ASTMetadata;

use super::ASTStatement;

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct ASTNamespace {
  pub name: Token,
  pub members: Vec<ASTStatement>,
  pub metadata: ASTMetadata,
}

impl ASTNamespace {
  pub fn new(
    name: Token,
    members: Vec<ASTStatement>,
    metadata: ASTMetadata,
  ) -> Self {
    Self {
      name,
      members,
      metadata,
    }
  }
}
