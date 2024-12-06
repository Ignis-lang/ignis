use ignis_token::token::Token;

use crate::metadata::ASTMetadata;

use super::{function::ASTGenericParameter, ASTStatement};

#[derive(Debug, Clone, PartialEq)]
pub struct ASTRecord {
  pub name: Token,
  pub items: Vec<ASTStatement>,
  pub metadata: ASTMetadata,
  pub generic_parameters: Vec<ASTGenericParameter>,
}

impl ASTRecord {
  pub fn new(
    name: Token,
    items: Vec<ASTStatement>,
    metadata: ASTMetadata,
    generic_parameters: Vec<ASTGenericParameter>,
  ) -> Self {
    Self {
      name,
      items,
      metadata,
      generic_parameters,
    }
  }
}
