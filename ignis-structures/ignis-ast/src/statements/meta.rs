use ignis_token::token::Token;
use serde::Serialize;

use crate::metadata::ASTMetadata;

use super::{function::ASTGenericParameter, variable::ASTVariable};

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct ASTMetaStatement {
  pub name: Token,
  pub parameters: Vec<ASTVariable>,
  pub metadata: ASTMetadata,
  pub generic_parameters: Vec<ASTGenericParameter>,
}

impl ASTMetaStatement {
  pub fn new(
    name: Token,
    parameters: Vec<ASTVariable>,
    metadata: ASTMetadata,
    generic_parameters: Vec<ASTGenericParameter>,
  ) -> Self {
    Self {
      name,
      parameters,
      metadata,
      generic_parameters,
    }
  }
}
