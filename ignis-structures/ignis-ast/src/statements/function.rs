use ignis_data_type::DataType;
use ignis_token::token::Token;

use crate::metadata::ASTMetadata;

use super::{variable::ASTVariable, ASTStatement};

#[derive(Debug, Clone, PartialEq)]
pub struct ASTFunction {
  pub name: Token,
  pub parameters: Vec<ASTVariable>,
  pub body: Vec<ASTStatement>,
  pub return_type: DataType,
  pub metadata: ASTMetadata,
}

impl ASTFunction {
  pub fn new(
    name: Token,
    parameters: Vec<ASTVariable>,
    body: Vec<ASTStatement>,
    return_type: DataType,
    metadata: ASTMetadata,
  ) -> Self {
    Self {
      name,
      parameters,
      body,
      return_type,
      metadata,
    }
  }
}
