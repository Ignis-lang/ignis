use ignis_data_type::DataType;
use ignis_token::token::Token;

use crate::metadata::ASTMetadata;

use super::{block::ASTBlock, variable::ASTVariable};

#[derive(Debug, Clone, PartialEq)]
pub struct ASTMethod {
  pub name: Token,
  pub parameters: Vec<ASTVariable>,
  pub body: ASTBlock,
  pub return_type: DataType,
  pub metadata: ASTMetadata,
  pub class_name: Token,
}

impl ASTMethod {
  pub fn new(
    name: Token,
    parameters: Vec<ASTVariable>,
    body: ASTBlock,
    return_type: DataType,
    metadata: ASTMetadata,
    class_name: Token,
  ) -> Self {
    Self {
      name,
      parameters,
      body,
      return_type,
      metadata,
      class_name,
    }
  }
}
