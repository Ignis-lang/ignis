use ignis_data_type::DataType;
use ignis_token::token::Token;
use serde::Serialize;

use crate::metadata::ASTMetadata;

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct ASTVariableExpression {
  pub name: Token,
  pub data_type: DataType,
  pub metadata: ASTMetadata,
}

impl ASTVariableExpression {
  pub fn new(
    name: Token,
    data_type: DataType,
    metadata: ASTMetadata,
  ) -> Self {
    Self {
      name,
      data_type,
      metadata,
    }
  }
}
