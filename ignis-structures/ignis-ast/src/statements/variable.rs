use ignis_data_type::DataType;
use ignis_token::token::Token;

use crate::{expressions::ASTExpression, metadata::ASTMetadata};

#[derive(Debug, Clone, PartialEq)]
pub struct ASTVariable {
  pub name: Token,
  pub initializer: Option<Box<ASTExpression>>,
  pub type_annotation: DataType,
  pub metadata: ASTMetadata,
}

impl ASTVariable {
  pub fn new(
    name: Token,
    initializer: Option<Box<ASTExpression>>,
    type_annotation: DataType,
    metadata: ASTMetadata,
  ) -> Self {
    Self {
      name,
      initializer,
      type_annotation,
      metadata,
    }
  }
}
