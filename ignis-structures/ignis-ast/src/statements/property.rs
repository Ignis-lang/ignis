use ignis_data_type::DataType;
use ignis_token::token::Token;
use serde::Serialize;

use crate::{expressions::ASTExpression, metadata::ASTMetadata};

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct ASTProperty {
  pub name: Box<Token>,
  pub initializer: Option<Box<ASTExpression>>,
  pub type_annotation: DataType,
  pub metadata: ASTMetadata,
}

impl ASTProperty {
  pub fn new(
    name: Box<Token>,
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
