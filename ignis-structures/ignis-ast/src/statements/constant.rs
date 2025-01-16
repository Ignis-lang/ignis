use ignis_data_type::DataType;
use ignis_token::token::Token;
use serde::Serialize;

use crate::{expressions::ASTExpression, metadata::ASTMetadata};

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct ASTConstant {
  pub name: Token,
  pub value: Box<ASTExpression>,
  pub type_annotation: DataType,
  pub metadata: ASTMetadata,
}

impl ASTConstant {
  pub fn new(
    name: Token,
    value: Box<ASTExpression>,
    type_annotation: DataType,
    metadata: ASTMetadata,
  ) -> Self {
    Self {
      name,
      value,
      type_annotation,
      metadata,
    }
  }
}
