use ignis_data_type::DataType;
use ignis_token::token::Token;
use serde::Serialize;

use crate::{expressions::ASTExpression, metadata::ASTMetadata};

use super::function::ASTGenericParameter;

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct ASTEnumItem {
  pub name: Token,
  pub value: Option<ASTExpression>,
  pub data_type: DataType,
  pub metadata: ASTMetadata,
}

impl ASTEnumItem {
  pub fn new(
    name: Token,
    value: Option<ASTExpression>,
    data_type: DataType,
    metadata: ASTMetadata,
  ) -> Self {
    Self {
      name,
      value,
      data_type,
      metadata,
    }
  }
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct ASTEnum {
  pub name: Token,
  pub members: Vec<ASTEnumItem>,
  pub metadata: ASTMetadata,
  pub generics: Vec<ASTGenericParameter>,
}

impl ASTEnum {
  pub fn new(
    name: Token,
    members: Vec<ASTEnumItem>,
    metadata: ASTMetadata,
    generics: Vec<ASTGenericParameter>,
  ) -> Self {
    Self {
      name,
      members,
      metadata,
      generics,
    }
  }
}
