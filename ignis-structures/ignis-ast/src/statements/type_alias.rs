use ignis_data_type::DataType;
use ignis_token::token::Token;
use serde::Serialize;

use crate::metadata::ASTMetadata;

use super::function::ASTGenericParameter;

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct ASTTypeAlias {
  pub name: Token,
  pub value: Box<DataType>,
  pub metadata: ASTMetadata,
  pub generics: Vec<ASTGenericParameter>,
}

impl ASTTypeAlias {
  pub fn new(
    name: Token,
    value: Box<DataType>,
    metadata: ASTMetadata,
    generics: Vec<ASTGenericParameter>,
  ) -> Self {
    Self {
      name,
      value,
      metadata,
      generics,
    }
  }
}
