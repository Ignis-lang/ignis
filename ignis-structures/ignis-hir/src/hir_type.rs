use ignis_data_type::DataType;
use ignis_token::token::Token;
use serde::Serialize;

use crate::HIRMetadata;

#[derive(Debug, Clone, Serialize)]
pub struct HIRType {
  pub name: Token,
  pub value: Box<DataType>,
  pub metadata: HIRMetadata,
  pub generics: Vec<DataType>,
}

impl HIRType {
  pub fn new(
    name: Token,
    value: Box<DataType>,
    metadata: HIRMetadata,
    generics: Vec<DataType>,
  ) -> Self {
    Self {
      name,
      value,
      metadata,
      generics,
    }
  }
}
