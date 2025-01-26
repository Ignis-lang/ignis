use ignis_data_type::DataType;
use ignis_token::token::Token;
use serde::Serialize;

use crate::{HIRInstruction, HIRMetadata};

#[derive(Debug, Clone, Serialize)]
pub struct HIRRecord {
  pub name: Token,
  pub items: Vec<HIRInstruction>,
  pub generic_parameters: Vec<DataType>,
  pub metadata: HIRMetadata,
  pub data_type: DataType,
}

impl HIRRecord {
  pub fn new(
    name: Token,
    items: Vec<HIRInstruction>,
    generic_parameters: Vec<DataType>,
    metadata: HIRMetadata,
    data_type: DataType,
  ) -> Self {
    Self {
      name,
      items,
      generic_parameters,
      metadata,
      data_type,
    }
  }
}
