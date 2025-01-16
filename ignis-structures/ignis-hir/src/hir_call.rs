use ignis_data_type::DataType;
use ignis_token::token::Token;
use serde::Serialize;

use crate::{HIRInstruction, HIRMetadata};

#[derive(Debug, Clone, Serialize)]
pub struct HIRCall {
  pub name: Token,
  pub arguments: Vec<HIRInstruction>,
  pub return_type: DataType,
  pub generic_parameters: Vec<DataType>,
  pub metadata: HIRMetadata,
}

impl HIRCall {
  pub fn new(
    name: Token,
    arguments: Vec<HIRInstruction>,
    return_type: DataType,
    generic_parameters: Vec<DataType>,
    metadata: HIRMetadata,
  ) -> Self {
    Self {
      name,
      arguments,
      return_type,
      generic_parameters,
      metadata,
    }
  }
}
