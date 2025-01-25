use ignis_data_type::DataType;
use ignis_token::token::Token;
use serde::Serialize;

use crate::{HIRInstruction, HIRMetadata};

#[derive(Debug, Clone, Serialize)]
pub struct HIRMeta {
  pub name: Token,
  pub parameters: Vec<HIRInstruction>,
  pub metadata: HIRMetadata,
  pub generic_parameters: Vec<DataType>,
}

impl HIRMeta {
  pub fn new(
    name: Token,
    parameters: Vec<HIRInstruction>,
    metadata: HIRMetadata,
    generic_parameters: Vec<DataType>,
  ) -> Self {
    Self {
      name,
      parameters,
      metadata,
      generic_parameters,
    }
  }
}
