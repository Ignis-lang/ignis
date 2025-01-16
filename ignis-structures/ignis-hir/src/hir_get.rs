use ignis_data_type::DataType;
use ignis_token::token::Token;
use serde::Serialize;

use crate::{HIRInstruction, HIRMetadata};

#[derive(Debug, Clone, Serialize)]
pub struct HIRGet {
  pub name: Token,
  pub object: Box<HIRInstruction>,
  pub data_type: DataType,
  pub metadata: HIRMetadata,
}

impl HIRGet {
  pub fn new(
    name: Token,
    object: Box<HIRInstruction>,
    data_type: DataType,
    metadata: HIRMetadata,
  ) -> Self {
    Self {
      name,
      object,
      data_type,
      metadata,
    }
  }
}
