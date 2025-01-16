use ignis_data_type::DataType;
use ignis_token::token::Token;
use serde::Serialize;

use crate::{HIRInstruction, HIRMetadata};

#[derive(Debug, Clone, Serialize)]
pub struct HIRConstant {
  pub name: Token,
  pub data_type: DataType,
  pub value: Box<HIRInstruction>,
  pub metadata: HIRMetadata,
}

impl HIRConstant {
  pub fn new(
    name: Token,
    data_type: DataType,
    value: Box<HIRInstruction>,
    metadata: HIRMetadata,
  ) -> Self {
    Self {
      name,
      data_type,
      value,
      metadata,
    }
  }
}
