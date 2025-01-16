use ignis_data_type::DataType;
use ignis_token::token::Token;
use serde::Serialize;

use crate::{HIRInstruction, HIRMetadata, };

#[derive(Debug, Clone, Serialize)]
pub struct HIRVariable {
  pub name: Token,
  pub data_type: DataType,
  pub value: Option<Box<HIRInstruction>>,
  pub metadata: HIRMetadata,
}

impl HIRVariable {
  pub fn new(
    name: Token,
    data_type: DataType,
    value: Option<Box<HIRInstruction>>,
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
