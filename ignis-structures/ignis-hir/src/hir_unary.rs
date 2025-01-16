use ignis_data_type::DataType;
use serde::Serialize;

use crate::{HIRInstruction, HIRInstructionType};

#[derive(Debug, Clone, Serialize)]
pub struct HIRUnary {
  pub operator: HIRInstructionType,
  pub value: Box<HIRInstruction>,
  pub data_type: DataType,
}

impl HIRUnary {
  pub fn new(
    operator: HIRInstructionType,
    value: Box<HIRInstruction>,
    data_type: DataType,
  ) -> Self {
    Self { operator, value, data_type }
  }
}
