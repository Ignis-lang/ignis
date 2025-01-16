use ignis_data_type::DataType;
use serde::Serialize;

use crate::{HIRInstruction, HIRInstructionType};

#[derive(Debug, Clone, Serialize)]
pub struct HIRBinary {
  pub left: Box<HIRInstruction>,
  pub operator: HIRInstructionType,
  pub right: Box<HIRInstruction>,
  pub data_type: DataType,
}

impl HIRBinary {
  pub fn new(
    left: Box<HIRInstruction>,
    operator: HIRInstructionType,
    right: Box<HIRInstruction>,
    data_type: DataType,
  ) -> Self {
    Self {
      left,
      operator,
      right,
      data_type,
    }
  }
}
