use serde::Serialize;

use crate::HIRInstruction;

#[derive(Debug, Clone, Serialize)]
pub struct HIRBlock {
  pub instructions: Vec<HIRInstruction>,
  pub variables: Vec<HIRInstruction>,
}

impl HIRBlock {
  pub fn new(instructions: Vec<HIRInstruction>, variables: Vec<HIRInstruction>) -> Self {
    Self { instructions, variables }
  }
}
