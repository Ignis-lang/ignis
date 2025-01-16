use serde::Serialize;

use crate::{HIRInstruction, HIRInstructionType};

#[derive(Debug, Clone, Serialize)]
pub struct HIRLogical {
  pub operator: HIRInstructionType,
  pub left: Box<HIRInstruction>,
  pub right: Box<HIRInstruction>,
}

impl HIRLogical {
  pub fn new(
    operator: HIRInstructionType,
    left: Box<HIRInstruction>,
    right: Box<HIRInstruction>,
  ) -> Self {
    Self { operator, left, right }
  }
}
