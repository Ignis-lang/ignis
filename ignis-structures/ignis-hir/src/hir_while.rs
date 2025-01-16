use serde::Serialize;

use crate::HIRInstruction;

#[derive(Debug, Clone, Serialize)]
pub struct HIRWhile {
  pub condition: Box<HIRInstruction>,
  pub body: Box<HIRInstruction>,
}

impl HIRWhile {
  pub fn new(
    condition: Box<HIRInstruction>,
    body: Box<HIRInstruction>,
  ) -> Self {
    Self { condition, body }
  }
}
