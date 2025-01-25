use serde::Serialize;

use crate::HIRInstruction;

#[derive(Debug, Clone, Serialize)]
pub struct HIRGrouping {
  pub expression: Box<HIRInstruction>,
}

impl HIRGrouping {
  pub fn new(expression: Box<HIRInstruction>) -> Self {
    Self { expression }
  }
}
