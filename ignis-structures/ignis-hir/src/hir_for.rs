use serde::Serialize;

use crate::HIRInstruction;

#[derive(Clone, Debug, Serialize)]
pub struct HIRFor {
  pub initializer: Box<HIRInstruction>,
  pub condition: Box<HIRInstruction>,
  pub increment: Box<HIRInstruction>,
  pub body: Box<HIRInstruction>,
}

impl HIRFor {
  pub fn new(
    initializer: Box<HIRInstruction>,
    condition: Box<HIRInstruction>,
    increment: Box<HIRInstruction>,
    body: Box<HIRInstruction>,
  ) -> Self {
    Self {
      initializer,
      condition,
      increment,
      body,
    }
  }
}
