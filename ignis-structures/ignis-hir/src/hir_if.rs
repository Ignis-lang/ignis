use serde::Serialize;

use crate::HIRInstruction;

#[derive(Debug, Clone, Serialize)]
pub struct HIRIf {
  pub condition: Box<HIRInstruction>,
  pub then_branch: Box<HIRInstruction>,
  pub else_branch: Option<Box<HIRInstruction>>,
}

impl HIRIf {
  pub fn new(
    condition: Box<HIRInstruction>,
    then_branch: Box<HIRInstruction>,
    else_branch: Option<Box<HIRInstruction>>,
  ) -> Self {
    Self {
      condition,
      then_branch,
      else_branch,
    }
  }
}
