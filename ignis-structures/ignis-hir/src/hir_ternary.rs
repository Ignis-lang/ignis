
use ignis_data_type::DataType;
use serde::Serialize;

use super::HIRInstruction;

#[derive(Debug, Clone, Serialize)]
pub struct HIRTernary {
  pub condition: Box<HIRInstruction>,
  pub then_branch: Box<HIRInstruction>,
  pub else_branch: Box<HIRInstruction>,
  pub data_type: DataType,
}

impl HIRTernary {
  pub fn new(
    condition: Box<HIRInstruction>,
    then_branch: Box<HIRInstruction>,
    else_branch: Box<HIRInstruction>,
    data_type: DataType,
  ) -> Self {
    Self {
      condition,
      then_branch,
      else_branch,
      data_type,
    }
  }
}
