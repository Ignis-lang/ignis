use enums::data_type::DataType;
use serde_json::{json, Value};

use crate::instruction_type::IRInstructionType;

use super::{IRInstruction, IRInstructionTrait};

#[derive(Debug, Clone)]
pub struct IRBinary {
  pub instruction_type: IRInstructionType,
  pub left: Box<IRInstruction>,
  pub right: Box<IRInstruction>,
  pub data_type: DataType,
}

impl IRBinary {
  pub fn new(
    instruction_type: IRInstructionType,
    left: Box<IRInstruction>,
    right: Box<IRInstruction>,
    data_type: DataType,
  ) -> Self {
    Self {
      instruction_type,
      left,
      right,
      data_type,
    }
  }
}

impl IRInstructionTrait for IRBinary {
  fn to_json(&self) -> Value {
    json!({
      "type": "IRBinary",
      "instruction_type": self.instruction_type.to_string(),
      "left": self.left.to_json(),
      "right": self.right.to_json(),
      "data_type": self.data_type.to_string(),
    })
  }
}
