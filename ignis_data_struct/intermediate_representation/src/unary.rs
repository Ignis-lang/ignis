use enums::data_type::DataType;

use super::{instruction_type::IRInstructionType, IRInstruction};

#[derive(Debug, Clone)]
pub struct IRUnary {
  pub instruction_type: IRInstructionType,
  pub right: Box<IRInstruction>,
  pub data_type: DataType,
}

impl IRUnary {
  pub fn new(
    instruction_type: IRInstructionType,
    right: Box<IRInstruction>,
    data_type: DataType,
  ) -> Self {
    Self {
      instruction_type,
      right,
      data_type,
    }
  }

  pub fn to_json(&self) -> serde_json::Value {
    serde_json::json!({
      "type": "unary",
      "instruction_type": self.instruction_type.to_string(),
      "right": self.right.to_json(),
      "data_type": self.data_type.to_string(),
    })
  }
}
