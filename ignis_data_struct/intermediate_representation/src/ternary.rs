use enums::data_type::DataType;

use super::{IRInstruction, IRInstructionTrait};

#[derive(Debug, Clone)]
pub struct IRTernary {
  pub condition: Box<IRInstruction>,
  pub then_branch: Box<IRInstruction>,
  pub else_branch: Box<IRInstruction>,
  pub data_type: DataType,
}

impl IRTernary {
  pub fn new(
    condition: Box<IRInstruction>,
    then_branch: Box<IRInstruction>,
    else_branch: Box<IRInstruction>,
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

impl IRInstructionTrait for IRTernary {
  fn to_json(&self) -> serde_json::Value {
    serde_json::json!({
      "type": "IRTernary",
      "condition": self.condition.to_json(),
      "then_branch": self.then_branch.to_json(),
      "else_branch": self.else_branch.to_json(),
    })
  }
}
