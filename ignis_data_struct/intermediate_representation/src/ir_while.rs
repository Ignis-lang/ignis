use serde_json::json;

use super::{IRInstruction, IRInstructionTrait};

#[derive(Debug, Clone)]
pub struct IRWhile {
  pub condition: Box<IRInstruction>,
  pub body: Box<IRInstruction>,
}

impl IRWhile {
  pub fn new(condition: Box<IRInstruction>, body: Box<IRInstruction>) -> Self {
    Self { condition, body }
  }
}

impl IRInstructionTrait for IRWhile {
  fn to_json(&self) -> serde_json::Value {
    json!({
      "type": "IRWhile",
      "condition": self.condition.to_json(),
      "body": self.body.to_json(),
    })
  }
}
