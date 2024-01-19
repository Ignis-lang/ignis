use crate::{IRInstruction, IRInstructionTrait};

#[derive(Debug, Clone)]
pub struct IRFor {
  pub initializer: Box<IRInstruction>,
  pub condition: Box<IRInstruction>,
  pub increment: Box<IRInstruction>,
  pub body: Box<IRInstruction>,
}

impl IRFor {
  pub fn new(
    initializer: Box<IRInstruction>,
    condition: Box<IRInstruction>,
    increment: Box<IRInstruction>,
    body: Box<IRInstruction>,
  ) -> Self {
    Self {
      initializer,
      condition,
      increment,
      body,
    }
  }
}
impl IRInstructionTrait for IRFor {
  fn to_json(&self) -> serde_json::Value {
    serde_json::json!({
      "type": "IRFor",
      "initializer": self.initializer.to_json(),
      "condition": self.condition.to_json(),
      "increment": self.increment.to_json(),
      "body": self.body.to_json(),
    })
  }
}
