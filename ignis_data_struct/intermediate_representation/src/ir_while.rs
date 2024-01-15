use super::IRInstruction;

#[derive(Debug, Clone)]
pub struct IRWhile {
  pub condition: Box<IRInstruction>,
  pub body: Box<IRInstruction>,
}

impl IRWhile {
  pub fn new(condition: Box<IRInstruction>, body: Box<IRInstruction>) -> Self {
    Self { condition, body }
  }

  pub fn to_json(&self) -> serde_json::Value {
    serde_json::json!({
      "type": "while",
      "condition": self.condition.to_json(),
      "body": self.body.to_json(),
    })
  }
}
