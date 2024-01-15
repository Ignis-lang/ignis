use super::IRInstruction;

#[repr(C)]
#[derive(Debug, Clone)]
pub struct IRAssign {
  pub name: String,
  pub value: Box<IRInstruction>,
}

impl IRAssign {
  pub fn new(name: String, value: Box<IRInstruction>) -> Self {
    Self { name, value }
  }

  pub fn to_json(&self) -> serde_json::Value {
    serde_json::json!({
      "type": "assign",
      "name": self.name,
      "value": self.value.to_json(),
    })
  }
}
