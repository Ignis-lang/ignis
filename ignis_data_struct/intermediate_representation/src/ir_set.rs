use serde_json::{json, Value};

use super::{IRInstructionTrait, IRInstruction, class_instance::IRClassInstance};

#[derive(Debug, Clone)]
pub struct IRSet {
  pub name: String,
  pub value: Box<IRInstruction>,
  pub object: Box<IRClassInstance>,
}

impl IRInstructionTrait for IRSet {
  fn to_json(&self) -> Value {
    json!({
      "type": "IRSet",
      "name": self.name,
      "value": self.value.to_json(),
      "object": self.object.to_json(),
    })
  }
}

impl IRSet {
  pub fn new(name: String, value: Box<IRInstruction>, object: Box<IRClassInstance>) -> Self {
    Self {
      name,
      value,
      object,
    }
  }
}
