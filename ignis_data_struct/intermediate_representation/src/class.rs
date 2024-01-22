use crate::ir_method::IRMethod;

use super::{variable::IRVariable, IRInstructionTrait};

#[derive(Debug, Clone)]
pub struct IRClass {
  pub name: String,
  pub properties: Vec<IRVariable>,
  pub methods: Vec<IRMethod>,
  pub superclass: Option<Box<IRClass>>,
  // pub interfaces: Vec<IRClass>,
}

impl IRClass {
  pub fn new(name: String, methods: Vec<IRMethod>, properties: Vec<IRVariable>) -> Self {
    Self {
      name,
      methods,
      properties,
      superclass: None,
    }
  }
}

impl IRInstructionTrait for IRClass {
  fn to_json(&self) -> serde_json::Value {
    serde_json::json!({
      "type": "IRClass",
      "name": self.name,
      "methods": self.methods.iter().map(|m| m.to_json()).collect::<Vec<serde_json::Value>>(),
      "properties": self.properties.iter().map(|p| p.to_json()).collect::<Vec<serde_json::Value>>(),
    })
  }
}
