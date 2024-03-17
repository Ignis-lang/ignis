use crate::{ir_interface::IRInterface, ir_method::IRMethod};

use super::{variable::IRVariable, IRInstructionTrait};

#[derive(Debug, Clone)]
pub struct IRClass {
  pub name: String,
  pub properties: Vec<IRVariable>,
  pub methods: Vec<IRMethod>,
  pub superclass: Option<Box<IRClass>>,
  pub is_exported: bool,
  pub is_imported: bool,
  pub interfaces: Vec<IRInterface>,
}

impl IRClass {
  pub fn new(
    name: String,
    methods: Vec<IRMethod>,
    properties: Vec<IRVariable>,
    is_exported: bool,
    is_imported: bool,
    interfaces: Vec<IRInterface>,
  ) -> Self {
    Self {
      name,
      methods,
      properties,
      superclass: None,
      is_exported,
      is_imported,
      interfaces,
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
      "is_exported": self.is_exported,
      "is_imported": self.is_imported,
      "interfaces": self.interfaces.iter().map(|i| i.to_json()).collect::<Vec<serde_json::Value>>(),
    })
  }
}
