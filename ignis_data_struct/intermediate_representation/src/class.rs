use super::{function::IRFunction, variable::IRVariable};

#[derive(Debug, Clone)]
pub struct IRClass {
  pub name: String,
  pub properties: Vec<IRVariable>,
  pub methods: Vec<IRFunction>,
  pub superclass: Option<Box<IRClass>>,
  // pub interfaces: Vec<IRClass>,
}

impl IRClass {
  pub fn new(name: String, methods: Vec<IRFunction>, properties: Vec<IRVariable>) -> Self {
    Self {
      name,
      methods,
      properties,
      superclass: None,
    }
  }

  pub fn to_json(&self) -> serde_json::Value {
    serde_json::json!({
      "type": "class",
      "name": self.name,
      "methods": self.methods.iter().map(|x| x.to_json()).collect::<Vec<_>>(),
    })
  }
}
