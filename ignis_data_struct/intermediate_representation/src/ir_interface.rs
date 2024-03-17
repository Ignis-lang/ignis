use serde_json::{Map, Value};
use token::token::Token;

use crate::{IRInstruction, IRInstructionTrait};

#[derive(Debug, Clone)]
pub struct IRInterface {
  pub name: Token,
  pub methods: Vec<IRInstruction>,
}

impl IRInterface {
  pub fn new(
    name: Token,
    methods: Vec<IRInstruction>,
  ) -> Self {
    Self { name, methods }
  }
}

impl IRInstructionTrait for IRInterface {
  fn to_json(&self) -> serde_json::Value {
    let mut json = Map::new();
    json.insert("name".to_string(), self.name.to_json());

    let methods = self
      .methods
      .clone()
      .into_iter()
      .map(|method| method.to_json())
      .collect::<Value>();
    json.insert("methods".to_string(), methods);

    Value::Object(json)
  }
}
