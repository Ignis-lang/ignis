use super::{class::IRClass, IRInstruction, IRInstructionTrait};

#[derive(Debug, Clone)]
pub struct IRClassInstance {
  pub class: Box<IRClass>,
  pub name: String,
  pub constructor_args: Vec<IRInstruction>,
}

impl IRClassInstance {
  pub fn new(class: Box<IRClass>, name: String, constructor_args: Vec<IRInstruction>) -> Self {
    Self { class, name, constructor_args }
  }
}

impl IRInstructionTrait for IRClassInstance {
  fn to_json(&self) -> serde_json::Value {
    serde_json::json!({
      "type": "IRClassInstance",
      "class": self.class.to_json(),
      "name": self.name,
      "constructor_args": self.constructor_args.iter().map(|i| i.to_json()).collect::<Vec<serde_json::Value>>(),
    })
  }
}
