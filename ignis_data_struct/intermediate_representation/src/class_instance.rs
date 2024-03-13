use token::token::Token;

use super::{class::IRClass, IRInstruction, IRInstructionTrait};

#[derive(Debug, Clone)]
pub struct IRClassInstance {
  pub class: Box<IRClass>,
  pub var_name: Token,
  pub constructor_args: Vec<IRInstruction>,
}

impl IRClassInstance {
  pub fn new(
    class: Box<IRClass>,
    var_name: Token,
    constructor_args: Vec<IRInstruction>,
  ) -> Self {
    Self {
      class,
      var_name,
      constructor_args,
    }
  }
}

impl IRInstructionTrait for IRClassInstance {
  fn to_json(&self) -> serde_json::Value {
    serde_json::json!({
      "type": "IRClassInstance",
      "class": self.class.to_json(),
      "name": self.var_name.to_json(),
      "constructor_args": self.constructor_args.iter().map(|i| i.to_json()).collect::<Vec<serde_json::Value>>(),
    })
  }
}
