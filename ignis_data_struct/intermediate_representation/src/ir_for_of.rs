use token::token::Token;

use super::{variable::IRVariable, IRInstruction, IRInstructionTrait};

#[derive(Debug, Clone)]
pub struct IRForOf {
  pub variable: IRVariable,
  pub iterable: Box<IRInstruction>,
  pub body: Box<IRInstruction>,
  pub token: Token,
}

impl IRForOf {
  pub fn new(
    variable: IRVariable,
    iterable: Box<IRInstruction>,
    body: Box<IRInstruction>,
    token: Token,
  ) -> Self {
    Self {
      variable,
      iterable,
      body,
      token,
    }
  }
}

impl IRInstructionTrait for IRForOf {
  fn to_json(&self) -> serde_json::Value {
    serde_json::json!({
      "type": "IRForOf",
      "token": self.token.to_string(),
      "variable": self.variable.to_json(),
      "iterable": self.iterable.to_json(),
      "body": self.body.to_json(),
    })
  }
}