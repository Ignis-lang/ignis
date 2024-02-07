use token::token::Token;
use serde_json::json;

use super::IRInstructionTrait;

#[derive(Debug, Clone)]
pub struct IRBreak {
  pub token: Token,
}

impl IRBreak {
  pub fn new(token: Token) -> Self {
    Self { token }
  }
}

impl IRInstructionTrait for IRBreak {
  fn to_json(&self) -> serde_json::Value {
    json!({
      "type": "IRBreak",
    })
  }
}
