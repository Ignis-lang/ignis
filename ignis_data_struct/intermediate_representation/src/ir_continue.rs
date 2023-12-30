use token::token::Token;

use crate::IRInstructionTrait;

#[derive(Debug, Clone)]
pub struct IRContinue {
  pub token: Token,
}

impl IRContinue {
  pub fn new(token: Token) -> Self {
    Self { token }
  }
}

impl IRInstructionTrait for IRContinue {
  fn to_json(&self) -> serde_json::Value {
    serde_json::json!({
      "type": "continue",
      "token": self.token.to_json(),
    })
  }
}
