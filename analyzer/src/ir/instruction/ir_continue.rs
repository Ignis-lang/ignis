use lexer::token::Token;
use serde_json::json;

use super::IRInstructionTrait;

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
    json!({
      "type": "continue",
      "token": self.token.to_string(),
    })
  }
}
