use token::token::Token;

#[derive(Debug, Clone)]
pub struct IRContinue {
  pub token: Token,
}

impl IRContinue {
  pub fn new(token: Token) -> Self {
    Self { token }
  }

  pub fn to_json(&self) -> serde_json::Value {
    serde_json::json!({
      "type": "continue",
      "token": self.token.to_json(),
    })
  }
}
