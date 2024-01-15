use token::token::Token;

#[derive(Debug, Clone)]
pub struct IRBreak {
  token: Token,
}

impl IRBreak {
  pub fn new(token: Token) -> Self {
    Self { token }
  }

  pub fn to_json(&self) -> serde_json::Value {
    serde_json::json!({
      "type": "break",
    })
  }
}
