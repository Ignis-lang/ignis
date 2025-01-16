use ignis_token::token::Token;
use serde::Serialize;

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct ASTThis {
  pub token: Token,
}

impl ASTThis {
  pub fn new(token: Token) -> Self {
    Self { token }
  }
}
