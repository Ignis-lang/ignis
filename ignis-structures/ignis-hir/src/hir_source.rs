
use ignis_token::token::Token;
use serde::Serialize;

#[derive(Debug, Clone, Serialize)]
pub struct HIRSource {
  pub include: Token,
}

impl HIRSource {
  pub fn new(include: Token) -> Self {
    Self { include }
  }
}
