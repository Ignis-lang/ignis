use ignis_token::token::Token;
use serde::Serialize;

#[derive(Debug, Clone, Serialize)]
pub struct HIRInclude {
  pub include: Token,
}

impl HIRInclude {
  pub fn new(include: Token) -> Self {
    Self { include }
  }
}
