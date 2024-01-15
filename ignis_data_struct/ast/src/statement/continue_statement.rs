use token::token::Token;

#[derive(Debug, PartialEq, Clone)]
pub struct Continue {
  pub token: Token,
}

impl Continue {
  pub fn new(token: Token) -> Self {
    Self { token }
  }
}
