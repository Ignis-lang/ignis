use token::Token;

#[derive(Debug, PartialEq, Clone)]
pub struct BreakStatement {
  pub token: Token,
}

impl BreakStatement {
  pub fn new(token: Token) -> Self {
    Self { token }
  }
}
