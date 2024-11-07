use ignis_token::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct ASTThis {
  pub token: Token,
}

impl ASTThis {
  pub fn new(token: Token) -> Self {
    Self { token }
  }
}
