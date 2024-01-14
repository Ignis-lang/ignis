use token::token::Token;

#[derive(Debug, Clone)]
pub struct IRContinue {
  pub token: Token,
}

impl IRContinue {
  pub fn new(token: Token) -> Self {
    Self { token }
  }
}
