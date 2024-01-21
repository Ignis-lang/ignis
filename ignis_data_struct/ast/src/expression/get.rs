use token::token::Token;
use super::Expression;

#[derive(Debug, PartialEq, Clone)]
pub struct Get {
  pub object: Box<Expression>,
  pub object_token: Box<Token>,
  pub name: Box<Token>,
}

impl Get {
  pub fn new(object: Box<Expression>, object_token: Box<Token>, name: Box<Token>) -> Self {
    Self {
      object,
      object_token,
      name,
    }
  }
}
