use token::token::Token;
use super::Expression;

#[derive(Debug, PartialEq, Clone)]
pub struct Get {
  pub object: Box<Expression>,
  pub name: Token,
}

impl Get {
  pub fn new(object: Box<Expression>, name: Token) -> Self {
    Self { object, name }
  }
}
