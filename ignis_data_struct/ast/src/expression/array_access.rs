use token::token::Token;

use super::Expression;

#[derive(Debug, PartialEq, Clone)]
pub struct ArrayAccess {
  pub name: Box<Token>,
  pub variable: Box<Expression>,
  pub index: Box<Expression>,
}

impl ArrayAccess {
  pub fn new(
    name: Box<Token>,
    variable: Box<Expression>,
    index: Box<Expression>,
  ) -> Self {
    Self { name, variable, index }
  }
}
