use token::token::Token;
use super::Expression;

#[derive(Debug, PartialEq, Clone)]
pub struct NewExpression {
  pub name: Token,
  pub arguments: Vec<Expression>,
}

impl NewExpression {
  pub fn new(
    name: Token,
    arguments: Vec<Expression>,
  ) -> Self {
    Self { name, arguments }
  }
}
