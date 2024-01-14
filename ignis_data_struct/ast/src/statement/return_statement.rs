use crate::expression::Expression;
use token::Token;

#[derive(Debug, PartialEq, Clone)]
pub struct Return {
  pub value: Option<Box<Expression>>,
  pub keyword: Box<Token>,
}

impl Return {
  pub fn new(value: Option<Box<Expression>>, keyword: Box<Token>) -> Self {
    Self { value, keyword }
  }
}
