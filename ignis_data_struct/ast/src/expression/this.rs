use token::token::Token;

use super::Expression;

#[derive(Debug, PartialEq, Clone)]
pub struct This {
  pub keyword: Token,
  pub access: Option<Box<Expression>>,
}

impl This {
  pub fn new(
    keyword: Token,
    access: Option<Box<Expression>>,
  ) -> Self {
    Self { keyword, access }
  }
}
