use token::token::Token;

use crate::expression::Expression;

use super::{Statement, variable::Variable};

#[derive(Debug, PartialEq, Clone)]
pub struct ForOf {
  pub variable: Variable,
  pub iterable: Box<Expression>,
  pub body: Box<Statement>,
  pub token: Token,
}

impl ForOf {
  pub fn new(variable: Variable, iterable: Expression, body: Statement, token: Token) -> Self {
    Self {
      variable,
      iterable: Box::new(iterable),
      body: Box::new(body),
      token,
    }
  }
}
