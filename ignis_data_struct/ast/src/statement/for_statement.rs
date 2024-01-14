use crate::expression::Expression;
use super::{Statement, variable::Variable};

#[derive(Debug, PartialEq, Clone)]
pub struct For {
  pub variable: Box<Variable>,
  pub condition: Box<Expression>,
  pub increment: Box<Expression>,
  pub body: Box<Statement>,
}

impl For {
  pub fn new(
    variable: Box<Variable>,
    condition: Box<Expression>,
    increment: Box<Expression>,
    body: Box<Statement>,
  ) -> Self {
    Self {
      variable,
      condition,
      increment,
      body,
    }
  }
}
