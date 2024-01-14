use crate::expression::Expression;

use super::Statement;

#[derive(Debug, PartialEq, Clone)]
pub struct WhileStatement {
  pub condition: Box<Expression>,
  pub body: Box<Statement>,
}

impl WhileStatement {
  pub fn new(condition: Box<Expression>, body: Box<Statement>) -> Self {
    Self { condition, body }
  }
}
