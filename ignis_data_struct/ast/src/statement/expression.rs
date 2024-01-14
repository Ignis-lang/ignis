use crate::expression::Expression;

#[derive(Debug, PartialEq, Clone)]
pub struct ExpressionStatement {
  pub expression: Box<Expression>,
}

impl ExpressionStatement {
  pub fn new(expression: Box<Expression>) -> Self {
    Self { expression }
  }
}
