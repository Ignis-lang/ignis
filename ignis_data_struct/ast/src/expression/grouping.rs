use super::Expression;

#[derive(Debug, PartialEq, Clone)]
pub struct Grouping {
  pub expression: Box<Expression>,
}

impl Grouping {
  pub fn new(expression: Box<Expression>) -> Self {
    Self { expression }
  }
}
