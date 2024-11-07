use crate::expressions::ASTExpression;

use super::ASTStatement;

#[derive(Debug, PartialEq, Clone)]
pub struct ASTWhile {
  pub condition: Box<ASTExpression>,
  pub body: Box<ASTStatement>,
}

impl ASTWhile {
  pub fn new(
    condition: Box<ASTExpression>,
    body: Box<ASTStatement>,
  ) -> Self {
    Self { condition, body }
  }
}
