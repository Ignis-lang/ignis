use ignis_token::token::Token;

use super::ASTExpression;

#[derive(Debug, PartialEq, Clone)]
pub struct ASTAssignment {
  pub left: Box<ASTExpression>,
  pub operator: Box<Token>,
  pub right: Box<ASTExpression>,
}

impl ASTAssignment {
  pub fn new(
    left: Box<ASTExpression>,
    operator: Box<Token>,
    right: Box<ASTExpression>,
  ) -> Self {
    Self { left, operator, right }
  }
}
