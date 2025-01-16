use ignis_token::token::Token;
use serde::Serialize;

use super::ASTExpression;

#[derive(Debug, PartialEq, Clone, Serialize)]
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
