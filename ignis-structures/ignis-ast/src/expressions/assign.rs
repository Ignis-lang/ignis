use ignis_token::token::{self, Token};
use serde::Serialize;

use super::ASTExpression;

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct ASTAssignment {
  pub token: Token,
  pub left: Box<ASTExpression>,
  pub right: Box<ASTExpression>,
}

impl ASTAssignment {
  pub fn new(
    token: Token,
    left: Box<ASTExpression>,
    right: Box<ASTExpression>,
  ) -> Self {
    Self { token, left, right }
  }
}
