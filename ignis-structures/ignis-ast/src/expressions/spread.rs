use ignis_token::token::Token;

use super::ASTExpression;

#[derive(Debug, Clone, PartialEq)]
pub struct ASTSpread {
  pub expression: Box<ASTExpression>,
  pub token: Token,
}

impl ASTSpread {
  pub fn new(
    expression: Box<ASTExpression>,
    token: Token,
  ) -> Self {
    Self { expression, token }
  }
}
