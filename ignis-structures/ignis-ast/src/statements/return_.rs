use ignis_token::token::Token;
use serde::Serialize;

use crate::expressions::ASTExpression;

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct ASTReturn {
  pub value: Option<Box<ASTExpression>>,
  pub token: Token,
}

impl ASTReturn {
  pub fn new(
    value: Option<Box<ASTExpression>>,
    token: Token,
  ) -> Self {
    Self { value, token }
  }
}
