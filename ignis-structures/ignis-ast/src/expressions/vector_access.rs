use ignis_token::token::Token;
use serde::Serialize;

use super::ASTExpression;

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct ASTVectorAccess {
  pub name: Box<Token>,
  pub variable: Box<ASTExpression>,
  pub index: Box<ASTExpression>,
}

impl ASTVectorAccess {
  pub fn new(
    name: Box<Token>,
    variable: Box<ASTExpression>,
    index: Box<ASTExpression>,
  ) -> Self {
    Self { name, variable, index }
  }
}
