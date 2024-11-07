use ignis_token::token::Token;

use crate::expressions::ASTExpression;

use super::{variable::ASTVariable, ASTStatement};

#[derive(Debug, PartialEq, Clone)]
pub struct ASTForOf {
  pub variable: ASTVariable,
  pub iterable: Box<ASTExpression>,
  pub body: Box<ASTStatement>,
  pub token: Token,
}

impl ASTForOf {
  pub fn new(
    variable: ASTVariable,
    iterable: Box<ASTExpression>,
    body: Box<ASTStatement>,
    token: Token,
  ) -> Self {
    Self {
      variable,
      iterable,
      body,
      token,
    }
  }
}
