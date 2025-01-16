use serde::Serialize;

use crate::expressions::ASTExpression;

use super::{variable::ASTVariable, ASTStatement};

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct ASTFor {
  pub variable: Box<ASTVariable>,
  pub condition: Box<ASTExpression>,
  pub increment: Box<ASTExpression>,
  pub body: Box<ASTStatement>,
}

impl ASTFor {
  pub fn new(
    variable: Box<ASTVariable>,
    condition: Box<ASTExpression>,
    increment: Box<ASTExpression>,
    body: Box<ASTStatement>,
  ) -> Self {
    Self {
      variable,
      condition,
      increment,
      body,
    }
  }
}
