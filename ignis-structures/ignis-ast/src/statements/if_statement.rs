use serde::Serialize;

use crate::expressions::ASTExpression;

use super::ASTStatement;

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct ASTIf {
  pub condition: Box<ASTExpression>,
  pub then_branch: Box<ASTStatement>,
  pub else_branch: Option<Box<ASTStatement>>,
}

impl ASTIf {
  pub fn new(
    condition: Box<ASTExpression>,
    then_branch: Box<ASTStatement>,
    else_branch: Option<Box<ASTStatement>>,
  ) -> Self {
    Self {
      condition,
      then_branch,
      else_branch,
    }
  }
}
