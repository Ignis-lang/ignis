use serde::Serialize;

use crate::statements::ASTStatement;

use super::ASTExpression;

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct ASTMatchCase {
  pub pattern: Vec<ASTExpression>,
  pub when: Option<ASTExpression>,
  pub block: Box<ASTStatement>,
}

impl ASTMatchCase {
  pub fn new(
    pattern: Vec<ASTExpression>,
    when: Option<ASTExpression>,
    block: Box<ASTStatement>,
  ) -> Self {
    Self { pattern, when, block }
  }
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct ASTMatchExpression {
  pub expression: Box<ASTExpression>,
  pub cases: Vec<ASTMatchCase>,
}

impl ASTMatchExpression {
  pub fn new(
    expression: Box<ASTExpression>,
    cases: Vec<ASTMatchCase>,
  ) -> Self {
    Self { expression, cases }
  }
}
