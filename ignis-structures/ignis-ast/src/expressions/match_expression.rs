use crate::statements::ASTStatement;

use super::ASTExpression;

#[derive(Debug, Clone, PartialEq)]
pub struct ASTMatchCase {
  pub pattern: Box<Vec<ASTExpression>>,
  pub when: Option<Box<ASTExpression>>,
  pub block: Box<ASTStatement>,
}

impl ASTMatchCase {
  pub fn new(
    pattern: Box<Vec<ASTExpression>>,
    when: Option<Box<ASTExpression>>,
    block: Box<ASTStatement>,
  ) -> Self {
    Self { pattern, when, block }
  }

  pub fn to_json(&self) -> serde_json::Value {
    serde_json::json!({
      "pattern": self.pattern.iter().map(|p| p.to_json()).collect::<Vec<serde_json::Value>>(),
      "when": self.when.as_ref().map(|w| w.to_json()),
      "block": self.block.to_json(),
    })
  }
}

#[derive(Debug, Clone, PartialEq)]
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
