use super::ASTExpression;

#[derive(Debug, Clone, PartialEq)]
pub struct ASTGrouping {
  pub expression: Box<ASTExpression>,
}

impl ASTGrouping {
  pub fn new(expression: Box<ASTExpression>) -> Self {
    Self { expression }
  }
}
