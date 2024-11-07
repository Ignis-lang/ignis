use super::ASTExpression;

#[derive(Debug, PartialEq, Clone)]
pub struct ASTCall {
  pub callee: Box<ASTExpression>,
  pub arguments: Vec<ASTExpression>,
}

impl ASTCall {
  pub fn new(
    callee: Box<ASTExpression>,
    arguments: Vec<ASTExpression>,
  ) -> Self {
    Self {
      callee,
      arguments,
    }
  }
}
