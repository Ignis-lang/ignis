use super::ASTStatement;

#[derive(Debug, PartialEq, Clone)]
pub struct ASTBlock {
  pub statements: Vec<ASTStatement>,
}

impl ASTBlock {
  pub fn new(statements: Vec<ASTStatement>) -> Self {
    Self { statements }
  }
}
