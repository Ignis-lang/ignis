use serde::Serialize;

use super::ASTStatement;

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct ASTBlock {
  pub statements: Vec<ASTStatement>,
}

impl ASTBlock {
  pub fn new(statements: Vec<ASTStatement>) -> Self {
    Self { statements }
  }
}
