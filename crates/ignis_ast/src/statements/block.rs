use crate::NodeId;

#[derive(Debug, Clone, PartialEq)]
pub struct ASTBlock {
  pub statements: Vec<NodeId>,
}

impl ASTBlock {
  pub fn new(statements: Vec<NodeId>) -> Self {
    Self { statements }
  }
}
