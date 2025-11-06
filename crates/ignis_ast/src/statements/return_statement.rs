use crate::NodeId;

#[derive(Debug, Clone, PartialEq)]
pub struct ASTReturn {
  pub expression: NodeId,
}

impl ASTReturn {
  pub fn new(expression: NodeId) -> Self {
    Self { expression }
  }
}
