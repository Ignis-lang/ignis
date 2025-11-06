use crate::NodeId;

#[derive(Debug, Clone, PartialEq)]
pub struct ASTWhile {
  pub condition: NodeId,
  pub body: NodeId,
}

impl ASTWhile {
  pub fn new(
    condition: NodeId,
    body: NodeId,
  ) -> Self {
    Self { condition, body }
  }
}
