use crate::NodeId;

#[derive(Debug, Clone, PartialEq)]
pub struct ASTFor {
  pub initializer: NodeId,
  pub condition: NodeId,
  pub increment: NodeId,
  pub body: NodeId,
}

impl ASTFor {
  pub fn new(
    initializer: NodeId,
    condition: NodeId,
    increment: NodeId,
    body: NodeId,
  ) -> Self {
    Self {
      initializer,
      condition,
      increment,
      body,
    }
  }
}
