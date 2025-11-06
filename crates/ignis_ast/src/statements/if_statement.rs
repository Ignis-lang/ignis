use crate::NodeId;

#[derive(Debug, Clone, PartialEq)]
pub struct ASTIf {
  pub condition: NodeId,
  pub then_block: NodeId,
  pub else_block: Option<NodeId>,
}

impl ASTIf {
  pub fn new(
    condition: NodeId,
    then_block: NodeId,
    else_block: Option<NodeId>,
  ) -> Self {
    Self {
      condition,
      then_block,
      else_block,
    }
  }
}
