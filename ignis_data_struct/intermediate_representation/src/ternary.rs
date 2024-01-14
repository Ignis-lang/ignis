use super::IRInstruction;

#[derive(Debug, Clone)]
pub struct IRTernary {
  pub condition: Box<IRInstruction>,
  pub then_branch: Box<IRInstruction>,
  pub else_branch: Box<IRInstruction>,
}

impl IRTernary {
  pub fn new(
    condition: Box<IRInstruction>,
    then_branch: Box<IRInstruction>,
    else_branch: Box<IRInstruction>,
  ) -> Self {
    Self {
      condition,
      then_branch,
      else_branch,
    }
  }
}
