use super::IRInstruction;

#[derive(Debug, Clone)]
pub struct IRIf {
  pub condition: Box<IRInstruction>,
  pub then_branch: Box<IRInstruction>,
  pub else_branch: Option<Box<IRInstruction>>,
}

impl IRIf {
  pub fn new(
    condition: Box<IRInstruction>,
    then_branch: Box<IRInstruction>,
    else_branch: Option<Box<IRInstruction>>,
  ) -> Self {
    Self {
      condition,
      then_branch,
      else_branch,
    }
  }
}
