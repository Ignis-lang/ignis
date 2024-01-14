use super::IRInstruction;

#[derive(Debug, Clone)]
pub struct IRWhile {
  pub condition: Box<IRInstruction>,
  pub body: Box<IRInstruction>,
}

impl IRWhile {
  pub fn new(condition: Box<IRInstruction>, body: Box<IRInstruction>) -> Self {
    Self { condition, body }
  }
}
