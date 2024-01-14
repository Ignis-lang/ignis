use super::IRInstruction;

#[repr(C)]
#[derive(Debug, Clone)]
pub struct IRAssign {
  pub name: String,
  pub value: Box<IRInstruction>,
}

impl IRAssign {
  pub fn new(name: String, value: Box<IRInstruction>) -> Self {
    Self { name, value }
  }
}
