use super::{IRInstruction, variable::IRVariable};

#[derive(Debug, Clone)]
pub struct IRBlock {
  pub instructions: Vec<IRInstruction>,
  pub scopes_variables: Vec<IRVariable>,
}

impl IRBlock {
  pub fn new(instructions: Vec<IRInstruction>, scopes_variables: Vec<IRVariable>) -> Self {
    Self {
      instructions,
      scopes_variables,
    }
  }
}
