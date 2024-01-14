use token::token::Token;

use super::{variable::IRVariable, IRInstruction};

#[derive(Debug, Clone)]
pub struct IRForIn {
  pub variable: IRVariable,
  pub iterable: Box<IRInstruction>,
  pub body: Box<IRInstruction>,
  pub token: Token,
}

impl IRForIn {
  pub fn new(
    variable: IRVariable,
    iterable: Box<IRInstruction>,
    body: Box<IRInstruction>,
    token: Token,
  ) -> Self {
    Self {
      variable,
      iterable,
      body,
      token,
    }
  }
}
