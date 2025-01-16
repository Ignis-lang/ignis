use super::HIRInstruction;
use ignis_token::token::Token;
use serde::Serialize;

#[derive(Debug, Clone, Serialize)]
pub struct HIRAssign {
  pub token: Token,
  pub left: Box<HIRInstruction>,
  pub right: Box<HIRInstruction>,
}

impl HIRAssign {
  pub fn new(
    token: Token,
    left: Box<HIRInstruction>,
    right: Box<HIRInstruction>,
  ) -> Self {
    Self { token, left, right }
  }
}
