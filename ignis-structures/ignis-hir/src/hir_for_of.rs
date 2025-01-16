use ignis_token::token::Token;
use serde::Serialize;

use crate::{hir_variable::HIRVariable, HIRInstruction};

#[derive(Debug, Clone, Serialize)]
pub struct HIRForOf {
  pub variable: HIRVariable,
  pub iterable: Box<HIRInstruction>,
  pub body: Box<HIRInstruction>,
  pub token: Token,
}

impl HIRForOf {
  pub fn new(
    variable: HIRVariable,
    iterable: Box<HIRInstruction>,
    body: Box<HIRInstruction>,
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
