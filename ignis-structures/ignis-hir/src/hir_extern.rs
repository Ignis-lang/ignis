use ignis_token::token::Token;
use serde::Serialize;

use crate::{HIRInstruction, HIRMetadata};

#[derive(Debug, Clone, Serialize)]
pub struct HIRExtern {
  pub name: Token,
  pub body: Vec<HIRInstruction>,
  pub metadata: HIRMetadata,
}

impl HIRExtern {
  pub fn new(
    name: Token,
    body: Vec<HIRInstruction>,
    metadata: HIRMetadata,
  ) -> Self {
    Self { name, body, metadata }
  }
}
