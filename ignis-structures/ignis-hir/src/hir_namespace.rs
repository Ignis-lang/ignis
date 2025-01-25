use ignis_token::token::Token;
use serde::Serialize;

use crate::{HIRInstruction, HIRMetadata};

#[derive(Debug, Clone, Serialize)]
pub struct HIRNamespace {
  pub name: Token,
  pub members: Vec<HIRInstruction>,
  pub metadata: HIRMetadata,
}

impl HIRNamespace {
  pub fn new(
    name: Token,
    members: Vec<HIRInstruction>,
    metadata: HIRMetadata,
  ) -> Self {
    Self {
      name,
      members,
      metadata,
    }
  }
}
