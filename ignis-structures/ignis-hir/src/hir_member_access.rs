use ignis_token::token::Token;
use serde::Serialize;

use crate::{HIRInstruction, HIRMetadata};

#[derive(Debug, Clone, Serialize)]
pub struct HIRMemberAccess {
  pub object: Box<HIRInstruction>,
  pub member: Box<Token>,
  pub metadata: HIRMetadata,
}

impl HIRMemberAccess {
  pub fn new(
    object: Box<HIRInstruction>,
    member: Box<Token>,
    metadata: HIRMetadata,
  ) -> Self {
    Self {
      object,
      member,
      metadata,
    }
  }
}
