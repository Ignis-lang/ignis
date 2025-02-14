use ignis_token::token::Token;
use serde::Serialize;

use crate::{HIRInstruction, HIRMetadata, HIRMetadataFlags};

#[derive(Debug, Clone, Serialize)]
pub struct HIRMemberAccess {
  pub object: Box<HIRInstruction>,
  pub member: Box<HIRInstruction>,
  pub metadata: HIRMetadata,
}

impl HIRMemberAccess {
  pub fn new(
    object: Box<HIRInstruction>,
    member: Box<HIRInstruction>,
    metadata: HIRMetadata,
  ) -> Self {
    Self {
      object,
      member,
      metadata,
    }
  }
}
