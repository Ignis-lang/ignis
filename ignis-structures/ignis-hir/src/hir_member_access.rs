use ignis_token::token::Token;
use serde::Serialize;

use crate::{HIRInstruction, HIRMetadata, HIRMetadataFlags};

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

  pub fn get_name(&self) -> String {
    let mut name = String::new();

    match &self.object.as_ref() {
      HIRInstruction::Variable(v) => name.push_str(&v.name.lexeme),
      HIRInstruction::MemberAccess(m) => name.push_str(&m.get_name()),
      _ => todo!(),
    }

    if self.metadata.is(HIRMetadataFlags::NamespaceMember) {
      name.push_str("::");
    }

    name.push_str(&self.member.lexeme);

    name
  }
}
