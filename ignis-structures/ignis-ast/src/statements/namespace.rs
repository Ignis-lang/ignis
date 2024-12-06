use crate::{expressions::ASTExpression, metadata::ASTMetadata};

use super::ASTStatement;

#[derive(Debug, Clone, PartialEq)]
pub struct ASTNamespace {
  pub name: Box<ASTExpression>,
  pub members: Vec<ASTStatement>,
  pub metadata: ASTMetadata,
}

impl ASTNamespace {
  pub fn new(
    name: Box<ASTExpression>,
    members: Vec<ASTStatement>,
    metadata: ASTMetadata,
  ) -> Self {
    Self {
      name,
      members,
      metadata,
    }
  }
}
