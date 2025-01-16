use serde::Serialize;

use crate::{expressions::ASTExpression, metadata::ASTMetadata};

use super::ASTStatement;

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct ASTExtern {
  pub name: Box<ASTExpression>,
  pub body: Vec<ASTStatement>,
  pub metadata: ASTMetadata,
}

impl ASTExtern {
  pub fn new(
    name: Box<ASTExpression>,
    body: Vec<ASTStatement>,
    metadata: ASTMetadata,
  ) -> Self {
    Self { name, body, metadata }
  }
}
