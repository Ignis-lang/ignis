use ignis_token::token::Token;

use crate::metadata::ASTMetadata;

use super::ASTExpression;

#[derive(Debug, PartialEq, Clone)]
pub struct ASTMemberAccess {
  pub object: Box<ASTExpression>,
  pub member: Box<Token>,
  pub metadata: ASTMetadata,
}

impl ASTMemberAccess {
  pub fn new(
    object: Box<ASTExpression>,
    member: Box<Token>,
    metadata: ASTMetadata,
  ) -> Self {
    Self {
      object,
      member,
      metadata,
    }
  }
}
