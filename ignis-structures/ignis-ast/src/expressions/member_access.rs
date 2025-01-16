use ignis_data_type::DataType;
use ignis_token::token::Token;
use serde::Serialize;

use crate::metadata::ASTMetadata;

use super::ASTExpression;

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct ASTMemberAccess {
  pub object: Box<ASTExpression>,
  pub member: Box<Token>,
  pub metadata: ASTMetadata,
  pub generics: Vec<DataType>
}

impl ASTMemberAccess {
  pub fn new(
    object: Box<ASTExpression>,
    member: Box<Token>,
    metadata: ASTMetadata,
    generics: Vec<DataType>,
  ) -> Self {
    Self {
      object,
      member,
      metadata,
      generics,
    }
  }
}
