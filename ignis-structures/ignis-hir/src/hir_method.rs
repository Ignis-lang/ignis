use ignis_data_type::DataType;
use ignis_token::token::Token;
use serde::Serialize;

use crate::{hir_block::HIRBlock, HIRInstruction, HIRMetadata};

#[derive(Debug, Clone, Serialize)]
pub struct HIRMethod {
  pub name: Token,
  pub parameters: Vec<HIRInstruction>,
  pub return_type: DataType,
  pub body: Option<Box<HIRBlock>>,
  pub metadata: HIRMetadata,
  pub object: Token,
}

impl HIRMethod {
  pub fn new(
    name: Token,
    parameters: Vec<HIRInstruction>,
    return_type: DataType,
    body: Option<Box<HIRBlock>>,
    metadata: HIRMetadata,
    object: Token,
  ) -> Self {
    Self {
      name,
      parameters,
      return_type,
      body,
      metadata,
      object,
    }
  }
}
