use ignis_data_type::DataType;
use ignis_token::token::Token;
use serde::Serialize;

use crate::{HIRInstruction, HIRMetadata};

#[derive(Debug, Clone, Serialize)]
pub struct HIRMethodCall {
  pub name: Box<Token>,
  pub calle: Box<HIRInstruction>,
  pub arguments: Vec<HIRInstruction>,
  pub return_type: DataType,
  pub object: Box<HIRInstruction>,
  pub metadata: HIRMetadata,
}

impl HIRMethodCall {
  pub fn new(
    name: Box<Token>,
    calle: Box<HIRInstruction>,
    arguments: Vec<HIRInstruction>,
    return_type: DataType,
    object: Box<HIRInstruction>,
    metadata: HIRMetadata,
  ) -> Self {
    Self {
      name,
      calle,
      arguments,
      return_type,
      object,
      metadata,
    }
  }
}
