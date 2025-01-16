use ignis_data_type::DataType;
use ignis_token::token::Token;
use serde::Serialize;

use crate::{hir_block::HIRBlock, HIRInstruction, HIRMetadata};

#[derive(Debug, Clone, Serialize)]
pub struct HIRFunctionInstance {
  pub original_name: Box<Token>,
  pub instance_name: Box<Token>,
  pub instantiated_types: Vec<DataType>,
  pub parameters: Vec<HIRInstruction>,
  pub return_type: DataType,
  pub body: Option<Box<HIRBlock>>,
  pub metadata: HIRMetadata,
}

impl HIRFunctionInstance {
  pub fn new(
    original_name: Box<Token>,
    instance_name: Box<Token>,
    instantiated_types: Vec<DataType>,
    parameters: Vec<HIRInstruction>,
    return_type: DataType,
    body: Option<Box<HIRBlock>>,
    metadata: HIRMetadata,
  ) -> Self {
    Self {
      original_name,
      instance_name,
      instantiated_types,
      parameters,
      return_type,
      body,
      metadata,
    }
  }
}
