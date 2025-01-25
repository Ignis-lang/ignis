use ignis_data_type::DataType;
use ignis_token::token::Token;
use serde::Serialize;

use crate::{HIRInstruction, HIRMetadata};

#[derive(Debug, Clone, Serialize)]
pub struct HIREnumItem {
  pub name: Token,
  pub value: Option<HIRInstruction>,
  pub data_type: DataType,
  pub metadata: HIRMetadata,
}

impl HIREnumItem {
  pub fn new(
    name: Token,
    value: Option<HIRInstruction>,
    data_type: DataType,
    metadata: HIRMetadata,
  ) -> Self {
    Self {
      name,
      value,
      data_type,
      metadata,
    }
  }
}

#[derive(Debug, Clone, Serialize)]
pub struct HIREnum {
  pub name: Box<Token>,
  pub members: Vec<HIREnumItem>,
  pub metadata: HIRMetadata,
  pub generic: Vec<DataType>,
  pub data_type: DataType,
}

impl HIREnum {
  pub fn new(
    name: Box<Token>,
    members: Vec<HIREnumItem>,
    metadata: HIRMetadata,
    generic: Vec<DataType>,
    data_type: DataType,
  ) -> Self {
    Self {
      name,
      members,
      metadata,
      generic,
      data_type,
    }
  }
}
