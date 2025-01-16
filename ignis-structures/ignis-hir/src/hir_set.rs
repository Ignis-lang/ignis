use ignis_data_type::DataType;
use ignis_token::token::Token;
use serde::Serialize;

use crate::HIRInstruction;

#[derive(Debug, Clone, Serialize)]
pub struct HIRSet {
  pub name: Token,
  pub value: Box<HIRInstruction>,
  pub object: Box<HIRInstruction>,
  pub var_name: String,
  pub data_type: DataType,
}

impl HIRSet {
  pub fn new(
    name: Token,
    value: Box<HIRInstruction>,
    object: Box<HIRInstruction>,
    var_name: String,
    data_type: DataType,
  ) -> Self {
    Self {
      name,
      value,
      object,
      var_name,
      data_type,
    }
  }
}
