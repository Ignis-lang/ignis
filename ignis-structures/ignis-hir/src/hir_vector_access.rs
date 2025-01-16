use ignis_data_type::DataType;
use ignis_token::token::Token;
use serde::Serialize;

use crate::HIRInstruction;

#[derive(Clone, Debug, Serialize)]
pub struct HIRVectorAccess {
  pub name: Token,
  pub index: Box<HIRInstruction>,
  pub data_type: DataType,
}

impl HIRVectorAccess {
  pub fn new(
    name: Token,
    index: Box<HIRInstruction>,
    data_type: DataType,
  ) -> Self {
    Self { name, index, data_type }
  }
}
