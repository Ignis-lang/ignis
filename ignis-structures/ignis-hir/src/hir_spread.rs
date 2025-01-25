use ignis_data_type::DataType;
use ignis_token::token::Token;
use serde::Serialize;

use crate::HIRInstruction;

#[derive(Debug, Clone, Serialize)]
pub struct HIRSpread {
  pub expression: Box<HIRInstruction>,
  pub token: Token,
  pub data_type: DataType,
}

impl HIRSpread {
  pub fn new(
    expression: Box<HIRInstruction>,
    token: Token,
    data_type: DataType,
  ) -> Self {
    Self {
      expression,
      token,
      data_type,
    }
  }
}
