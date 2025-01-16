use ignis_data_type::DataType;
use ignis_token::token::Token;
use serde::Serialize;

use crate::HIRInstruction;

#[derive(Clone, Debug, Serialize)]
pub struct HIRCast {
  pub target_type: DataType,
  pub operand: Box<HIRInstruction>,
  pub token: Token,
}

impl HIRCast {
  pub fn new(
    target_type: DataType,
    operand: Box<HIRInstruction>,
    token: Token,
  ) -> Self {
    Self {
      target_type,
      operand,
      token,
    }
  }
}
