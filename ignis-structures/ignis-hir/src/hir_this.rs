use ignis_data_type::DataType;
use ignis_token::token::Token;
use serde::Serialize;

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct HIRThis {
  pub token: Token,
  pub data_type: DataType,
}

impl HIRThis {
  pub fn new(
    token: Token,
    data_type: DataType,
  ) -> Self {
    Self { token, data_type }
  }
}
