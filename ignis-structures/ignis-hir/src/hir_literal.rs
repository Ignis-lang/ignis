use ignis_data_type::value::IgnisLiteralValue;
use ignis_token::token::Token;
use serde::Serialize;

#[derive(Debug, Clone, Serialize)]
pub struct HIRLiteral {
  pub value: IgnisLiteralValue,
  pub token: Token,
}

impl HIRLiteral {
  pub fn new(
    value: IgnisLiteralValue,
    token: Token,
  ) -> Self {
    Self { value, token }
  }
}
