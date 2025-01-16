use ignis_data_type::value::IgnisLiteralValue;
use ignis_token::token::Token;
use serde::Serialize;

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct ASTLiteral {
  pub value: IgnisLiteralValue,
  pub token: Token,
}

impl ASTLiteral {
  pub fn new(
    value: IgnisLiteralValue,
    token: Token,
  ) -> Self {
    Self { value, token }
  }
}
