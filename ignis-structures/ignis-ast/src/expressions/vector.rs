use ignis_data_type::DataType;
use ignis_token::token::Token;

use super::ASTExpression;

#[derive(Debug, Clone, PartialEq)]
pub struct ASTVector {
  pub token: Token,
  pub elements: Vec<ASTExpression>,
  pub data_type: DataType,
}

impl ASTVector {
  pub fn new(
    token: Token,
    elements: Vec<ASTExpression>,
    data_type: DataType,
  ) -> Self {
    Self {
      token,
      elements,
      data_type,
    }
  }
}
