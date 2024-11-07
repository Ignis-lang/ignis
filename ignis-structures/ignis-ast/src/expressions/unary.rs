use ignis_data_type::DataType;
use ignis_token::token::Token;

use super::ASTExpression;

#[derive(Debug, Clone, PartialEq)]
pub struct ASTUnary {
  pub operator: Token,
  pub right: Box<ASTExpression>,
  pub data_type: DataType,
  pub is_prefix: bool,
}

impl ASTUnary {
  pub fn new(
    operator: Token,
    right: Box<ASTExpression>,
    data_type: DataType,
    is_prefix: bool,
  ) -> Self {
    Self {
      operator,
      right,
      data_type,
      is_prefix,
    }
  }
}
