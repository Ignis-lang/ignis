use ignis_data_type::DataType;
use ignis_token::token::Token;

use super::ASTExpression;

#[derive(Debug, Clone, PartialEq)]
pub struct ASTBinary {
  pub left: Box<ASTExpression>,
  pub operator: Token,
  pub right: Box<ASTExpression>,
  pub data_type: DataType,
}

impl ASTBinary {
  pub fn new(
    left: Box<ASTExpression>,
    operator: Token,
    right: Box<ASTExpression>,
    data_type: DataType,
  ) -> Self {
    Self {
      left,
      operator,
      right,
      data_type,
    }
  }
}
