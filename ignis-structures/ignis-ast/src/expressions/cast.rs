use ignis_data_type::DataType;
use ignis_token::token::Token;

use super::ASTExpression;

#[derive(Debug, Clone, PartialEq)]
pub struct ASTCast {
  pub token: Token,
  pub target_type: DataType,
  pub operand: Box<ASTExpression>,
}

impl ASTCast {
  pub fn new(
    token: Token,
    target_type: DataType,
    operand: Box<ASTExpression>,
  ) -> Self {
    Self {
      token,
      target_type,
      operand,
    }
  }
}
