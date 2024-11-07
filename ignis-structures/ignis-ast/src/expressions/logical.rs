use ignis_data_type::DataType;
use ignis_token::token::Token;

use super::ASTExpression;

#[derive(Debug, Clone, PartialEq)]
pub struct ASTLogical {
  pub left: Box<ASTExpression>,
  pub operator: Token,
  pub right: Box<ASTExpression>,
  pub data_type: DataType,
}

impl ASTLogical {
  pub fn new(
    left: Box<ASTExpression>,
    operator: Token,
    right: Box<ASTExpression>,
  ) -> Self {
    Self {
      left,
      operator,
      right,
      data_type: DataType::Boolean,
    }
  }
}
