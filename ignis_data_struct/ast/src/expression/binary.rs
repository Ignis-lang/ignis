use token::token::Token;
use enums::data_type::DataType;
use super::Expression;

#[derive(Debug, PartialEq, Clone)]
pub struct Binary {
  pub left: Box<Expression>,
  pub operator: Token,
  pub right: Box<Expression>,
  pub data_type: DataType,
}

impl Binary {
  pub fn new(
    left: Box<Expression>,
    operator: Token,
    right: Box<Expression>,
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
