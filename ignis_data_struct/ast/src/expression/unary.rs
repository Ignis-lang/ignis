use super::Expression;
use {token::Token, enums::data_type::DataType};

#[derive(Debug, PartialEq, Clone)]
pub struct Unary {
  pub operator: Token,
  pub right: Box<Expression>,
  pub data_type: DataType,
  pub is_prefix: bool,
}

impl Unary {
  pub fn new(operator: Token, right: Box<Expression>, data_type: DataType, is_prefix: bool) -> Self {
    Self {
      operator,
      right,
      data_type,
      is_prefix
    }
  }
}
