use enums::data_type::DataType;
use token::token::Token;

use super::Expression;

#[derive(Debug, PartialEq, Clone)]
pub struct Set {
  pub name: Box<Token>,
  pub value: Box<Expression>,
  pub object: Box<Expression>,
  pub data_type: DataType,
}

impl Set {
  pub fn new(
    name: Box<Token>,
    value: Box<Expression>,
    object: Box<Expression>,
    data_type: DataType,
  ) -> Self {
    Self {
      name,
      value,
      object,
      data_type,
    }
  }
}
