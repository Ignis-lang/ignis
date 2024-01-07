use enums::data_type::DataType;
use token::token::Token;

use super::Expression;

#[derive(Debug, PartialEq, Clone)]
pub struct MethodCall {
  pub name: Box<Token>,
  pub calle: Box<Expression>,
  pub data_type: DataType,
  pub instance: Box<Expression>,
}

impl MethodCall {
  pub fn new(
    name: Box<Token>,
    calle: Box<Expression>,
    data_type: DataType,
    instance: Box<Expression>,
  ) -> Self {
    Self {
      name,
      calle,
      data_type,
      instance,
    }
  }
}
