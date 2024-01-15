use enums::data_type::DataType;
use token::token::Token;

use super::Expression;

#[derive(Debug, PartialEq, Clone)]
pub struct Array {
  pub token: Token,
  pub elements: Vec<Expression>,
  pub data_type: DataType,
}

impl Array {
  pub fn new(token: Token, elements: Vec<Expression>, data_type: DataType) -> Self {
    Self {
      token,
      elements,
      data_type,
    }
  }
}
