use token::token::Token;
use enums::data_type::DataType;

use super::Expression;

#[derive(Debug, PartialEq, Clone)]
pub struct Call {
  pub callee: Box<Expression>,
  pub paren: Token,
  pub arguments: Vec<Expression>,
  pub type_arguments: Vec<DataType>,
  pub return_type: DataType,
}

impl Call {
  pub fn new(
    callee: Box<Expression>,
    paren: Token,
    arguments: Vec<Expression>,
    type_arguments: Vec<DataType>,
    return_type: DataType,
  ) -> Self {
    Self {
      callee,
      paren,
      arguments,
      type_arguments,
      return_type,
    }
  }
}
