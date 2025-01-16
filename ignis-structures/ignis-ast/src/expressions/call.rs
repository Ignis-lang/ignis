use ignis_data_type::DataType;
use ignis_token::token::Token;
use serde::Serialize;

use super::ASTExpression;

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct ASTCall {
  pub name: Token,
  pub callee: Box<ASTExpression>,
  pub arguments: Vec<ASTExpression>,
  pub generics: Vec<DataType>,
}

impl ASTCall {
  pub fn new(
    name: Token,
    callee: Box<ASTExpression>,
    arguments: Vec<ASTExpression>,
    generics: Vec<DataType>,
  ) -> Self {
    Self {
      name,
      callee,
      arguments,
      generics,
    }
  }
}
