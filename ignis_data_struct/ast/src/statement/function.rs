use token::token::Token;

use enums::data_type::DataType;

use super::{Statement, variable::Variable};

#[derive(Debug, Clone, PartialEq)]
pub enum FunctionDecorator {
  Extern(Token),
  Custom,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionStatement {
  pub name: Token,
  pub parameters: Vec<Variable>,
  pub body: Vec<Statement>,
  pub return_type: Option<DataType>,
  pub is_exported: bool,
  pub annotations: Vec<FunctionDecorator>,
}

impl FunctionStatement {
  pub fn new(
    name: Token,
    parameters: Vec<Variable>,
    body: Vec<Statement>,
    return_type: Option<DataType>,
    is_exported: bool,
    annotations: Vec<FunctionDecorator>,
  ) -> Self {
    Self {
      name,
      parameters,
      body,
      return_type,
      is_exported,
      annotations,
    }
  }
}
