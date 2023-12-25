use token::token::Token;

use super::{function::FunctionStatement, variable::Variable};

#[derive(Debug, Clone, PartialEq)]
pub struct Class {
  pub name: Token,
  pub methods: Vec<FunctionStatement>,
  pub attributes: Vec<Variable>,
}

impl Class {
  pub fn new(name: Token, methods: Vec<FunctionStatement>, attributes: Vec<Variable>) -> Self {
    Self {
      name,
      methods,
      attributes,
    }
  }
}
