use token::token::Token;

use super::{function::FunctionStatement, variable::Variable};

#[derive(Debug, Clone, PartialEq)]
pub struct Class {
  pub name: Token,
  pub methods: Vec<FunctionStatement>,
  pub properties: Vec<Variable>,
}

impl Class {
  pub fn new(name: Token, methods: Vec<FunctionStatement>, properties: Vec<Variable>) -> Self {
    Self {
      name,
      methods,
      properties,
    }
  }
}
