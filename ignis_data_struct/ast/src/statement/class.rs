use token::token::Token;

use super::function::FunctionStatement;

#[derive(Debug, Clone, PartialEq)]
pub struct Class {
  pub name: Token,
  pub methods: Vec<FunctionStatement>,
}

impl Class {
  pub fn new(name: Token, methods: Vec<FunctionStatement>) -> Self {
    Self { name, methods }
  }
}
