use token::token::Token;

use super::{Statement, property::PropertyStatement};

#[derive(Debug, Clone, PartialEq)]
pub struct Class {
  pub name: Token,
  pub methods: Vec<Statement>,
  pub properties: Vec<Statement>,
}

impl Class {
  pub fn new(name: Token, methods: Vec<Statement>, properties: Vec<Statement>) -> Self {
    Self {
      name,
      methods,
      properties,
    }
  }
}
