use token::token::Token;

use super::Statement;

#[derive(Debug, Clone, PartialEq)]
pub struct InterfaceStatement {
  pub name: Token,
  pub methods: Vec<Statement>,
}

impl InterfaceStatement {
  pub fn new(
    name: Token,
    methods: Vec<Statement>,
  ) -> Self {
    Self { name, methods }
  }
}
