use token::token::Token;

use super::variable::Variable;

#[derive(Debug, PartialEq, Clone)]
pub struct Enum {
  pub name: Token,
  pub members: Vec<Variable>,
  pub is_exported: bool,
  pub generic: Option<Vec<String>>,
}

impl Enum {
  pub fn new(
    name: Token,
    members: Vec<Variable>,
    is_exported: bool,
    generic: Option<Vec<String>>,
  ) -> Self {
    Self {
      name,
      members,
      is_exported,
      generic,
    }
  }
}
