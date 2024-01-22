use token::token::Token;

use super::Statement;

#[derive(Debug, Clone, PartialEq)]
pub struct ClassMetadata {
  pub is_exported: bool,
}

impl ClassMetadata {
  pub fn new(is_exported: bool) -> Self {
    Self { is_exported }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Class {
  pub name: Token,
  pub methods: Vec<Statement>,
  pub properties: Vec<Statement>,
  pub metadata: ClassMetadata,
}

impl Class {
  pub fn new(
    name: Token,
    methods: Vec<Statement>,
    properties: Vec<Statement>,
    metadata: ClassMetadata,
  ) -> Self {
    Self {
      name,
      methods,
      properties,
      metadata,
    }
  }
}
