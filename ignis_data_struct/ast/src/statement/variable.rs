use enums::data_type::DataType;
use token::token::Token;

use crate::expression::Expression;

#[derive(Debug, PartialEq, Clone)]
pub struct VariableMetadata {
  pub is_mutable: bool,
  pub is_global: bool,
  pub is_static: bool,
  pub is_public: bool,
  pub is_reference: bool,
}

impl VariableMetadata {
  pub fn new(
    is_mutable: bool,
    is_global: bool,
    is_static: bool,
    is_public: bool,
    is_reference: bool,
  ) -> Self {
    Self {
      is_mutable,
      is_global,
      is_static,
      is_public,
      is_reference,
    }
  }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Variable {
  pub name: Box<Token>,
  pub initializer: Option<Box<Expression>>,
  pub type_annotation: DataType,
  pub metadata: VariableMetadata,
}

impl Variable {
  pub fn new(
    name: Box<Token>,
    initializer: Option<Box<Expression>>,
    type_annotation: DataType,
    metadata: VariableMetadata,
  ) -> Self {
    Self {
      name,
      initializer,
      type_annotation,
      metadata,
    }
  }
}
