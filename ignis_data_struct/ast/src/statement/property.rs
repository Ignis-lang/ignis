use enums::data_type::DataType;
use token::token::Token;

use crate::expression::Expression;
use super::variable::VariableMetadata;

#[derive(Debug, PartialEq, Clone)]
pub struct PropertyStatement {
  pub name: Box<Token>,
  pub initializer: Option<Box<Expression>>,
  pub type_annotation: DataType,
  pub metadata: VariableMetadata,
}

impl PropertyStatement {
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
