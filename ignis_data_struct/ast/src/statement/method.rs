use std::fmt::{Display, Formatter};

use token::token::Token;
use enums::data_type::DataType;
use serde_json::json;

use super::{
  Statement,
  function::{FunctionParameter, FunctionDecorator},
};

#[derive(Debug, Clone, PartialEq)]
pub struct MethodMetadata {
  pub is_public: bool,
  pub is_static: bool,
  pub is_contructor: bool,
}

impl MethodMetadata {
  pub fn new(is_public: bool, is_static: bool, is_contructor: bool) -> Self {
    Self {
      is_public,
      is_static,
      is_contructor,
    }
  }

  pub fn to_json(&self) -> serde_json::Value {
    json!({
      "is_public": self.is_public,
      "is_static": self.is_static,
      "is_constructor": self.is_contructor,
    })
  }
}

impl Display for MethodMetadata {
  fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
    write!(
      f,
      "is_public: {}, is_static: {}, is_constructor: {}",
      self.is_public, self.is_static, self.is_contructor
    )
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct MethodStatement {
  pub name: Token,
  pub parameters: Vec<FunctionParameter>,
  pub body: Vec<Statement>,
  pub return_type: Option<DataType>,
  pub annotations: Vec<FunctionDecorator>,
  pub metadata: MethodMetadata,
  pub class_name: Token,
}

impl MethodStatement {
  pub fn new(
    name: Token,
    parameters: Vec<FunctionParameter>,
    body: Vec<Statement>,
    return_type: Option<DataType>,
    annotations: Vec<FunctionDecorator>,
    metadata: MethodMetadata,
    class_name: Token,
  ) -> Self {
    Self {
      name,
      parameters,
      body,
      return_type,
      annotations,
      metadata,
      class_name
    }
  }
}
