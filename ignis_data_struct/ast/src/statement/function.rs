use token::token::Token;
use enums::data_type::DataType;
use serde_json::json;

use super::Statement;

#[derive(Debug, Clone, PartialEq)]
pub enum FunctionDecorator {
  Extern(Token),
  Custom,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionParameter {
  pub name: Token,
  pub data_type: DataType,
  // TODO:
  pub is_mutable: bool,
  pub is_reference: bool,
}

impl FunctionParameter {
  pub fn new(name: Token, data_type: DataType, is_mutable: bool) -> Self {
    Self {
      name,
      data_type,
      is_mutable,
      is_reference: false,
    }
  }

  pub fn to_string(&self) -> String {
    format!("{}: {}", self.name.span.literal, self.data_type.to_string())
  }

  pub fn to_json(&self) -> serde_json::Value {
    json!({
      "name": self.name.span.literal,
      "data_type": self.data_type.to_string(),
      "is_mutable": self.is_mutable,
      "is_reference": self.is_reference,
    })
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionStatement {
  pub name: Token,
  pub parameters: Vec<FunctionParameter>,
  pub body: Vec<Statement>,
  pub return_type: Option<DataType>,
  pub is_exported: bool,
  pub annotations: Vec<FunctionDecorator>,
}

impl FunctionStatement {
  pub fn new(
    name: Token,
    parameters: Vec<FunctionParameter>,
    body: Vec<Statement>,
    return_type: Option<DataType>,
    is_exported: bool,
    annotations: Vec<FunctionDecorator>,
  ) -> Self {
    Self {
      name,
      parameters,
      body,
      return_type,
      is_exported,
      annotations
    }
  }
}
