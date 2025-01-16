use ignis_data_type::DataType;
use ignis_token::token::Token;
use serde::Serialize;

use crate::metadata::ASTMetadata;

use super::{variable::ASTVariable, ASTStatement};

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct ASTGenericParameter {
  pub name: Token,
  pub constraints: Vec<DataType>,
}

impl ASTGenericParameter {
  pub fn new(
    name: Token,
    constraints: Vec<DataType>,
  ) -> Self {
    Self { name, constraints }
  }
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct ASTFunction {
  pub name: Token,
  pub parameters: Vec<ASTVariable>,
  pub body: Vec<ASTStatement>,
  pub return_type: DataType,
  pub metadata: ASTMetadata,
  pub generic_parameters: Vec<ASTGenericParameter>,
}

impl ASTFunction {
  pub fn new(
    name: Token,
    parameters: Vec<ASTVariable>,
    body: Vec<ASTStatement>,
    return_type: DataType,
    metadata: ASTMetadata,
    generic_parameters: Vec<ASTGenericParameter>,
  ) -> Self {
    Self {
      name,
      parameters,
      body,
      return_type,
      metadata,
      generic_parameters,
    }
  }
}
