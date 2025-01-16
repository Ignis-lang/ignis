use ignis_data_type::DataType;
use serde::Serialize;

use crate::statements::{function::ASTGenericParameter, variable::ASTVariable, ASTStatement};

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct ASTLambda {
  pub parameters: Vec<ASTVariable>,
  pub body: Box<ASTStatement>,
  pub return_type: DataType,
  pub lambda_type: DataType,
  pub generic_parameters: Vec<ASTGenericParameter>,
}

impl ASTLambda {
  pub fn new(
    parameters: Vec<ASTVariable>,
    body: Box<ASTStatement>,
    return_type: DataType,
    lambda_type: DataType,
    generic_parameters: Vec<ASTGenericParameter>,
  ) -> Self {
    Self {
      parameters,
      body,
      return_type,
      lambda_type,
      generic_parameters,
    }
  }
}
