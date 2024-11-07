use ignis_data_type::DataType;

use crate::statements::{variable::ASTVariable, ASTStatement};

#[derive(Debug, Clone, PartialEq)]
pub struct ASTLambda {
  pub parameters: Vec<ASTVariable>,
  pub body: Box<ASTStatement>,
  pub return_type: DataType,
  pub lambda_type: DataType,
}

impl ASTLambda {
  pub fn new(
    parameters: Vec<ASTVariable>,
    body: Box<ASTStatement>,
    return_type: DataType,
    lambda_type: DataType,
  ) -> Self {
    Self {
      parameters,
      body,
      return_type,
      lambda_type,
    }
  }
}
