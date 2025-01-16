use ignis_data_type::DataType;
use ignis_token::token::Token;
use serde::Serialize;

use super::ASTExpression;

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct ASTTernary {
  pub condition: Box<ASTExpression>,
  pub then_branch: Box<ASTExpression>,
  pub else_branch: Box<ASTExpression>,
  pub token: Box<Token>,
  pub data_type: DataType,
}

impl ASTTernary {
  pub fn new(
    condition: Box<ASTExpression>,
    then_branch: Box<ASTExpression>,
    else_branch: Box<ASTExpression>,
    token: Box<Token>,
    data_type: DataType,
  ) -> Self {
    Self {
      condition,
      then_branch,
      else_branch,
      token,
      data_type,
    }
  }
}
