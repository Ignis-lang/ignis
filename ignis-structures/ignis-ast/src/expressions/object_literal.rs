use ignis_data_type::DataType;
use ignis_token::token::Token;
use serde::Serialize;

use crate::statements::method::ASTMethod;

use super::ASTExpression;

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct ASTObject {
  pub token: Token,
  pub properties: Vec<(Token, ASTExpression)>,
  pub methods: Vec<ASTMethod>,
  pub data_type: DataType,
}

impl ASTObject {
  pub fn new(
    token: Token,
    properties: Vec<(Token, ASTExpression)>,
    methods: Vec<ASTMethod>,
    data_type: DataType,
  ) -> Self {
    Self {
      token,
      properties,
      methods,
      data_type,
    }
  }
}
