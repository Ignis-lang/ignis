use ignis_data_type::DataType;
use ignis_token::token::Token;

use crate::statements::{method::ASTMethod, property::ASTProperty};

#[derive(Debug, Clone, PartialEq)]
pub struct ASTObject {
  pub token: Token,
  pub properties: Vec<ASTProperty>,
  pub methods: Vec<ASTMethod>,
  pub data_type: DataType,
}

impl ASTObject {
  pub fn new(
    token: Token,
    properties: Vec<ASTProperty>,
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
