use ignis_data_type::DataType;
use ignis_token::token::Token;
use serde::Serialize;

use crate::{hir_method::HIRMethod, HIRInstruction};

#[derive(Debug, Clone, Serialize)]
pub struct HIRObjectLiteral {
  pub properties: Vec<(Token, HIRInstruction)>,
  pub methods: Vec<HIRMethod>,
  pub data_type: DataType,
}

impl HIRObjectLiteral {
  pub fn new(
    properties: Vec<(Token, HIRInstruction)>,
    methods: Vec<HIRMethod>,
    data_type: DataType,
  ) -> Self {
    Self {
      properties,
      methods,
      data_type,
    }
  }
}
