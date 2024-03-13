use enums::data_type::{self, DataType};
use token::token::Token;

use crate::{variable::IRVariable, IRInstructionTrait};

#[derive(Debug, Clone)]
pub struct IREnum {
  pub name: Box<Token>,
  pub members: Vec<IRVariable>,
  pub is_exported: bool,
  pub generic: Option<Vec<String>>,
  pub data_type: DataType
}

impl IRInstructionTrait for IREnum {
  fn to_json(&self) -> serde_json::Value {
    let mut members = vec![];
    for member in &self.members {
      members.push(member.to_json());
    }
    serde_json::json!({
      "type": "IREnum",
      "name": self.name.to_string(),
      "members": members,
      "is_exported": self.is_exported,
      "generic": self.generic,
      "data_type": self.data_type.to_string(),
    })
  }
}

impl IREnum {
  pub fn new(
    name: Box<Token>,
    members: Vec<IRVariable>,
    data_type: DataType,
    is_exported: bool,
    generic: Option<Vec<String>>,
  ) -> Self {
    Self {
      name,
      members,
      data_type,
      is_exported,
      generic,
    }
  }
}
