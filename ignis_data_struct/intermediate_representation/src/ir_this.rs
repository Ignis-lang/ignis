use enums::data_type::{DataType, self};
use token::token::Token;
use crate::{IRInstructionTrait, IRInstruction};

#[derive(Debug, Clone)]
pub struct IRThis {
  pub keyword: Box<Token>,
  pub access: Option<Box<IRInstruction>>,
  pub data_type: Box<DataType>,
}

impl IRThis {
  pub fn new(keyword: Box<Token>, access: Option<Box<IRInstruction>>, data_type: Box<DataType>) -> Self {
    Self { keyword, access, data_type }
  }
}

impl IRInstructionTrait for IRThis {
  fn to_json(&self) -> serde_json::Value {
    serde_json::json!({
      "type": "This",
      "keyword": self.keyword.to_json(),
      "data_type": self.data_type.to_string(),
      "access": if let Some(access) = &self.access {
        access.to_json()
      } else {
        serde_json::Value::Null
      }
    })
  }
}
