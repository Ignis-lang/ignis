use enums::data_type::DataType;

use crate::{IRInstruction, IRInstructionTrait};

#[derive(Debug, Clone)]
pub struct IRArrayAccess {
  pub name: String,
  pub index: Box<IRInstruction>,
  pub data_type: DataType,
}

impl IRArrayAccess {
  pub fn new(name: String, index: Box<IRInstruction>, data_type: DataType) -> Self {
    Self {
      name,
      index,
      data_type,
    }
  }
}

impl IRInstructionTrait for IRArrayAccess {
  fn to_json(&self) -> serde_json::Value {
    serde_json::json!({
      "type": "IRArrayAccess",
      "name": self.name,
      "index": self.index.to_json(),
      "data_type": self.data_type.to_string(),
    })
  }
}
