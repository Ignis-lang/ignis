use enums::data_type::DataType;

use super::IRInstruction;

#[derive(Debug, Clone)]
pub struct IRCall {
  pub name: String,
  pub arguments: Vec<IRInstruction>,
  pub return_type: DataType,
}

impl IRCall {
  pub fn new(name: String, arguments: Vec<IRInstruction>, return_type: DataType) -> Self {
    Self {
      name,
      arguments,
      return_type,
    }
  }

  pub fn to_json(&self) -> serde_json::Value {
    serde_json::json!({
      "type": "call",
      "name": self.name,
      "arguments": self.arguments.iter().map(|x| x.to_json()).collect::<Vec<serde_json::Value>>(),
      "return_type": self.return_type.to_string(),
    })
  }
}
