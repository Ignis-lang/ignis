use enums::data_type::DataType;

use super::IRInstruction;

#[derive(Debug, Clone)]
pub struct IRReturn {
  pub value: Box<IRInstruction>,
  pub data_type: DataType,
}

impl IRReturn {
  pub fn new(value: Box<IRInstruction>, data_type: DataType) -> Self {
    Self { value, data_type }
  }

  pub fn to_json(&self) -> serde_json::Value {
    serde_json::json!({
      "type": "return",
      "value": self.value.to_json(),
      "data_type": self.data_type.to_string(),
    })
  }
}
