use enums::data_type::DataType;

use super::IRInstruction;

#[derive(Debug, Clone)]
pub struct IRArray {
  pub elements: Vec<IRInstruction>,
  pub data_type: DataType,
}

impl IRArray {
  pub fn new(elements: Vec<IRInstruction>, data_type: DataType) -> Self {
    Self {
      elements,
      data_type,
    }
  }

  pub fn to_json(&self) -> serde_json::Value {
    serde_json::json!({
      "type": "array",
      "elements": self.elements.iter().map(|x| x.to_json()).collect::<Vec<serde_json::Value>>(),
      "data_type": self.data_type.to_string(),
    })
  }
}
