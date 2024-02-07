use enums::data_type::DataType;

use super::{IRInstruction, IRInstructionTrait};

#[derive(Debug, Clone)]
pub struct IRArray {
  pub elements: Vec<IRInstruction>,
  pub data_type: DataType,
  pub lenght: usize,
}

impl IRArray {
  pub fn new(elements: Vec<IRInstruction>, data_type: DataType) -> Self {
    Self {
      lenght: elements.len(),
      elements,
      data_type,
    }
  }
}

impl IRInstructionTrait for IRArray {
  fn to_json(&self) -> serde_json::Value {
    serde_json::json!({
      "type": "IRArray",
      "elements": self.elements.iter().map(|x| x.to_json()).collect::<Vec<serde_json::Value>>(),
      "data_type": self.data_type.to_string(),
      "lenght": self.lenght,
    })
  }
}
