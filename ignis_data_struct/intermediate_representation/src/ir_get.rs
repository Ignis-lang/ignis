use enums::data_type::DataType;
use super::{class::IRClass, IRInstructionTrait};

#[derive(Debug, Clone)]
pub struct IRGet {
  pub name: String,
  pub object: Box<IRClass>,
  pub data_type: DataType,
}

impl IRGet {
  pub fn new(name: String, object: Box<IRClass>, data_type: DataType) -> Self {
    Self {
      name,
      object,
      data_type,
    }
  }
}

impl IRInstructionTrait for IRGet {
  fn to_json(&self) -> serde_json::Value {
    serde_json::json!({
      "type": "IRGet",
      "name": self.name,
      "object": self.object.to_json(),
      "data_type": self.data_type.to_string(),
    })
  }
}
