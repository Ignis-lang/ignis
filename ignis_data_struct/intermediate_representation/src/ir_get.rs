use enums::data_type::DataType;
use super::{IRInstruction, IRInstructionTrait};

#[derive(Debug, Clone)]
pub struct GetMetadata {
  pub object_data_type: DataType,
}

impl GetMetadata {
  pub fn new(object_data_type: DataType) -> Self {
    Self { object_data_type }
  }
}

#[derive(Debug, Clone)]
pub struct IRGet {
  pub name: String,
  pub object: Box<IRInstruction>,
  pub data_type: DataType,
  pub metadata: GetMetadata,
}

impl IRGet {
  pub fn new(
    name: String,
    object: Box<IRInstruction>,
    data_type: DataType,
    metadata: GetMetadata,
  ) -> Self {
    Self {
      name,
      object,
      data_type,
      metadata,
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
      "metadata": {
        "object_data_type": self.metadata.object_data_type.to_string(),
      },
    })
  }
}
