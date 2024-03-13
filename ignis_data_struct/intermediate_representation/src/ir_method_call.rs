use enums::data_type::DataType;
use token::token::Token;

use crate::{IRInstruction, IRInstructionTrait};

#[derive(Debug, Clone)]
pub struct MethodCallMetadata {
  pub return_type: DataType,
  pub object_data_type: DataType,
}

impl MethodCallMetadata {
  pub fn new(
    return_type: DataType,
    object_data_type: DataType,
  ) -> Self {
    Self {
      return_type,
      object_data_type,
    }
  }
}

#[derive(Debug, Clone)]
pub struct IRMethodCall {
  pub name: Box<Token>,
  pub calle: Box<IRInstruction>,
  pub return_type: DataType,
  pub object: Box<IRInstruction>,
  pub metadata: MethodCallMetadata,
}

impl IRMethodCall {
  pub fn new(
    name: Box<Token>,
    calle: Box<IRInstruction>,
    return_type: DataType,
    object: Box<IRInstruction>,
    metadata: MethodCallMetadata,
  ) -> Self {
    Self {
      name,
      calle,
      return_type,
      object,
      metadata,
    }
  }
}

impl IRInstructionTrait for IRMethodCall {
  fn to_json(&self) -> serde_json::Value {
    serde_json::json!({
      "type": "IRMethodCall",
      "name": self.name.to_string(),
      "calle": self.calle.to_json(),
      "return_type": self.return_type.to_string(),
      "object": self.object.to_json(),
      "metadata": {
        "return_type": self.metadata.return_type.to_string(),
        "object_data_type": self.metadata.object_data_type.to_string(),
      }
    })
  }
}
