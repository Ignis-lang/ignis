use enums::data_type::DataType;
use token::token::Token;

use crate::{block::IRBlock, function::IRFunctionMetadata, IRInstruction, IRInstructionTrait};

#[derive(Debug, Clone)]
pub struct IRFunctionInstance {
  pub original_name: Box<Token>,
  pub instance_name: Box<Token>,
  pub instantiated_types: Vec<DataType>,
  pub parameters: Vec<IRInstruction>,
  pub return_type: DataType,
  pub body: Option<Box<IRBlock>>,
  pub metadata: IRFunctionMetadata,
}

impl IRFunctionInstance {
  pub fn new(
    original_name: Box<Token>,
    instance_name: Box<Token>,
    instantiated_types: Vec<DataType>,
    parameters: Vec<IRInstruction>,
    return_type: DataType,
    body: Option<Box<IRBlock>>,
    metadata: IRFunctionMetadata,
  ) -> Self {
    Self {
      original_name,
      instance_name,
      instantiated_types,
      parameters,
      return_type,
      body,
      metadata,
    }
  }
}

impl IRInstructionTrait for IRFunctionInstance {
  fn to_json(&self) -> serde_json::Value {
    serde_json::json!({
      "original_name": self.original_name.to_string(),
      "instance_name": self.instance_name.to_string(),
      "instantiated_types": self.instantiated_types.iter().map(|x| x.to_string()).collect::<Vec<String>>(),
      "parameters": self.parameters.iter().map(|x| x.to_json()).collect::<Vec<serde_json::Value>>(),
      "return_type": self.return_type.to_string(),
      "body": self.body.as_ref().map(|x| x.to_json()).unwrap_or(serde_json::Value::Null),
      "metadata": self.metadata.to_json()
    })
  }
}
