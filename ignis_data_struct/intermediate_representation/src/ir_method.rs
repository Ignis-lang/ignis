use enums::data_type::DataType;
use token::token::Token;

use crate::{
  block::IRBlock, function::IRFunctionMetadata,
  IRInstructionTrait, IRInstruction,
};

#[derive(Debug, Clone)]
pub struct IRMethod {
  pub name: Token,
  pub parameters: Vec<IRInstruction>,
  pub return_type: DataType,
  pub body: Option<Box<IRBlock>>,
  pub metadata: IRFunctionMetadata,
  pub object: Token,
}

impl IRMethod {
  pub fn new(
    name: Token,
    parameters: Vec<IRInstruction>,
    return_type: DataType,
    body: Option<Box<IRBlock>>,
    metadata: IRFunctionMetadata,
    object: Token,
  ) -> Self {
    Self {
      name,
      parameters,
      return_type,
      body,
      metadata,
      object,
    }
  }
}

impl IRInstructionTrait for IRMethod {
  fn to_json(&self) -> serde_json::Value {
    serde_json::json!({
      "name": self.name.span.literal,
      "parameters": self.parameters.iter().map(|x| x.to_json()).collect::<Vec<serde_json::Value>>(),
      "return_type": self.return_type.to_string(),
      "body": self.body.as_ref().unwrap().to_json(),
      "metadata": self.metadata.to_json(),
      "object": self.object.span.literal,
    })
  }
}
