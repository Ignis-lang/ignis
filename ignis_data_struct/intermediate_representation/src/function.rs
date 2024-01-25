use enums::data_type::DataType;
use token::token::Token;

use crate::IRInstruction;

use super::{variable::IRVariable, block::IRBlock, IRInstructionTrait};

#[derive(Debug, Clone)]
pub struct IRFunctionMetadata {
  pub is_recursive: bool,
  pub is_exported: bool,
  pub is_imported: bool,
  pub is_extern: bool,
  pub is_static: bool,
  pub is_public: bool,
  pub is_constructor: bool,
  pub is_method: bool
}

impl IRFunctionMetadata {
  pub fn new(
    is_recursive: bool,
    is_exported: bool,
    is_imported: bool,
    is_extern: bool,
    is_static: bool,
    is_public: bool,
    is_constructor: bool,
    is_method: bool
  ) -> Self {
    Self {
      is_recursive,
      is_exported,
      is_imported,
      is_extern,
      is_public,
      is_static,
      is_constructor,
      is_method
    }
  }
}

impl IRInstructionTrait for IRFunctionMetadata {
  fn to_json(&self) -> serde_json::Value {
    serde_json::json!({
      "is_recursive": self.is_recursive,
      "is_exported": self.is_exported,
      "is_imported": self.is_imported,
      "is_extern": self.is_extern,
      "is_static": self.is_static,
      "is_public": self.is_public,
      "is_constructor": self.is_constructor,
      "is_method": self.is_method
    })
  }
}


#[derive(Debug, Clone)]
pub struct IRFunction {
  pub name: Token,
  pub parameters: Vec<IRInstruction>,
  pub return_type: DataType,
  pub body: Option<Box<IRBlock>>,
  pub metadata: IRFunctionMetadata,
}

impl IRFunction {
  pub fn new(
    name: Token,
    parameters: Vec<IRInstruction>,
    return_type: DataType,
    body: Option<Box<IRBlock>>,
    metadata: IRFunctionMetadata,
  ) -> Self {
    Self {
      name,
      parameters,
      return_type,
      body,
      metadata,
    }
  }
}

impl IRInstructionTrait for IRFunction {
  fn to_json(&self) -> serde_json::Value {
    serde_json::json!({
      "type": "function",
      "name": self.name.to_json(),
      "parameters": self.parameters.iter().map(|p| p.to_json()).collect::<Vec<serde_json::Value>>(),
      "return_type": self.return_type.to_string(),
      "body": if let Some(body) = &self.body {
        body.to_json()
      } else {
        serde_json::Value::Null
      },
      "metadata": self.metadata.to_json(),
    })
  }
}
