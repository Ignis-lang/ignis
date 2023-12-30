use enums::data_type::DataType;

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
  ) -> Self {
    Self {
      is_recursive,
      is_exported,
      is_imported,
      is_extern,
      is_public,
      is_static,
      is_constructor,
    }
  }
}

#[derive(Debug, Clone)]
pub struct IRFunction {
  pub name: String,
  pub parameters: Vec<IRVariable>,
  pub return_type: DataType,
  pub body: Option<Box<IRBlock>>,
  pub metadata: IRFunctionMetadata,
}

impl IRFunction {
  pub fn new(
    name: String,
    parameters: Vec<IRVariable>,
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
      "name": self.name,
      "parameters": self.parameters.iter().map(|p| p.to_json()).collect::<Vec<serde_json::Value>>(),
      "return_type": self.return_type.to_string(),
      "body": if let Some(body) = &self.body {
        body.to_json()
      } else {
        serde_json::Value::Null
      },
      "metadata": {
        "is_recursive": self.metadata.is_recursive,
        "is_exported": self.metadata.is_exported,
        "is_imported": self.metadata.is_imported,
        "is_extern": self.metadata.is_extern,
        "is_static": self.metadata.is_static,
        "is_public": self.metadata.is_public,
        "is_constructor": self.metadata.is_constructor,
      }
    })
  }
}
