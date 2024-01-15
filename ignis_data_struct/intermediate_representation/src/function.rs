use enums::data_type::DataType;

use super::{variable::IRVariable, block::IRBlock};

#[derive(Debug, Clone)]
pub struct IRFunctionMetadata {
  pub is_recursive: bool,
  pub is_exported: bool,
  pub is_imported: bool,
  pub is_extern: bool,
}

impl IRFunctionMetadata {
  pub fn new(is_recursive: bool, is_exported: bool, is_imported: bool, is_extern: bool) -> Self {
    Self {
      is_recursive,
      is_exported,
      is_imported,
      is_extern,
    }
  }

  pub fn to_json(&self) -> serde_json::Value {
    serde_json::json!({
      "is_recursive": self.is_recursive,
      "is_exported": self.is_exported,
      "is_imported": self.is_imported,
      "is_extern": self.is_extern,
    })
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

  pub fn to_json(&self) -> serde_json::Value {
    serde_json::json!({
      "name": self.name,
      "parameters": self.parameters.iter().map(|x| x.to_json()).collect::<Vec<serde_json::Value>>(),
      "return_type": self.return_type.to_string(),
      "body": match &self.body {
        Some(body) => body.to_json(),
        None => serde_json::json!(null),
      },
      "metadata": self.metadata.to_json(),
    })
  }
}
