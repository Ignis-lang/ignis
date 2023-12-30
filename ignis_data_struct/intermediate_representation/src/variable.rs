use enums::data_type::DataType;

use super::{IRInstruction, IRInstructionTrait};

#[derive(Debug, Clone)]
pub struct IRVariableMetadata {
  // TODO: Implement inmutable parameters and reference
  pub is_mutable: bool,
  pub is_reference: bool,
  pub is_parameter: bool,
  pub is_function: bool,
  pub is_class: bool,
  pub is_declaration: bool,
  pub is_static: bool,
  pub is_public: bool,
  pub is_constructor: bool,
}

impl IRVariableMetadata {
  pub fn new(
    is_mutable: bool,
    is_reference: bool,
    is_parameter: bool,
    is_function: bool,
    is_class: bool,
    is_declaration: bool,
    is_static: bool,
    is_public: bool,
    is_constructor: bool,
  ) -> Self {
    Self {
      is_mutable,
      is_reference,
      is_parameter,
      is_function,
      is_class,
      is_declaration,
      is_static,
      is_public,
      is_constructor
    }
  }
}

#[derive(Debug, Clone)]
pub struct IRVariable {
  pub name: String,
  pub data_type: DataType,
  pub value: Option<Box<IRInstruction>>,
  pub metadata: IRVariableMetadata,
}

impl IRVariable {
  pub fn new(
    name: String,
    data_type: DataType,
    value: Option<Box<IRInstruction>>,
    metadata: IRVariableMetadata,
  ) -> Self {
    Self {
      name,
      data_type,
      value,
      metadata,
    }
  }
}

impl  IRInstructionTrait for IRVariable {
  fn to_json(&self) -> serde_json::Value {
    serde_json::json!({
      "name": self.name,
      "data_type": self.data_type.to_string(),
      "value": match &self.value {
        Some(value) => value.to_json(),
        None => serde_json::Value::Null,
      },
      "metadata": {
        "is_mutable": self.metadata.is_mutable,
        "is_reference": self.metadata.is_reference,
        "is_parameter": self.metadata.is_parameter,
        "is_function": self.metadata.is_function,
        "is_class": self.metadata.is_class,
        "is_declaration": self.metadata.is_declaration,
        "is_static": self.metadata.is_static,
        "is_public": self.metadata.is_public,
        "is_constructor": self.metadata.is_constructor,
      }
    })
  }
}
