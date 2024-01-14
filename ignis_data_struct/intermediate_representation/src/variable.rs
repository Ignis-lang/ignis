use enums::data_type::DataType;

use super::IRInstruction;

#[derive(Debug, Clone)]
pub struct IRVariableMetadata {
  // TODO: Implement inmutable parameters and reference
  pub is_mutable: bool,
  pub is_reference: bool,
  pub is_parameter: bool,
  pub is_function: bool,
  pub is_class: bool,
  pub is_declaration: bool,
}

impl IRVariableMetadata {
  pub fn new(
    is_mutable: bool,
    is_reference: bool,
    is_parameter: bool,
    is_function: bool,
    is_class: bool,
    is_declaration: bool,
  ) -> Self {
    Self {
      is_mutable,
      is_reference,
      is_parameter,
      is_function,
      is_class,
      is_declaration,
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
