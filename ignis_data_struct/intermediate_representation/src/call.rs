use enums::data_type::DataType;

use super::IRInstruction;

#[derive(Debug, Clone)]
pub struct IRCall {
  pub name: String,
  pub arguments: Vec<IRInstruction>,
  pub return_type: DataType,
}

impl IRCall {
  pub fn new(name: String, arguments: Vec<IRInstruction>, return_type: DataType) -> Self {
    Self {
      name,
      arguments,
      return_type,
    }
  }
}
