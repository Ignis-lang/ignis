use enums::data_type::DataType;

use super::IRInstruction;

#[derive(Debug, Clone)]
pub struct IRArray {
  pub elements: Vec<IRInstruction>,
  pub data_type: DataType,
}

impl IRArray {
  pub fn new(elements: Vec<IRInstruction>, data_type: DataType) -> Self {
    Self {
      elements,
      data_type,
    }
  }
}
