use enums::data_type::DataType;

use super::IRInstruction;

#[derive(Debug, Clone)]
pub struct IRReturn {
  pub value: Box<IRInstruction>,
  pub data_type: DataType,
}

impl IRReturn {
  pub fn new(value: Box<IRInstruction>, data_type: DataType) -> Self {
    Self { value, data_type }
  }
}
