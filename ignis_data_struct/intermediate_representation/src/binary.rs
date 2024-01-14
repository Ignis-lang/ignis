use enums::data_type::DataType;

use super::{
  IRInstruction,
  instruction_type::IRInstructionType
};

#[derive(Debug, Clone)]
pub struct IRBinary {
  pub instruction_type: IRInstructionType,
  pub left: Box<IRInstruction>,
  pub right: Box<IRInstruction>,
  pub data_type: DataType,
}

impl IRBinary {
  pub fn new(
    instruction_type: IRInstructionType,
    left: Box<IRInstruction>,
    right: Box<IRInstruction>,
    data_type: DataType,
  ) -> Self {
    Self {
      instruction_type,
      left,
      right,
      data_type,
    }
  }
}
