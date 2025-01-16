use ignis_data_type::DataType;
use serde::Serialize;

use crate::HIRInstruction;

#[derive(Debug, Clone, Serialize)]
pub struct HIRReturn {
  pub value: Box<HIRInstruction>,
  pub data_type: DataType,
}

impl HIRReturn {
  pub fn new(
    value: Box<HIRInstruction>,
    data_type: DataType,
  ) -> Self {
    Self { value, data_type }
  }
}
