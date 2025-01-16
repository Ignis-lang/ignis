use ignis_data_type::DataType;
use serde::Serialize;

use crate::HIRInstruction;

#[derive(Debug, Clone, Serialize)]
pub struct HIRVector {
  pub elements: Vec<HIRInstruction>,
  pub data_type: DataType,
  pub length: Option<usize>,
}

impl HIRVector {
  pub fn new(
    elements: Vec<HIRInstruction>,
    data_type: DataType,
    length: Option<usize>,
  ) -> Self {
    Self {
      elements,
      data_type,
      length,
    }
  }
}
