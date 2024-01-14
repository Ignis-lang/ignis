use enums::data_type::DataType;

use super::Expression;

#[derive(Debug, PartialEq, Clone)]
pub struct Ternary {
  pub condition: Box<Expression>,
  pub then_branch: Box<Expression>,
  pub else_branch: Box<Expression>,
  pub data_type: DataType,
}

impl Ternary {
  pub fn new(
    condition: Box<Expression>,
    then_branch: Box<Expression>,
    else_branch: Box<Expression>,
    data_type: DataType,
  ) -> Self {
    Self {
      condition,
      then_branch,
      else_branch,
      data_type,
    }
  }
}
