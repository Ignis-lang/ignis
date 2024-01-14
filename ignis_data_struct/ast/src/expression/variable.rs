use {token::Token, enums::data_type::DataType};

#[derive(Debug, PartialEq, Clone)]
pub struct VariableExpression {
  pub name: Token,
  pub data_type: DataType,
}

impl VariableExpression {
  pub fn new(name: Token, data_type: DataType) -> Self {
    Self { name, data_type }
  }
}
