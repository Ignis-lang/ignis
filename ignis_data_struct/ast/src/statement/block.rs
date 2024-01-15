use super::Statement;

#[derive(Debug, PartialEq, Clone)]
pub struct Block {
  pub statements: Vec<Statement>,
}

impl Block {
  pub fn new(statements: Vec<Statement>) -> Self {
    Self { statements }
  }
}
