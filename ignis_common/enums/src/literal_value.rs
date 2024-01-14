use crate::token_type::TokenType;

#[derive(Debug, Clone, PartialEq)]
pub enum LiteralValue {
  Int(i64),
  Float(f64),
  Char(char),
  String(String),
  Boolean(bool),
  Null,
}

impl LiteralValue {
  pub fn to_string(&self) -> String {
    match self {
      LiteralValue::Boolean(x) => x.to_string(),
      LiteralValue::Null => "null".to_string(),
      LiteralValue::Float(x) => x.to_string(),
      LiteralValue::Int(x) => x.to_string(),
      LiteralValue::String(x) => x.clone(),
      LiteralValue::Char(x) => x.to_string(),
    }
  }
  pub fn from_token_type(kind: TokenType, value: String) -> Self {
    match kind {
      TokenType::Int => Self::Int(value.parse().unwrap()),
      TokenType::Float => Self::Float(value.parse().unwrap()),
      TokenType::String => Self::String(value),
      TokenType::False | TokenType::True => Self::Boolean(value.parse().unwrap()),
      _ => Self::Null,
    }
  }
}
