use std::fmt::Display;

use ignis_data_type::DataType;
use ignis_token::{token::Token, token_types::TokenType};
use serde::{Deserialize, Serialize};

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub enum ASTLiteralValue {
  Int8(i8),
  Int16(i16),
  Int32(i32),
  Int64(i64),
  UnsignedInt8(u8),
  UnsignedInt16(u16),
  UnsignedInt32(u32),
  UnsignedInt64(u64),
  Float32(f32),
  Float64(f64),
  Boolean(bool),
  Char(char),
  String(String),
  Hex(String),
  Binary(String),
  Null,
}

impl Into<DataType> for ASTLiteralValue {
  fn into(self) -> DataType {
    match self {
      ASTLiteralValue::Int8(_) => DataType::Int8,
      ASTLiteralValue::Int16(_) => DataType::Int16,
      ASTLiteralValue::Int32(_) => DataType::Int32,
      ASTLiteralValue::Int64(_) => DataType::Int64,
      ASTLiteralValue::UnsignedInt8(_) => DataType::UnsignedInt8,
      ASTLiteralValue::UnsignedInt16(_) => DataType::UnsignedInt16,
      ASTLiteralValue::UnsignedInt32(_) => DataType::UnsignedInt32,
      ASTLiteralValue::UnsignedInt64(_) => DataType::UnsignedInt64,
      ASTLiteralValue::Float32(_) => DataType::Float32,
      ASTLiteralValue::Float64(_) => DataType::Float64,
      ASTLiteralValue::Boolean(_) => DataType::Boolean,
      ASTLiteralValue::Char(_) => DataType::Char,
      ASTLiteralValue::String(_) => DataType::String,
      ASTLiteralValue::Hex(_) => DataType::Hex,
      ASTLiteralValue::Binary(_) => DataType::Binary,
      ASTLiteralValue::Null => DataType::Null,
    }
  }
}

impl Display for ASTLiteralValue {
  fn fmt(
    &self,
    f: &mut std::fmt::Formatter<'_>,
  ) -> std::fmt::Result {
    match self {
      ASTLiteralValue::Int8(i) => write!(f, "{}", i),
      ASTLiteralValue::Int16(i) => write!(f, "{}", i),
      ASTLiteralValue::Int32(i) => write!(f, "{}", i),
      ASTLiteralValue::Int64(i) => write!(f, "{}", i),
      ASTLiteralValue::UnsignedInt8(i) => write!(f, "{}", i),
      ASTLiteralValue::UnsignedInt16(i) => write!(f, "{}", i),
      ASTLiteralValue::UnsignedInt32(i) => write!(f, "{}", i),
      ASTLiteralValue::UnsignedInt64(i) => write!(f, "{}", i),
      ASTLiteralValue::Float32(x) => write!(f, "{}", x),
      ASTLiteralValue::Float64(x) => write!(f, "{}", x),
      ASTLiteralValue::Char(x) => write!(f, "{}", x),
      ASTLiteralValue::String(x) => write!(f, "{}", x),
      ASTLiteralValue::Hex(x) => write!(f, "0x{}", x),
      ASTLiteralValue::Binary(x) => write!(f, "0b{}", x),
      ASTLiteralValue::Boolean(x) => write!(f, "{}", x),
      ASTLiteralValue::Null => write!(f, "null"),
    }
  }
}

impl From<(TokenType, String)> for ASTLiteralValue {
  fn from((kind, value): (TokenType, String)) -> Self {
    match kind {
      TokenType::Int => Self::Int64(value.parse().unwrap_or(0)),
      TokenType::Float => Self::Float64(value.parse().unwrap_or(0.0)),
      TokenType::Char => Self::Char(value.parse().unwrap_or('\0')),
      TokenType::String => Self::String(value),
      TokenType::False | TokenType::True => Self::Boolean(value.parse().unwrap_or(false)),
      TokenType::Hex => Self::Hex(value),
      TokenType::Binary => Self::Binary(value),
      _ => Self::Null,
    }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTLiteral {
  pub value: ASTLiteralValue,
  pub token: Token,
}

impl ASTLiteral {
  pub fn new(
    value: ASTLiteralValue,
    token: Token,
  ) -> Self {
    Self { value, token }
  }
}
