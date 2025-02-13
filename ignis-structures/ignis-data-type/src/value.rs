use std::fmt::Display;

use ignis_token::token_types::TokenType;
use serde::{Deserialize, Serialize};

use crate::DataType;

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub enum IgnisLiteralValue {
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

impl Into<DataType> for IgnisLiteralValue {
  fn into(self) -> DataType {
    match self {
      IgnisLiteralValue::Int8(_) => DataType::Int8,
      IgnisLiteralValue::Int16(_) => DataType::Int16,
      IgnisLiteralValue::Int32(_) => DataType::Int32,
      IgnisLiteralValue::Int64(_) => DataType::Int64,
      IgnisLiteralValue::UnsignedInt8(_) => DataType::UnsignedInt8,
      IgnisLiteralValue::UnsignedInt16(_) => DataType::UnsignedInt16,
      IgnisLiteralValue::UnsignedInt32(_) => DataType::UnsignedInt32,
      IgnisLiteralValue::UnsignedInt64(_) => DataType::UnsignedInt64,
      IgnisLiteralValue::Float32(_) => DataType::Float32,
      IgnisLiteralValue::Float64(_) => DataType::Float64,
      IgnisLiteralValue::Boolean(_) => DataType::Boolean,
      IgnisLiteralValue::Char(_) => DataType::Char,
      IgnisLiteralValue::String(_) => DataType::String,
      IgnisLiteralValue::Hex(_) => DataType::Hex,
      IgnisLiteralValue::Binary(_) => DataType::Binary,
      IgnisLiteralValue::Null => DataType::Null,
    }
  }
}

impl Display for IgnisLiteralValue {
  fn fmt(
    &self,
    f: &mut std::fmt::Formatter<'_>,
  ) -> std::fmt::Result {
    match self {
      IgnisLiteralValue::Int8(i) => write!(f, "{}", i),
      IgnisLiteralValue::Int16(i) => write!(f, "{}", i),
      IgnisLiteralValue::Int32(i) => write!(f, "{}", i),
      IgnisLiteralValue::Int64(i) => write!(f, "{}", i),
      IgnisLiteralValue::UnsignedInt8(i) => write!(f, "{}", i),
      IgnisLiteralValue::UnsignedInt16(i) => write!(f, "{}", i),
      IgnisLiteralValue::UnsignedInt32(i) => write!(f, "{}", i),
      IgnisLiteralValue::UnsignedInt64(i) => write!(f, "{}", i),
      IgnisLiteralValue::Float32(x) => write!(f, "{}", x),
      IgnisLiteralValue::Float64(x) => write!(f, "{}", x),
      IgnisLiteralValue::Char(x) => write!(f, "'{}'", x),
      IgnisLiteralValue::String(x) => write!(f, "\"{}\"", x),
      IgnisLiteralValue::Hex(x) => write!(f, "0x{}", x),
      IgnisLiteralValue::Binary(x) => write!(f, "0b{}", x),
      IgnisLiteralValue::Boolean(x) => write!(f, "{}", x),
      IgnisLiteralValue::Null => write!(f, "null"),
    }
  }
}

impl From<(TokenType, String)> for IgnisLiteralValue {
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
