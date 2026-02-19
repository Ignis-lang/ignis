use std::fmt::Display;
use ordered_float::OrderedFloat;

use crate::symbol::SymbolId;

fn escape_byte_as_char_literal(value: u8) -> String {
  match value {
    b'\n' => "\\n".to_string(),
    b'\r' => "\\r".to_string(),
    b'\t' => "\\t".to_string(),
    b'\0' => "\\0".to_string(),
    b'\\' => "\\\\".to_string(),
    b'\'' => "\\'".to_string(),
    b'"' => "\\\"".to_string(),
    b if b.is_ascii_graphic() || b == b' ' => (b as char).to_string(),
    _ => format!("\\x{:02x}", value),
  }
}

fn escape_special_characters(value: &str) -> String {
  value
    .replace("\\", "\\\\")
    .replace("\"", "\\\"")
    .replace("\n", "\\n")
    .replace("\r", "\\r")
    .replace("\t", "\\t")
    .replace("\0", "\\0")
}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum IgnisLiteralValue {
  Int8(i8),
  Int16(i16),
  Int32(i32),
  Int64(i64),
  UnsignedInt8(u8),
  UnsignedInt16(u16),
  UnsignedInt32(u32),
  UnsignedInt64(u64),
  Float32(OrderedFloat<f32>),
  Float64(OrderedFloat<f64>),
  Boolean(bool),
  Char(u8),
  String(String),
  Atom(SymbolId),
  Hex(String),
  Binary(String),
  Null,
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
      IgnisLiteralValue::Char(x) => write!(f, "'{}'", escape_byte_as_char_literal(*x)),
      IgnisLiteralValue::String(x) => write!(f, "\"{}\"", escape_special_characters(x)),
      IgnisLiteralValue::Atom(sym) => write!(f, ":{}", sym.index()),
      IgnisLiteralValue::Hex(x) => write!(f, "0x{}", x),
      IgnisLiteralValue::Binary(x) => write!(f, "0b{}", x),
      IgnisLiteralValue::Boolean(x) => write!(f, "{}", x),
      IgnisLiteralValue::Null => write!(f, "null"),
    }
  }
}
