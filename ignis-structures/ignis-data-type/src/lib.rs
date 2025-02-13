pub mod value;
use std::fmt::Display;

use ignis_token::{token::Token, token_types::TokenType};
use serde::{Deserialize, Serialize};

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub struct GenericType {
  pub base: Box<DataType>,
  pub constraints: Vec<DataType>,
}

impl GenericType {
  pub fn new(
    base: Box<DataType>,
    constraints: Vec<DataType>,
  ) -> Self {
    Self { base, constraints }
  }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum DataType {
  Hex,
  Binary,
  String,
  Int8,
  Int16,
  Int32,
  Int64,
  UnsignedInt8,
  UnsignedInt16,
  UnsignedInt32,
  UnsignedInt64,
  Float32,
  Float64,
  Boolean,
  Char,
  Null,
  Unknown,
  Pending,
  Void,
  Variable(String, Box<DataType>),
  Vector(Box<DataType>, Option<usize>),
  Callable(Vec<DataType>, Box<DataType>),
  Function(Vec<DataType>, Box<DataType>),
  PendingImport(String),
  Record(String, Vec<(String, DataType)>),
  Object(Vec<(String, DataType)>),
  Reference(Box<DataType>),
  Pointer(Box<DataType>),
  Optional(Box<DataType>),
  GenericType(GenericType),
  Enum(String),
  AliasType(String),
  UnionType(Vec<DataType>),
  IntersectionType(Vec<DataType>),
  // TODO: Ignis v0.3.0
  Interface(String),
  ClassType(String),
  TupleType(Vec<DataType>),
}

impl DataType {
  pub fn get_number_range(&self) -> String {
    match self {
      DataType::Int8 => format!("{} to {}", i8::MIN, i8::MAX),
      DataType::Int16 => format!("{} to {}", i16::MIN, i16::MAX),
      DataType::Int32 => format!("{} to {}", i32::MIN, i32::MAX),
      DataType::Int64 => format!("{} to {}", i64::MIN, i64::MAX),
      DataType::UnsignedInt8 => format!("0 to {}", u8::MAX),
      DataType::UnsignedInt16 => format!("0 to {}", u16::MAX),
      DataType::UnsignedInt32 => format!("0 to {}", u32::MAX),
      DataType::UnsignedInt64 => format!("0 to {}", u64::MAX),
      _ => String::new(),
    }
  }
}

impl From<&TokenType> for DataType {
  fn from(kind: &TokenType) -> Self {
    match kind {
      TokenType::StringType => DataType::String,
      TokenType::Int8Type => DataType::Int8,
      TokenType::Int16Type => DataType::Int16,
      TokenType::Int32Type => DataType::Int32,
      TokenType::Int64Type => DataType::Int64,
      TokenType::UnsignedInt8Type => DataType::UnsignedInt8,
      TokenType::UnsignedInt16Type => DataType::UnsignedInt16,
      TokenType::UnsignedInt32Type => DataType::UnsignedInt32,
      TokenType::UnsignedInt64Type => DataType::UnsignedInt64,
      TokenType::Float32Type => DataType::Float32,
      TokenType::Float64Type => DataType::Float64,
      TokenType::CharType => DataType::Char,
      TokenType::BooleanType => DataType::Boolean,
      TokenType::Void => DataType::Void,
      TokenType::Null => DataType::Null,
      TokenType::Unknown => DataType::Unknown,
      TokenType::HexType => DataType::Hex,
      TokenType::BinaryType => DataType::Binary,
      _ => DataType::Pending,
    }
  }
}

impl Display for DataType {
  fn fmt(
    &self,
    f: &mut std::fmt::Formatter<'_>,
  ) -> std::fmt::Result {
    match self {
      DataType::Hex => write!(f, "hex"),
      DataType::Binary => write!(f, "binary"),
      DataType::String => write!(f, "string"),
      DataType::Int8 => write!(f, "i8"),
      DataType::Int16 => write!(f, "i16"),
      DataType::Int32 => write!(f, "i32"),
      DataType::Int64 => write!(f, "i64"),
      DataType::UnsignedInt8 => write!(f, "u8"),
      DataType::UnsignedInt16 => write!(f, "u16"),
      DataType::UnsignedInt32 => write!(f, "u32"),
      DataType::UnsignedInt64 => write!(f, "u64"),
      DataType::Float32 => write!(f, "f32"),
      DataType::Float64 => write!(f, "f64"),
      DataType::Boolean => write!(f, "boolean"),
      DataType::Char => write!(f, "char"),
      DataType::Null => write!(f, "null"),
      DataType::Unknown => write!(f, "unknown"),
      DataType::Void => write!(f, "void"),
      DataType::Pending => write!(f, "pending"),
      DataType::PendingImport(name) => write!(f, "pending_import({})", name),
      DataType::Record(name, items) => write!(
        f,
        "{}<{{{}}}>",
        name,
        items
          .iter()
          .map(|(name, data_type)| format!("{}: {}", name, data_type))
          .collect::<Vec<String>>()
          .join(", ")
      ),
      DataType::Object(object) => write!(
        f,
        "{{{}}}",
        object
          .iter()
          .map(|(name, data_type)| format!("{}: {}", name, data_type))
          .collect::<Vec<String>>()
          .join(", ")
      ),
      DataType::Reference(data_type) => write!(f, "&{}", data_type),
      DataType::Pointer(data_type) => write!(f, "*{}", data_type),
      DataType::Variable(name, data_type) => write!(f, "variable({}, {})", name, data_type),
      DataType::Vector(data_type, size) => {
        write!(
          f,
          "{}[{}]",
          data_type,
          size
            .as_ref()
            .map(|s| -> String { s.clone().to_string() })
            .unwrap_or(String::new())
        )
      },
      DataType::Callable(parameters, return_type) => write!(
        f,
        "callable<{}, {}>",
        parameters
          .iter()
          .map(|p| p.to_string())
          .collect::<Vec<String>>()
          .join(", "),
        return_type
      ),
      DataType::Function(parameters, return_type) => write!(
        f,
        "({}) -> {}",
        parameters
          .iter()
          .map(|p| p.to_string())
          .collect::<Vec<String>>()
          .join(", "),
        return_type
      ),
      DataType::Optional(data_type) => write!(f, "optional({})", data_type),
      DataType::GenericType(generic_type) => write!(
        f,
        "generic<{}, {}>",
        generic_type.base,
        generic_type
          .constraints
          .iter()
          .map(|c| c.to_string())
          .collect::<Vec<String>>()
          .join(", ")
      ),
      DataType::Enum(name) => write!(f, "enum({})", name),
      DataType::AliasType(name) => write!(f, "alias({})", name),
      DataType::UnionType(types) => write!(
        f,
        "union<{}>",
        types.iter().map(|t| t.to_string()).collect::<Vec<String>>().join(", ")
      ),
      DataType::IntersectionType(types) => write!(
        f,
        "intersection<{}>",
        types.iter().map(|t| t.to_string()).collect::<Vec<String>>().join(", ")
      ),
      DataType::Interface(name) => write!(f, "interface({})", name),
      DataType::ClassType(name) => write!(f, "class({})", name),
      DataType::TupleType(types) => write!(
        f,
        "tuple<{}>",
        types.iter().map(|t| t.to_string()).collect::<Vec<String>>().join(", ")
      ),
    }
  }
}
