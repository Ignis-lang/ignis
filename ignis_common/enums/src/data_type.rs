use std::fmt::Display;

use super::token_type::TokenType;

#[derive(Debug, PartialEq, Clone)]
pub enum DataType {
  String,
  Int,
  Float,
  Boolean,
  Char,
  Null,
  None,
  Pending,
  Void,
  Variable(String),
  Array(Box<DataType>),
  Callable(Vec<DataType>, Box<DataType>),
  // TODO: Type non-primitive
  ClassType(String),
  GenericType {
    base: Box<DataType>,
    parameters: Vec<DataType>,
  },
  UnionType(Vec<DataType>),
  IntersectionType(Vec<DataType>),
  TupleType(Vec<DataType>),
  AliasType(String),
}

impl Display for DataType {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      DataType::String => write!(f, "String"),
      DataType::Int => write!(f, "Int"),
      DataType::Float => write!(f, "Float"),
      DataType::Boolean => write!(f, "Boolean"),
      DataType::Char => write!(f, "Char"),
      DataType::Null => write!(f, "Null"),
      DataType::None => write!(f, "None"),
      DataType::Pending => write!(f, "Pending"),
      DataType::Void => write!(f, "Void"),
      DataType::Variable(name) => write!(f, "{}", name),
      DataType::Array(types) => write!(f, "Array<{}>", types),
      DataType::Callable(params, ret) => {
        let params: Vec<String> = params.iter().map(|p| p.to_string()).collect();
        write!(f, "({}) -> {}", params.join(", "), ret)
      }
      DataType::ClassType(name) => write!(f, "{}", name),
      DataType::GenericType { base, parameters } => {
        let params: Vec<String> = parameters.iter().map(|p| p.to_string()).collect();
        write!(f, "{}<{}>", base, params.join(", "))
      }
      DataType::UnionType(types) => {
        let type_strings: Vec<String> = types.iter().map(|t| t.to_string()).collect();
        write!(f, "Union<{}>", type_strings.join(" | "))
      }
      DataType::IntersectionType(types) => {
        let type_strings: Vec<String> = types.iter().map(|t| t.to_string()).collect();
        write!(f, "Intersection<{}>", type_strings.join(" & "))
      }
      DataType::TupleType(types) => {
        let type_strings: Vec<String> = types.iter().map(|t| t.to_string()).collect();
        write!(f, "Tuple<{}>", type_strings.join(", "))
      }
      DataType::AliasType(alias) => write!(f, "{}", alias),
    }
  }
}

impl DataType {
  pub fn from_token_type(kind: TokenType) -> Self {
    match kind {
      TokenType::StringType => DataType::String,
      TokenType::FloatType => DataType::Float,
      TokenType::CharType => DataType::Char,
      TokenType::BooleanType => DataType::Boolean,
      TokenType::IntType => DataType::Int,
      TokenType::Void => DataType::Void,
      TokenType::Null => DataType::Null,
      _ => DataType::None,
    }
  }
}
