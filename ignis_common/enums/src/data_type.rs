use std::fmt::Display;

use super::token_type::TokenType;

#[derive(Debug, PartialEq, Clone)]
pub struct GenericType {
  pub base: Box<DataType>,
  pub parameters: Vec<DataType>,
}

impl GenericType {
  pub fn new(
    base: Box<DataType>,
    parameters: Vec<DataType>,
  ) -> Self {
    Self { base, parameters }
  }
}

#[derive(Debug, PartialEq, Clone)]
pub enum DataType {
  String,
  Int,
  Float,
  Boolean,
  Char,
  Null,
  Unknown,
  Pending,
  Void,
  Variable(String),
  Array(Box<DataType>),
  Callable(Vec<DataType>, Box<DataType>),
  ClassType(String),
  Enum(String),
  PendingImport(String),
  Interface(String),
  // TODO: Type non-primitive
  GenericType(GenericType),
  UnionType(Vec<DataType>),
  IntersectionType(Vec<DataType>),
  TupleType(Vec<DataType>),
  AliasType(String),
}

impl Display for DataType {
  fn fmt(
    &self,
    f: &mut std::fmt::Formatter<'_>,
  ) -> std::fmt::Result {
    match self {
      DataType::String => write!(f, "String"),
      DataType::Int => write!(f, "Int"),
      DataType::Float => write!(f, "Float"),
      DataType::Boolean => write!(f, "Boolean"),
      DataType::Char => write!(f, "Char"),
      DataType::Null => write!(f, "Null"),
      DataType::Unknown => write!(f, "Unknown"),
      DataType::Pending => write!(f, "Pending"),
      DataType::Void => write!(f, "Void"),
      DataType::Variable(name) => write!(f, "{}", name),
      DataType::Array(types) => write!(f, "Array<{}>", types),
      DataType::Callable(params, ret) => {
        let params: Vec<String> = params.iter().map(|p| p.to_string()).collect();
        write!(f, "({}) -> {}", params.join(", "), ret)
      },
      DataType::ClassType(name) => write!(f, "{}", name),
      DataType::GenericType(generic) => {
        let params: Vec<String> = generic.parameters.iter().map(|p| p.to_string()).collect();

        if params.is_empty() {
          write!(f, "{}", generic.base)
        } else {
          write!(f, "{}<{}>", generic.base, params.join(", "))
        }
      },
      DataType::UnionType(types) => {
        let type_strings: Vec<String> = types.iter().map(|t| t.to_string()).collect();
        write!(f, "Union<{}>", type_strings.join(" | "))
      },
      DataType::IntersectionType(types) => {
        let type_strings: Vec<String> = types.iter().map(|t| t.to_string()).collect();
        write!(f, "Intersection<{}>", type_strings.join(" & "))
      },
      DataType::TupleType(types) => {
        let type_strings: Vec<String> = types.iter().map(|t| t.to_string()).collect();
        write!(f, "Tuple<{}>", type_strings.join(", "))
      },
      DataType::AliasType(alias) => write!(f, "{}", alias),
      DataType::Enum(name) => {
        write!(f, "Enum<{}>", name)
      },
      DataType::PendingImport(import) => {
        write!(f, "PendingImport<{}>", import)
      },
      DataType::Interface(i) => {
        write!(f, "Interface<{}>", i)
      },
    }
  }
}

impl DataType {
  pub fn from_token_type(kind: &TokenType) -> Self {
    match kind {
      TokenType::StringType => DataType::String,
      TokenType::FloatType => DataType::Float,
      TokenType::CharType => DataType::Char,
      TokenType::BooleanType => DataType::Boolean,
      TokenType::IntType => DataType::Int,
      TokenType::Void => DataType::Void,
      TokenType::Null => DataType::Null,
      TokenType::Unknown => DataType::Unknown,
      _ => DataType::Pending,
    }
  }
  pub fn to_c_type(
    &self,
    is_mutable: bool,
  ) -> String {
    let mut kind: String = if !is_mutable {
      String::from("const ")
    } else {
      "".to_string()
    };

    match self {
      DataType::Int | DataType::Boolean => kind.push_str("int"),
      DataType::Float => kind.push_str("float"),
      DataType::Char => kind.push_str("char"),
      DataType::String => kind.push_str("char*"),
      DataType::Void | DataType::Null | DataType::Unknown | DataType::Pending => kind.push_str("void"),
      DataType::Variable(_name) => todo!(),
      DataType::ClassType(_name) => todo!(),
      DataType::Array(array) => kind.push_str(array.to_c_type(true).to_string().as_str()),
      DataType::Callable(_, _) => todo!(),
      DataType::GenericType(_) => todo!(),
      DataType::UnionType(_) => todo!(),
      DataType::IntersectionType(_) => todo!(),
      DataType::TupleType(_) => todo!(),
      DataType::AliasType(_) => todo!(),
      DataType::Enum(_) => todo!(),
      DataType::PendingImport(_) => todo!(),
      DataType::Interface(_) => todo!(),
    };

    kind
  }
}
