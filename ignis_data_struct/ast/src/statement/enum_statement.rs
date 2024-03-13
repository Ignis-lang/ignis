use std::fmt::Display;

use token::token::Token;

#[derive(Debug, PartialEq, Clone)]
pub enum EnumMemberValue {
  Int(i64),
  String(String),
  None,
}

impl Display for EnumMemberValue {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      EnumMemberValue::Int(value) => write!(f, "{}", value),
      EnumMemberValue::String(value) => write!(f, "{}", value),
      EnumMemberValue::None => write!(f, "None"),
    }
  }
}

#[derive(Debug, PartialEq, Clone)]
pub struct EnumMember {
  pub name: Token,
  pub value: EnumMemberValue,
}

impl EnumMember {
  pub fn new(name: Token, value: EnumMemberValue) -> Self {
    Self { name, value }
  }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Enum {
  pub name: Token,
  pub members: Vec<EnumMember>,
  pub is_exported: bool,
  pub generic: Option<Vec<String>>,
}

impl Enum {
  pub fn new(
    name: Token,
    members: Vec<EnumMember>,
    is_exported: bool,
    generic: Option<Vec<String>>,
  ) -> Self {
    Self {
      name,
      members,
      is_exported,
      generic,
    }
  }
}
