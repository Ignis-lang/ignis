use std::fmt::Display;

use enums::{data_type::DataType, literal_value::LiteralValue};

use crate::function::IRFunction;

#[derive(Debug)]
pub enum AnalyzerValue {
  String(String),
  Int(i64),
  Float(f64),
  Boolean(bool),
  Return(Box<AnalyzerValue>),
  Function(Box<IRFunction>),
  Null,
  None,
}

impl Clone for AnalyzerValue {
  fn clone(&self) -> Self {
    match self {
      AnalyzerValue::String(s) => AnalyzerValue::String(s.clone()),
      AnalyzerValue::Int(i) => AnalyzerValue::Int(*i),
      AnalyzerValue::Float(d) => AnalyzerValue::Float(*d),
      AnalyzerValue::Boolean(b) => AnalyzerValue::Boolean(*b),
      AnalyzerValue::Null => AnalyzerValue::Null,
      AnalyzerValue::None => AnalyzerValue::None,
      AnalyzerValue::Return(r) => r.as_ref().clone(),
      AnalyzerValue::Function(f) => AnalyzerValue::Function(f.clone()),
    }
  }
}

impl Display for AnalyzerValue {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      AnalyzerValue::String(s) => write!(f, "{}", s),
      AnalyzerValue::Int(i) => write!(f, "{}", i),
      AnalyzerValue::Float(d) => write!(f, "{}", d),
      AnalyzerValue::Boolean(b) => write!(f, "{}", b),
      AnalyzerValue::Null => write!(f, "null"),
      AnalyzerValue::None => write!(f, "none"),
      AnalyzerValue::Return(r) => write!(f, "{}", r),
      AnalyzerValue::Function(_) => write!(f, "function"),
    }
  }
}

impl AnalyzerValue {
  pub fn to_data_type(&self) -> DataType {
    match self {
      AnalyzerValue::String(_) => DataType::String,
      AnalyzerValue::Int(_) => DataType::Int,
      AnalyzerValue::Float(_) => DataType::Float,
      AnalyzerValue::Boolean(_) => DataType::Boolean,
      AnalyzerValue::None | AnalyzerValue::Null => DataType::None,
      AnalyzerValue::Return(r) => r.to_data_type(),
      AnalyzerValue::Function(f) => f.return_type.clone(),
    }
  }

  pub fn from_literation_value(value: LiteralValue) -> Self {
    match value {
      LiteralValue::Int(i) => AnalyzerValue::Int(i),
      LiteralValue::Float(d) => AnalyzerValue::Float(d),
      LiteralValue::String(s) => AnalyzerValue::String(s),
      LiteralValue::Boolean(b) => AnalyzerValue::Boolean(b),
      LiteralValue::Null => AnalyzerValue::Null,
      LiteralValue::Char(_) => todo!(),
    }
  }
}
