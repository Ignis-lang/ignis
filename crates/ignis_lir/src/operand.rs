use ignis_type::{definition::DefinitionId, types::TypeId};

use crate::TempId;

/// An operand: a value that can be used in an instruction.
#[derive(Debug, Clone, PartialEq)]
pub enum Operand {
  /// A temporary value (result of a previous instruction).
  Temp(TempId),
  /// A constant/literal value.
  Const(ConstValue),
  /// Reference to a function (for function pointers).
  FuncRef(DefinitionId),
  /// Reference to a global constant.
  GlobalRef(DefinitionId),
}

/// Compile-time constant values.
#[derive(Debug, Clone, PartialEq)]
pub enum ConstValue {
  Int(i64, TypeId),
  UInt(u64, TypeId),
  Float(ordered_float::OrderedFloat<f64>, TypeId),
  Bool(bool, TypeId),
  Char(char, TypeId),
  String(String, TypeId),
  Null(TypeId),
  /// Undefined/uninitialized.
  Undef(TypeId),
}

impl ConstValue {
  /// Returns the TypeId of this constant value.
  pub fn type_id(&self) -> TypeId {
    match self {
      ConstValue::Int(_, ty) => *ty,
      ConstValue::UInt(_, ty) => *ty,
      ConstValue::Float(_, ty) => *ty,
      ConstValue::Bool(_, ty) => *ty,
      ConstValue::Char(_, ty) => *ty,
      ConstValue::String(_, ty) => *ty,
      ConstValue::Null(ty) => *ty,
      ConstValue::Undef(ty) => *ty,
    }
  }
}
