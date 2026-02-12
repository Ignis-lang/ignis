use ignis_type::{definition::DefinitionId, types::TypeId};

use crate::{LocalId, TempId};

/// An operand: a value that can be used in an instruction.
///
/// # IMPORTANT: Adding New Variants
///
/// When adding new variants to this enum, ensure ALL pattern matches in the codebase
/// are updated. Critical locations to check:
/// - `ignis_codegen_c::emit::format_operand` - formatting operands to C code
/// - `ignis_codegen_c::emit::operand_type` - extracting type information
/// - `ignis_lir::verify::check_operand` - validating operand usage
/// - `ignis_lir::verify::operand_type` - type checking
///
/// The compiler will catch exhaustive match violations, but non-exhaustive patterns
/// (using `_` wildcards) may silently fail. Search for `match.*Operand` to find all sites.
#[derive(Debug, Clone, PartialEq)]
pub enum Operand {
  /// A temporary value (result of a previous instruction).
  Temp(TempId),
  /// A local variable slot (for synthetic temps that need dropping).
  Local(LocalId),
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
  Atom(u32, TypeId),
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
      ConstValue::Atom(_, ty) => *ty,
      ConstValue::Null(ty) => *ty,
      ConstValue::Undef(ty) => *ty,
    }
  }
}
