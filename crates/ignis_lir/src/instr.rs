use ignis_hir::operation::{BinaryOperation, UnaryOperation};
use ignis_type::{definition::DefinitionId, types::TypeId};

use crate::{LocalId, Operand, TempId};

/// A single LIR instruction (TAC form: at most one operation, result in a temp).
#[derive(Debug, Clone, PartialEq)]
pub enum Instr {
  /// Load a value from a local slot into a temporary.
  /// `dest = *local`
  Load { dest: TempId, source: LocalId },

  /// Store a value into a local slot.
  /// `*local = value`
  Store { dest: LocalId, value: Operand },

  /// Load from a pointer/reference operand (indirect load).
  /// `dest = *ptr`
  LoadPtr { dest: TempId, ptr: Operand },

  /// Store to a pointer/reference operand (indirect store).
  /// `*ptr = value`
  StorePtr { ptr: Operand, value: Operand },

  /// Copy/move a value to a new temporary.
  /// `dest = source`
  Copy { dest: TempId, source: Operand },

  /// Binary operation: `dest = left op right`
  BinOp {
    dest: TempId,
    op: BinaryOperation,
    left: Operand,
    right: Operand,
  },

  /// Unary operation: `dest = op operand`
  UnaryOp {
    dest: TempId,
    op: UnaryOperation,
    operand: Operand,
  },

  /// Function call: `dest = callee(args...)`
  Call {
    dest: Option<TempId>,
    callee: DefinitionId,
    args: Vec<Operand>,
  },

  /// Type cast: `dest = source as target_type`
  Cast {
    dest: TempId,
    source: Operand,
    target_type: TypeId,
  },

  /// Get address of a local: `dest = &local`
  AddrOfLocal {
    dest: TempId,
    local: LocalId,
    mutable: bool,
  },

  /// Get element pointer (for array indexing).
  /// `dest = &base[index]`
  GetElementPtr {
    dest: TempId,
    base: Operand,
    index: Operand,
    element_type: TypeId,
  },

  /// Initialize a vector in memory.
  InitVector {
    dest_ptr: Operand,
    elements: Vec<Operand>,
    element_type: TypeId,
  },

  /// No-operation (placeholder).
  Nop,
}
