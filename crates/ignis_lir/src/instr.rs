use ignis_hir::operation::{BinaryOperation, UnaryOperation};
use ignis_type::{definition::DefinitionId, span::Span, types::TypeId};

use crate::{LocalId, Operand, TempId};

/// A single LIR instruction (TAC form: at most one operation, result in a temp).
#[derive(Debug, Clone, PartialEq)]
pub enum Instr {
  /// Load a value from a local slot into a temporary.
  /// `dest = *local`
  Load {
    dest: TempId,
    source: LocalId,
  },

  /// Store a value into a local slot.
  /// `*local = value`
  Store {
    dest: LocalId,
    value: Operand,
  },

  /// Load from a pointer/reference operand (indirect load).
  /// `dest = *ptr`
  LoadPtr {
    dest: TempId,
    ptr: Operand,
  },

  /// Store to a pointer/reference operand (indirect store).
  /// `*ptr = value`
  StorePtr {
    ptr: Operand,
    value: Operand,
  },

  /// Builtin load: `dest = *ptr` with explicit type.
  BuiltinLoad {
    dest: TempId,
    ptr: Operand,
    ty: TypeId,
  },

  /// Builtin store: `*ptr = value` with explicit type.
  BuiltinStore {
    ptr: Operand,
    value: Operand,
    ty: TypeId,
  },

  /// Copy/move a value to a new temporary.
  /// `dest = source`
  Copy {
    dest: TempId,
    source: Operand,
  },

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

  /// Bit-level reinterpret cast: `memcpy(&dest, &source, sizeof(target_type))`
  BitCast {
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

  Nop,

  /// Call to a runtime function by name (e.g., drop functions).
  RuntimeCall {
    name: String,
    args: Vec<Operand>,
  },

  /// Extract type_id from a runtime value: `dest = source.type_id`
  TypeIdOf {
    dest: TempId,
    source: Operand,
  },

  /// Get byte size of type: `dest = sizeof(ty)`
  SizeOf {
    dest: TempId,
    ty: TypeId,
  },

  /// Get alignment of type: `dest = alignof(ty)`
  AlignOf {
    dest: TempId,
    ty: TypeId,
  },

  /// Get maximum value of numeric type: `dest = maxOf<ty>()`
  MaxOf {
    dest: TempId,
    ty: TypeId,
  },

  /// Get minimum value of numeric type: `dest = minOf<ty>()`
  MinOf {
    dest: TempId,
    ty: TypeId,
  },

  /// Hardware trap (abnormal termination).
  Trap {
    span: Span,
  },

  /// Print a panic message to stderr and exit with code 101.
  PanicMessage {
    message: String,
    span: Span,
  },

  /// Drop an owned value's resources.
  /// The local must have a droppable type (string, dynamic vector, infer).
  Drop {
    local: LocalId,
  },

  /// Get pointer to a record field: `dest = &base.field`
  GetFieldPtr {
    dest: TempId,
    base: Operand,
    field_index: u32,
    field_type: TypeId,
  },

  /// Initialize a record in memory with field values.
  InitRecord {
    dest_ptr: Operand,
    fields: Vec<(u32, Operand)>,
    record_type: TypeId,
  },

  /// Create an enum variant value.
  InitEnumVariant {
    dest_ptr: Operand,
    enum_type: TypeId,
    variant_tag: u32,
    payload: Vec<Operand>,
  },

  /// Read enum tag from a value: `dest = source.tag`.
  EnumGetTag {
    dest: TempId,
    source: Operand,
  },

  /// Read enum payload field from a value.
  EnumGetPayloadField {
    dest: TempId,
    source: Operand,
    variant_tag: u32,
    field_index: u32,
  },

  /// Run T's drop on the value at `ptr`. No-op when T doesn't need dropping.
  DropInPlace {
    ptr: Operand,
    ty: TypeId,
  },

  /// `dest = &drop_glue_<T>` — function pointer `(*mut u8) -> void` that drops T.
  DropGlue {
    dest: TempId,
    ty: TypeId,
  },
}
