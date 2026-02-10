#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum RecordAttr {
  Packed,
  Aligned(u64),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum FunctionAttr {
  ExternName(String),
  Cold,
  Deprecated(Option<String>),
  Extension { type_name: String, mutable: bool },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum FieldAttr {
  Aligned(u64),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum NamespaceAttr {
  /// Marks a namespace as a compiler-recognized language hook provider.
  ///
  /// Examples: `rc_runtime`, `string_runtime`, `vector_runtime`, `weak_runtime`.
  LangHook(String),
}
