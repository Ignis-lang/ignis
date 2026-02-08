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
  Extension(String),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum FieldAttr {
  Aligned(u64),
}
