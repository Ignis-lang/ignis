#[derive(Debug, Clone)]
pub enum HIRValue {
  Char(char),
  String(String),
  Int8(i8),
  Int16(i16),
  Int32(i32),
  Int64(i64),
  UnsignedInt8(u8),
  UnsignedInt16(u16),
  UnsignedInt32(u32),
  UnsignedInt64(u64),
  Float32(f32),
  Float64(f64),
  Boolean(bool),
  Return(Box<HIRValue>),
  // Function(Box<HIRFunction>),
  // Class(Box<HIRClassInstance>),
  // Reference(Box<AnalyzerValue>),
  Hex(String),
  Binary(String),
  Null,
  Unknown,
}
