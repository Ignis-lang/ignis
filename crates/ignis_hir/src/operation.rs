#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BinaryOperation {
  // Arithmetic
  Add,
  Sub,
  Mul,
  Div,
  Mod,
  Pow,

  // Logical
  And,
  Or,

  // Comparison
  Equal,
  NotEqual,
  LessThan,
  LessEqual,
  GreaterThan,
  GreaterEqual,

  // Bitwise
  BitAnd,
  BitOr,
  BitXor,
  BitShiftLeft,
  BitShiftRight,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum UnaryOperation {
  Not,
  Neg,
  BitNot,
}
