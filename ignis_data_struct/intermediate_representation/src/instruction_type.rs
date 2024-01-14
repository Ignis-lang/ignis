use enums::token_type::TokenType;

#[derive(Debug, Clone)]
pub enum IRInstructionType {
  Add,
  Sub,
  Mul,
  Div,
  GreaterEqual,
  Greater,
  LessEqual,
  Less,
  Equal,
  NotEqual,
  And,
  Or,
  Not,
  Assign,
  AssignAdd,
  AssignSub,
  Mod,
  Concatenate,
}

impl IRInstructionType {
  pub fn to_string(&self) -> String {
    match self {
      IRInstructionType::Add => String::from("add"),
      IRInstructionType::Sub => String::from("sub"),
      IRInstructionType::Mul => String::from("mul"),
      IRInstructionType::Div => String::from("div"),
      IRInstructionType::GreaterEqual => String::from("greater_equal"),
      IRInstructionType::Greater => String::from("greater"),
      IRInstructionType::LessEqual => String::from("less_equal"),
      IRInstructionType::Less => String::from("less"),
      IRInstructionType::Equal => String::from("equal"),
      IRInstructionType::NotEqual => String::from("not_equal"),
      IRInstructionType::And => String::from("and"),
      IRInstructionType::Or => String::from("or"),
      IRInstructionType::Not => String::from("not"),
      IRInstructionType::Assign => String::from("assign"),
      IRInstructionType::AssignAdd => String::from("assign_add"),
      IRInstructionType::AssignSub => String::from("assign_sub"),
      IRInstructionType::Mod => String::from("mod"),
      IRInstructionType::Concatenate => String::from("concatenate"),
    }
  }

  pub fn from_token_kind(kind: &TokenType) -> Self {
    match kind {
      TokenType::Plus => IRInstructionType::Add,
      TokenType::Minus => IRInstructionType::Sub,
      TokenType::Asterisk => IRInstructionType::Mul,
      TokenType::Slash => IRInstructionType::Div,
      TokenType::GreaterEqual => IRInstructionType::GreaterEqual,
      TokenType::Greater => IRInstructionType::Greater,
      TokenType::LessEqual => IRInstructionType::LessEqual,
      TokenType::Less => IRInstructionType::Less,
      TokenType::Mod => IRInstructionType::Mod,
      TokenType::EqualEqual => IRInstructionType::Equal,
      TokenType::BangEqual => IRInstructionType::NotEqual,
      TokenType::And => IRInstructionType::And,
      TokenType::Or => IRInstructionType::Or,
      TokenType::Bang => IRInstructionType::Not,
      TokenType::Equal => IRInstructionType::Assign,
      TokenType::Increment => IRInstructionType::AssignAdd,
      TokenType::Decrement => IRInstructionType::AssignSub,
      _ => panic!("Invalid token type"), // TODO:
    }
  }
}
