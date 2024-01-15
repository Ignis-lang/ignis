use std::fmt::Display;

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

impl Display for IRInstructionType {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      IRInstructionType::Add => write!(f, "add"),
      IRInstructionType::Sub => write!(f, "sub"),
      IRInstructionType::Mul => write!(f, "mul"),
      IRInstructionType::Div => write!(f, "div"),
      IRInstructionType::GreaterEqual => write!(f, "greater_equal"),
      IRInstructionType::Greater => write!(f, "greater"),
      IRInstructionType::LessEqual => write!(f, "less_equal"),
      IRInstructionType::Less => write!(f, "less"),
      IRInstructionType::Equal => write!(f, "equal"),
      IRInstructionType::NotEqual => write!(f, "not_equal"),
      IRInstructionType::And => write!(f, "and"),
      IRInstructionType::Or => write!(f, "or"),
      IRInstructionType::Not => write!(f, "not"),
      IRInstructionType::Assign => write!(f, "assign"),
      IRInstructionType::AssignAdd => write!(f, "assign_add"),
      IRInstructionType::AssignSub => write!(f, "assign_sub"),
      IRInstructionType::Mod => write!(f, "mod"),
      IRInstructionType::Concatenate => write!(f, "concatenate"),
    }
  }
}

impl IRInstructionType {
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
