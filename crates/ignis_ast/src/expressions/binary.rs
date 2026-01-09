use ignis_token::token_types::TokenType;
use ignis_type::span::Span;

use crate::NodeId;

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum ASTBinaryOperator {
  Add,
  And,
  BitAnd,
  BitOr,
  BitXor,
  Divide,
  Equal,
  GreaterThan,
  GreaterThanOrEqual,
  LessThan,
  LessThanOrEqual,
  Modulo,
  Multiply,
  NotEqual,
  Or,
  ShiftLeft,
  ShiftRight,
  Subtract,
}

impl From<&TokenType> for ASTBinaryOperator {
  fn from(token_type: &TokenType) -> Self {
    match token_type {
      TokenType::Plus => ASTBinaryOperator::Add,
      TokenType::Minus => ASTBinaryOperator::Subtract,
      TokenType::Asterisk => ASTBinaryOperator::Multiply,
      TokenType::Slash => ASTBinaryOperator::Divide,
      TokenType::Mod => ASTBinaryOperator::Modulo,
      TokenType::EqualEqual => ASTBinaryOperator::Equal,
      TokenType::BangEqual => ASTBinaryOperator::NotEqual,
      TokenType::Greater => ASTBinaryOperator::GreaterThan,
      TokenType::GreaterEqual => ASTBinaryOperator::GreaterThanOrEqual,
      TokenType::Less => ASTBinaryOperator::LessThan,
      TokenType::LessEqual => ASTBinaryOperator::LessThanOrEqual,
      TokenType::Ampersand => ASTBinaryOperator::BitAnd,
      TokenType::Pipe => ASTBinaryOperator::BitOr,
      TokenType::Caret => ASTBinaryOperator::BitXor,
      TokenType::And => ASTBinaryOperator::And,
      TokenType::Or => ASTBinaryOperator::Or,
      TokenType::LeftShift => ASTBinaryOperator::ShiftLeft,
      TokenType::RightShift => ASTBinaryOperator::ShiftRight,
      _ => panic!("Unexpected token type for ASTBinaryOperator"),
    }
  }
}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub struct ASTBinary {
  pub left: NodeId,
  pub right: NodeId,
  pub operator: ASTBinaryOperator,
  pub span: Span,
}

impl ASTBinary {
  pub fn new(
    left: NodeId,
    operator: ASTBinaryOperator,
    right: NodeId,
    span: Span,
  ) -> Self {
    Self {
      left,
      right,
      operator,
      span,
    }
  }
}
