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

impl TryFrom<&TokenType> for ASTBinaryOperator {
  type Error = ();

  fn try_from(token_type: &TokenType) -> Result<Self, Self::Error> {
    match token_type {
      TokenType::Plus => Ok(ASTBinaryOperator::Add),
      TokenType::Minus => Ok(ASTBinaryOperator::Subtract),
      TokenType::Asterisk => Ok(ASTBinaryOperator::Multiply),
      TokenType::Slash => Ok(ASTBinaryOperator::Divide),
      TokenType::Mod => Ok(ASTBinaryOperator::Modulo),
      TokenType::EqualEqual => Ok(ASTBinaryOperator::Equal),
      TokenType::BangEqual => Ok(ASTBinaryOperator::NotEqual),
      TokenType::Greater => Ok(ASTBinaryOperator::GreaterThan),
      TokenType::GreaterEqual => Ok(ASTBinaryOperator::GreaterThanOrEqual),
      TokenType::Less => Ok(ASTBinaryOperator::LessThan),
      TokenType::LessEqual => Ok(ASTBinaryOperator::LessThanOrEqual),
      TokenType::Ampersand => Ok(ASTBinaryOperator::BitAnd),
      TokenType::Pipe => Ok(ASTBinaryOperator::BitOr),
      TokenType::Caret => Ok(ASTBinaryOperator::BitXor),
      TokenType::And => Ok(ASTBinaryOperator::And),
      TokenType::Or => Ok(ASTBinaryOperator::Or),
      TokenType::LeftShift => Ok(ASTBinaryOperator::ShiftLeft),
      TokenType::RightShift => Ok(ASTBinaryOperator::ShiftRight),
      _ => Err(()),
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
