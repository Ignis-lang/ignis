use ignis_token::token_types::TokenType;
use ignis_type::span::Span;

use crate::NodeId;

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub enum UnaryOperator {
  Not,
  Negate,
  Increment,
  Decrement,
  BitNot,
}

impl From<&TokenType> for UnaryOperator {
  fn from(token_type: &TokenType) -> Self {
    match token_type {
      TokenType::Bang => UnaryOperator::Not,
      TokenType::Minus => UnaryOperator::Negate,
      TokenType::Increment => UnaryOperator::Increment,
      TokenType::Decrement => UnaryOperator::Decrement,
      TokenType::Tilde => UnaryOperator::BitNot,
      _ => panic!("Invalid token type for unary operator"),
    }
  }
}

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct ASTUnary {
  pub operator: UnaryOperator,
  pub operand: NodeId,
  pub span: Span,
}

impl ASTUnary {
  pub fn new(
    operator: UnaryOperator,
    operand: NodeId,
    span: Span,
  ) -> Self {
    ASTUnary {
      operator,
      operand,
      span,
    }
  }
}
