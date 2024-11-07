use ignis_token::token::Token;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ASTCommentType {
  SingleLine,
  MultiLine,
  Documentation,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ASTComment {
  pub comment: String,
  pub type_: ASTCommentType,
}

impl ASTComment {
  pub fn new(token: Token, type_: ASTCommentType) -> Self {
    Self {
      comment: token.lexeme,
      type_,
    }
  }
}
