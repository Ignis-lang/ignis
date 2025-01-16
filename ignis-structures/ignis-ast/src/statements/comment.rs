use ignis_token::token::Token;
use serde::Serialize;

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum ASTCommentType {
  SingleLine,
  MultiLine,
  Documentation,
}

#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct ASTComment {
  pub comment: String,
  pub type_: ASTCommentType,
  pub token: Token,
}

impl ASTComment {
  pub fn new(
    token: Token,
    type_: ASTCommentType,
  ) -> Self {
    Self {
      comment: token.lexeme.clone(),
      type_,
      token,
    }
  }
}
