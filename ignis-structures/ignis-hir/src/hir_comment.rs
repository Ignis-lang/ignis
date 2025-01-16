use ignis_ast::statements::comment::ASTCommentType;
use ignis_token::token::Token;
use serde::Serialize;

#[derive(Debug, Clone, Serialize)]
pub enum HIRCommentType {
  SingleLine,
  MultiLine,
  Documentation,
}

impl Into<HIRCommentType> for &ASTCommentType {
  fn into(self) -> HIRCommentType {
    match self {
      ASTCommentType::Documentation => HIRCommentType::Documentation,
      ASTCommentType::SingleLine => HIRCommentType::SingleLine,
      ASTCommentType::MultiLine => HIRCommentType::MultiLine,
    }
  }
}

#[derive(Debug, Clone, Serialize)]
pub struct HIRComment {
  pub comment: String,
  pub type_: HIRCommentType,
  pub token: Token,
}

impl HIRComment {
  pub fn new(
    comment: String,
    type_: HIRCommentType,
    token: Token,
  ) -> Self {
    Self { comment, type_, token }
  }
}
