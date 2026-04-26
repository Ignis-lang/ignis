use crate::token_types::TokenType;
use ignis_type::{span::Span, value::IgnisLiteralValue};
use ordered_float::OrderedFloat;

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
  pub type_: TokenType,
  pub lexeme: String,
  pub span: Span,
}

impl Default for Token {
  fn default() -> Self {
    Self {
      type_: TokenType::Eof,
      lexeme: String::new(),
      span: Span::default(),
    }
  }
}

impl Token {
  pub fn new(
    type_: TokenType,
    lexeme: String,
    span: Span,
  ) -> Self {
    Self { type_, lexeme, span }
  }

  pub fn source_text<'a>(
    &self,
    source: &'a str,
  ) -> &'a str {
    let start = self.span.start.0 as usize;
    let end = self.span.end.0 as usize;

    source.get(start..end).unwrap_or("")
  }

  pub fn is_comment_trivia(&self) -> bool {
    self.type_.is_comment_trivia()
  }

  pub fn is_directive_keyword(&self) -> bool {
    self.type_ == TokenType::If
      || self.type_ == TokenType::Else
      || (self.type_ == TokenType::Identifier && matches!(self.lexeme.as_str(), "ifelse" | "configFlag"))
  }
}

impl std::fmt::Display for Token {
  fn fmt(
    &self,
    f: &mut std::fmt::Formatter<'_>,
  ) -> std::fmt::Result {
    write!(f, "(token type: {} lexeme: {} span: {})", self.type_, self.lexeme, self.span,)
  }
}

impl From<&Token> for IgnisLiteralValue {
  fn from(val: &Token) -> Self {
    match val.type_ {
      TokenType::Int => IgnisLiteralValue::Int32(val.lexeme.parse().unwrap_or(0)),
      TokenType::Float => IgnisLiteralValue::Float64(val.lexeme.parse().unwrap_or(OrderedFloat::default())),
      TokenType::Char => IgnisLiteralValue::Char(val.lexeme.parse().unwrap_or(0)),
      TokenType::String => IgnisLiteralValue::String(val.lexeme.clone()),
      TokenType::False | TokenType::True => IgnisLiteralValue::Boolean(val.lexeme.parse().unwrap_or(false)),
      TokenType::Hex => IgnisLiteralValue::Hex(val.lexeme.clone()),
      TokenType::Binary => IgnisLiteralValue::Binary(val.lexeme.clone()),
      _ => IgnisLiteralValue::Null,
    }
  }
}

#[cfg(test)]
mod tests {
  use super::Token;
  use crate::token_types::TokenType;
  use ignis_type::{BytePosition, file::SourceMap, span::Span};

  #[test]
  fn source_text_uses_span_boundaries() {
    let source = "@if(flag)";
    let mut source_map = SourceMap::new();
    let file_id = source_map.add_file("test.ign", source.to_string());
    let token = Token::new(
      TokenType::Identifier,
      "if".to_string(),
      Span {
        file: file_id,
        start: BytePosition(1),
        end: BytePosition(3),
      },
    );

    assert_eq!(token.source_text(source), "if");
  }

  #[test]
  fn comment_and_directive_helpers_match_formatter_needs() {
    let comment = Token::new(TokenType::DocComment, "/// docs".to_string(), Span::default());
    let directive_name = Token::new(TokenType::Identifier, "configFlag".to_string(), Span::default());
    let plain_identifier = Token::new(TokenType::Identifier, "value".to_string(), Span::default());

    assert!(comment.is_comment_trivia());
    assert!(directive_name.is_directive_keyword());
    assert!(!plain_identifier.is_directive_keyword());
  }
}
