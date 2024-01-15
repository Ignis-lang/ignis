use enums::token_type::TokenType;

use super::text_span::TextSpan;

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
  pub kind: TokenType,
  pub span: TextSpan,
}

impl Token {
  pub fn new(kind: TokenType, span: TextSpan) -> Self {
    Self { kind, span }
  }

  pub fn to_json(&self) -> serde_json::Value {
    serde_json::json!({
      "kind": self.kind.to_string(),
      "span": self.span.to_json(),
    })
  }

  pub fn vec_to_json(tokens: Vec<Token>) -> Vec<serde_json::Value> {
    tokens.iter().map(|token| token.to_json()).collect()
  }
}
