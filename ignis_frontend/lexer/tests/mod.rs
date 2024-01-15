use lexer::Lexer;
use enums::token_type::TokenType;

#[test]
fn test_valid_indentifiers() {
  let source: &str = "let helloWorld: string = \"Hello World\";";
  let mut lexer: Lexer<'_> = Lexer::new(source, "".to_string());
  lexer.scan_tokens();

  assert_eq!(lexer.tokens.len(), 8);
  assert_eq!(lexer.tokens[0].kind, TokenType::Let);
  assert_eq!(lexer.tokens[1].kind, TokenType::Identifier);
  assert_eq!(lexer.tokens[2].kind, TokenType::Colon);
  assert_eq!(lexer.tokens[3].kind, TokenType::StringType);
  assert_eq!(lexer.tokens[4].kind, TokenType::Equal);

  assert_eq!(lexer.tokens[5].kind, TokenType::String);
  assert_eq!(lexer.tokens[5].span.literal, "Hello World".to_string());

  assert_eq!(lexer.tokens[6].kind, TokenType::SemiColon);
  assert_eq!(lexer.tokens[7].kind, TokenType::Eof);
}

#[test]
fn test_valid_expression() {
  let source: &str = "(3 + 5) * 12;";
  let mut lexer: Lexer<'_> = Lexer::new(source, "".to_string());
  lexer.scan_tokens();

  assert_eq!(lexer.tokens.len(), 9);
  assert_eq!(lexer.tokens[0].kind, TokenType::LeftParen);
  assert_eq!(lexer.tokens[1].kind, TokenType::Int);
  assert_eq!(lexer.tokens[1].span.literal, "3".to_string());

  assert_eq!(lexer.tokens[2].kind, TokenType::Plus);

  assert_eq!(lexer.tokens[3].kind, TokenType::Int);
  assert_eq!(lexer.tokens[3].span.literal, "5".to_string());

  assert_eq!(lexer.tokens[4].kind, TokenType::RightParen);
  assert_eq!(lexer.tokens[5].kind, TokenType::Asterisk);

  assert_eq!(lexer.tokens[6].kind, TokenType::Int);
  assert_eq!(lexer.tokens[6].span.literal, "12".to_string());

  assert_eq!(lexer.tokens[7].kind, TokenType::SemiColon);
  assert_eq!(lexer.tokens[8].kind, TokenType::Eof);
}

#[test]
fn test_valid_null() {
  let source: &str = "null";
  let mut lexer: Lexer<'_> = Lexer::new(source, "".to_string());
  lexer.scan_tokens();

  assert_eq!(lexer.tokens.len(), 2);
  assert_eq!(lexer.tokens[0].kind, TokenType::Null);
  assert_eq!(lexer.tokens[1].kind, TokenType::Eof);
}

#[test]
fn test_valid_key_boolean() {
  let source: &str = "false; true;";
  let mut lexer: Lexer<'_> = Lexer::new(source, "".to_string());
  lexer.scan_tokens();

  assert_eq!(lexer.tokens.len(), 5);
  assert_eq!(lexer.tokens[0].kind, TokenType::False);
  assert_eq!(lexer.tokens[1].kind, TokenType::SemiColon);
  assert_eq!(lexer.tokens[2].kind, TokenType::True);
  assert_eq!(lexer.tokens[3].kind, TokenType::SemiColon);
  assert_eq!(lexer.tokens[4].kind, TokenType::Eof);
}
