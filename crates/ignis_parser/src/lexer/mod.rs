use ignis_diagnostics::{message::DiagnosticMessage};
use ignis_token::{token::Token, token_types::TokenType};
use colored::*;
use ignis_config::IgnisConfig;
use std::sync::Arc;

type LexerResult = Result<TokenType, Box<DiagnosticMessage>>;

pub struct IgnisLexer<'a> {
  config: Arc<IgnisConfig>,
  source: &'a str,
  chars: std::str::Chars<'a>,
  pub tokens: Vec<Token>,
  start: usize,
  line: usize,
  current: usize,
  module_path: String,
  pub diagnostics: Vec<DiagnosticMessage>,
}

impl<'a> IgnisLexer<'a> {
  pub fn new(
    config: Arc<IgnisConfig>,
    source: &'a str,
    file: &'a str,
  ) -> Self {
    Self {
      config,
      chars: source.chars(),
      source,
      tokens: vec![],
      start: 0,
      line: 0,
      current: 0,
      module_path: file.to_string(),
      diagnostics: vec![],
    }
  }

  pub fn scan_tokens(
    &mut self,
    std: bool,
  ) {
    if std && !self.config.quiet {
      println!(
        "{:indent$}{} Scanning... {}",
        " ",
        "-->".bright_yellow().bold(),
        self.module_path,
        indent = 6
      )
    } else if !self.config.quiet {
      println!(
        "{:indent$}Scanning... {}",
        "-->".bright_green().bold(),
        self.module_path,
        indent = 4
      );
    }

    loop {
      self.start = self.current;

      match self.scan_token() {
        Ok(TokenType::Whitespace) => {},
        Ok(token_type) => self.add_token(token_type),
        Err(err) => {
          self.diagnostics.push(*err);
          continue;
        },
      }

      if self.is_at_end() {
        break;
      }
    }

    self.tokens.push(Token::new(
      TokenType::Eof,
      String::new(),
      self.line,
      self.start,
      0,
      self.module_path.clone(),
    ));
  }

  fn is_at_end(&self) -> bool {
    self.chars.as_str().is_empty()
  }

  fn scan_token(&mut self) -> LexerResult {
    let c: char = self.advance();

    match c {
      '(' => Ok(TokenType::LeftParen),
      ')' => Ok(TokenType::RightParen),
      '{' => Ok(TokenType::LeftBrace),
      '}' => Ok(TokenType::RightBrace),
      '[' => Ok(TokenType::LeftBrack),
      ']' => Ok(TokenType::RightBrack),
      ',' => Ok(TokenType::Comma),
      '.' if self.match_char('.') => {
        if self.match_char('.') {
          Ok(TokenType::Variadic)
        } else if self.match_char('=') {
          Ok(TokenType::RangeInclusive)
        } else {
          Ok(TokenType::Range)
        }
      },
      '.' => Ok(TokenType::Dot),
      ';' => Ok(TokenType::SemiColon),
      '@' => Ok(TokenType::At),
      '-' if self.match_char('=') => Ok(TokenType::SubtractAssign),
      '-' if self.match_char('-') => Ok(TokenType::Decrement),
      '-' if self.match_char('>') => Ok(TokenType::Arrow),
      '-' if self.peek().is_ascii_digit() => self.number(),
      '-' => Ok(TokenType::Minus),
      '+' if self.match_char('=') => Ok(TokenType::AddAssign),
      '+' if self.match_char('+') => Ok(TokenType::Increment),
      '+' if self.peek().is_ascii_digit() => self.number(),
      '+' => Ok(TokenType::Plus),
      '*' => Ok(TokenType::Asterisk),
      ':' if self.match_char(':') => Ok(TokenType::DoubleColon),
      ':' => Ok(TokenType::Colon),
      '%' => Ok(TokenType::Mod),
      '!' if self.match_char('=') => Ok(TokenType::BangEqual),
      '!' => Ok(TokenType::Bang),
      '=' if self.match_char('=') => Ok(TokenType::EqualEqual),
      '=' => Ok(TokenType::Equal),
      '<' if self.match_char('=') => Ok(TokenType::LessEqual),
      '<' => Ok(TokenType::Less),
      '>' if self.match_char('=') => Ok(TokenType::GreaterEqual),
      '>' => Ok(TokenType::Greater),
      '|' if self.match_char('|') => Ok(TokenType::Or),
      '|' => Ok(TokenType::Pipe),
      '&' if self.match_char('&') => Ok(TokenType::And),
      '&' => Ok(TokenType::Ampersand),
      '#' => Ok(TokenType::Hash),
      '?' => Ok(TokenType::QuestionMark),
      '/' if self.peek() == '/' || self.peek() == '*' => self.comments(),
      '/' => Ok(TokenType::Slash),
      '"' => self.string(),
      '0' if self.peek() == 'x' || self.peek() == 'X' => self.hex_number(),
      '0' if self.peek() == 'b' || self.peek() == 'B' => self.binary_number(),
      '0' if self.peek().is_ascii_digit() => self.number(),
      c if c.is_ascii_digit() => self.number(),
      ' ' | '\r' | '\t' => Ok(TokenType::Whitespace),
      '\n' => {
        self.line += 1;
        Ok(TokenType::Whitespace)
      },
      n if n.is_ascii_alphabetic() || n == '_' => self.identifier(),
      _ => Err(Box::new(DiagnosticMessage::InvalidToken(Token::new(
        TokenType::Bad,
        c.to_string(),
        self.line,
        self.start,
        self.current - self.start,
        self.module_path.clone(),
      )))),
    }
  }

  fn peek(&self) -> char {
    self.chars.clone().next().unwrap_or('\0')
  }

  fn peek_next(&self) -> char {
    self.source.chars().nth(self.current + 1).unwrap_or('\0')
  }

  fn peek_prev(&self) -> char {
    self.source.chars().nth(self.current - 1).unwrap_or('\0')
  }

  fn match_char(
    &mut self,
    expected: char,
  ) -> bool {
    let mut lookahead = self.chars.clone();

    if let Some(next_char) = lookahead.next() {
      if next_char == expected {
        self.chars = lookahead;
        self.current += next_char.len_utf8();
        return true;
      }
    }

    false
  }

  fn advance(&mut self) -> char {
    let next_char = self.chars.next();

    if let Some(c) = next_char {
      self.current += c.len_utf8();
    }

    next_char.unwrap_or('\0')
  }

  fn string(&mut self) -> LexerResult {
    let mut result: String = String::new();

    while self.peek() != '\"' && !self.is_at_end() {
      if self.peek() == '\\' {
        self.advance();

        match self.peek() {
          '\"' => result.push('\"'),
          '\\' => result.push('\\'),
          'n' => result.push('\n'),
          'r' => result.push('\r'),
          't' => result.push('\t'),
          '0' => result.push('\0'),
          _ => {},
        }
      } else {
        result.push(self.peek());
      }

      self.advance();
    }

    if self.is_at_end() {
      return Err(Box::new(DiagnosticMessage::UnterminatedString(Token::new(
        TokenType::String,
        result.clone(),
        self.line + 1,
        self.start,
        self.current - self.start,
        self.module_path.clone(),
      ))));
    }

    self.advance();

    Ok(TokenType::String)
  }

  fn number(&mut self) -> LexerResult {
    let mut is_float: bool = false;
    while self.peek().is_ascii_digit() || self.peek() == '_' {
      if self.peek() == '_' && (!self.peek_next().is_ascii_digit() || !self.peek_prev().is_ascii_digit()) {
        let literal = self.source[self.start..self.current].to_string();
        return Err(Box::new(DiagnosticMessage::ExpectedToken(
          TokenType::Int,
          Token::new(
            TokenType::Bad,
            literal,
            self.line + 1,
            self.start,
            self.current - self.start,
            self.module_path.clone(),
          ),
        )));
      }

      self.advance();
    }

    if self.peek() == '.' && self.peek_next().is_ascii_digit() {
      self.advance();

      while self.peek().is_ascii_digit() || self.peek() == '_' {
        if self.peek() == '_' && (!self.peek_next().is_ascii_digit() || !self.peek_prev().is_ascii_digit()) {
          let literal = self.source[self.start..self.current].to_string();
          return Err(Box::new(DiagnosticMessage::ExpectedToken(
            TokenType::Float,
            Token::new(
              TokenType::Bad,
              literal,
              self.line + 1,
              self.start,
              self.current - self.start,
              self.module_path.clone(),
            ),
          )));
        }

        self.advance();
      }

      is_float = true;
    }

    if is_float {
      Ok(TokenType::Float)
    } else {
      Ok(TokenType::Int)
    }
  }

  fn binary_number(&mut self) -> LexerResult {
    self.advance();
    let mut saw_digit = false;

    while self.peek() == '0' || self.peek() == '1' {
      saw_digit = true;
      self.advance();
    }

    if !saw_digit || self.peek().is_ascii_alphanumeric() || self.peek() == '_' {
      let literal = self.source[self.start..self.current].to_string();
      return Err(Box::new(DiagnosticMessage::ExpectedBinary(Token::new(
        TokenType::Bad,
        literal,
        self.line + 1,
        self.start,
        self.current - self.start,
        self.module_path.clone(),
      ))));
    }

    Ok(TokenType::Binary)
  }

  fn hex_number(&mut self) -> LexerResult {
    self.advance();
    let mut saw_digit = false;

    while self.peek().is_ascii_hexdigit() {
      saw_digit = true;
      self.advance();
    }

    if !saw_digit || self.peek().is_ascii_alphanumeric() || self.peek() == '_' {
      let literal = self.source[self.start..self.current].to_string();
      return Err(Box::new(DiagnosticMessage::ExpectedHex(Token::new(
        TokenType::Bad,
        literal,
        self.line + 1,
        self.start,
        self.current - self.start,
        self.module_path.clone(),
      ))));
    }

    Ok(TokenType::Hex)
  }

  fn comments(&mut self) -> LexerResult {
    if self.match_char('/') {
      while self.peek() != '\n' && !self.is_at_end() {
        self.advance();
      }

      return Ok(TokenType::Comment);
    } else if self.match_char('*') {
      let mut is_doc_comment = false;

      if self.peek() == '*' {
        is_doc_comment = true;
      }

      let mut lexeme: String = String::from("/*");
      lexeme.push_str(if is_doc_comment { "*" } else { "" });
      self.advance();

      while !self.is_at_end() && !(self.peek() == '*' && self.peek_next() == '/') {
        lexeme.push(self.peek());
        if self.peek() == '\n' {
          self.line += 1;
        }
        self.advance();
      }
      lexeme.push_str("*/");
      self.advance();
      self.advance();

      return Ok(if is_doc_comment {
        TokenType::DocComment
      } else {
        TokenType::MultiLineComment
      });
    }

    Err(Box::new(DiagnosticMessage::UntermintedComment(Token::new(
      TokenType::Comment,
      self.source[self.start..self.current].to_string(),
      self.line,
      self.start,
      self.current - self.start,
      self.module_path.clone(),
    ))))
  }

  fn is_identifier_letter(&self) -> bool {
    let c: char = self.peek();

    c.is_ascii_digit() || c.is_ascii_lowercase() || c.is_ascii_uppercase() || c == '_'
  }

  fn identifier(&mut self) -> LexerResult {
    while self.is_identifier_letter() {
      self.advance();
    }

    let value: String = self.source[self.start..self.current].to_string();
    let kind: Option<TokenType> = TokenType::get_keyword_from_string(value.as_str());

    Ok(kind.unwrap_or(TokenType::Identifier))
  }

  fn add_token(
    &mut self,
    kind: TokenType,
  ) {
    let mut literal = self.source[self.start..self.current].to_string();

    if (kind == TokenType::Int || kind == TokenType::Float) && literal.contains('_') {
      literal = literal.replace('_', "");
    }

    self.tokens.push(Token::new(
      kind.clone(),
      literal,
      self.line + 1,
      self.start,
      self.current - self.start,
      self.module_path.clone(),
    ));
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use ignis_config::IgnisConfig;
  use ignis_diagnostics::message::DiagnosticMessage;
  use ignis_token::{token::Token, token_types::TokenType};

  struct LexResult {
    tokens: Vec<Token>,
    diagnostics: Vec<DiagnosticMessage>,
  }

  fn lex(source: &str) -> LexResult {
    let mut config = IgnisConfig::default();
    config.quiet = true;

    let mut lexer = IgnisLexer::new(Arc::new(config), source, "test.ign");
    lexer.scan_tokens(false);

    LexResult {
      tokens: lexer.tokens.clone(),
      diagnostics: lexer.diagnostics.clone(),
    }
  }

  fn assert_tokens(
    tokens: &[Token],
    expected: &[(TokenType, &str)],
  ) {
    assert_eq!(
      tokens.len(),
      expected.len(),
      "expected {} tokens but lexer produced {}: {:?}",
      expected.len(),
      tokens.len(),
      tokens
        .iter()
        .map(|token| (&token.type_, token.lexeme.as_str()))
        .collect::<Vec<_>>()
    );

    for (index, (token, (expected_type, expected_lexeme))) in tokens.iter().zip(expected.iter()).enumerate() {
      assert_eq!(
        &token.type_, expected_type,
        "token type mismatch at index {} (lexeme '{}')",
        index, token.lexeme
      );
      assert_eq!(
        token.lexeme.as_str(),
        *expected_lexeme,
        "token lexeme mismatch at index {} (expected {:?})",
        index,
        expected_lexeme
      );
    }
  }

  #[test]
  fn lexes_basic_symbols_and_operators() {
    let LexResult { tokens, diagnostics } =
      lex("() {} [] , . .. ... ..= ; @ + ++ - -- * / % ! != = == < <= > >= | || & && # ? : :: ->");

    assert!(diagnostics.is_empty(), "unexpected diagnostics: {:?}", diagnostics);

    assert_tokens(
      &tokens,
      &[
        (TokenType::LeftParen, "("),
        (TokenType::RightParen, ")"),
        (TokenType::LeftBrace, "{"),
        (TokenType::RightBrace, "}"),
        (TokenType::LeftBrack, "["),
        (TokenType::RightBrack, "]"),
        (TokenType::Comma, ","),
        (TokenType::Dot, "."),
        (TokenType::Range, ".."),
        (TokenType::Variadic, "..."),
        (TokenType::RangeInclusive, "..="),
        (TokenType::SemiColon, ";"),
        (TokenType::At, "@"),
        (TokenType::Plus, "+"),
        (TokenType::Increment, "++"),
        (TokenType::Minus, "-"),
        (TokenType::Decrement, "--"),
        (TokenType::Asterisk, "*"),
        (TokenType::Slash, "/"),
        (TokenType::Mod, "%"),
        (TokenType::Bang, "!"),
        (TokenType::BangEqual, "!="),
        (TokenType::Equal, "="),
        (TokenType::EqualEqual, "=="),
        (TokenType::Less, "<"),
        (TokenType::LessEqual, "<="),
        (TokenType::Greater, ">"),
        (TokenType::GreaterEqual, ">="),
        (TokenType::Pipe, "|"),
        (TokenType::Or, "||"),
        (TokenType::Ampersand, "&"),
        (TokenType::And, "&&"),
        (TokenType::Hash, "#"),
        (TokenType::QuestionMark, "?"),
        (TokenType::Colon, ":"),
        (TokenType::DoubleColon, "::"),
        (TokenType::Arrow, "->"),
        (TokenType::Eof, ""),
      ],
    );
  }

  #[test]
  fn lexes_integer_and_float_literals() {
    let LexResult { tokens, diagnostics } = lex("-42 3.14 1_000 2.5_0 0.5");

    assert!(diagnostics.is_empty(), "unexpected diagnostics: {:?}", diagnostics);

    assert_tokens(
      &tokens,
      &[
        (TokenType::Int, "-42"),
        (TokenType::Float, "3.14"),
        (TokenType::Int, "1000"),
        (TokenType::Float, "2.50"),
        (TokenType::Float, "0.5"),
        (TokenType::Eof, ""),
      ],
    );
  }

  #[test]
  fn reports_invalid_numeric_separator_usage() {
    let LexResult { tokens, diagnostics } = lex("1_");

    assert_eq!(diagnostics.len(), 1, "expected a single diagnostic for invalid separator usage");
    match &diagnostics[0] {
      DiagnosticMessage::ExpectedToken(expected_type, token) => {
        assert_eq!(*expected_type, TokenType::Int);
        assert_eq!(token.lexeme, "1");
      },
      other => panic!("unexpected diagnostic: {:?}", other),
    }

    assert_tokens(&tokens, &[(TokenType::Identifier, "_"), (TokenType::Eof, "")]);
  }

  #[test]
  fn lexes_binary_literals_and_reports_missing_digits() {
    let LexResult { tokens, diagnostics } = lex("0b1010; 0B11;");

    assert!(diagnostics.is_empty(), "unexpected diagnostics: {:?}", diagnostics);

    assert_tokens(
      &tokens,
      &[
        (TokenType::Binary, "0b1010"),
        (TokenType::SemiColon, ";"),
        (TokenType::Binary, "0B11"),
        (TokenType::SemiColon, ";"),
        (TokenType::Eof, ""),
      ],
    );

    let LexResult { tokens, diagnostics } = lex("0b;");

    assert_eq!(diagnostics.len(), 1, "expected diagnostic for binary literal without digits");
    match &diagnostics[0] {
      DiagnosticMessage::ExpectedBinary(token) => assert_eq!(token.lexeme, "0b"),
      other => panic!("unexpected diagnostic: {:?}", other),
    }

    assert_tokens(&tokens, &[(TokenType::SemiColon, ";"), (TokenType::Eof, "")]);
  }

  #[test]
  fn lexes_hex_literals_and_reports_missing_digits() {
    let LexResult { tokens, diagnostics } = lex("0xCAFE 0X10;");

    assert!(diagnostics.is_empty(), "unexpected diagnostics: {:?}", diagnostics);

    assert_tokens(
      &tokens,
      &[
        (TokenType::Hex, "0xCAFE"),
        (TokenType::Hex, "0X10"),
        (TokenType::SemiColon, ";"),
        (TokenType::Eof, ""),
      ],
    );

    let LexResult { tokens, diagnostics } = lex("0x;");

    assert_eq!(diagnostics.len(), 1, "expected diagnostic for hex literal without digits");
    match &diagnostics[0] {
      DiagnosticMessage::ExpectedHex(token) => assert_eq!(token.lexeme, "0x"),
      other => panic!("unexpected diagnostic: {:?}", other),
    }

    assert_tokens(&tokens, &[(TokenType::SemiColon, ";"), (TokenType::Eof, "")]);
  }

  #[test]
  fn lexes_identifiers_keywords_and_types() {
    let LexResult { tokens, diagnostics } = lex("let binary hex value SomeIdentifier _hidden");

    assert!(diagnostics.is_empty(), "unexpected diagnostics: {:?}", diagnostics);

    assert_tokens(
      &tokens,
      &[
        (TokenType::Let, "let"),
        (TokenType::BinaryType, "binary"),
        (TokenType::HexType, "hex"),
        (TokenType::Identifier, "value"),
        (TokenType::Identifier, "SomeIdentifier"),
        (TokenType::Identifier, "_hidden"),
        (TokenType::Eof, ""),
      ],
    );
  }

  #[test]
  fn lexes_strings_and_comments() {
    let LexResult { tokens, diagnostics } = lex("\"hello\" // comment\n/* block */");

    assert!(diagnostics.is_empty(), "unexpected diagnostics: {:?}", diagnostics);

    assert_tokens(
      &tokens,
      &[
        (TokenType::String, "\"hello\""),
        (TokenType::Comment, "// comment"),
        (TokenType::MultiLineComment, "/* block */"),
        (TokenType::Eof, ""),
      ],
    );
  }
}
