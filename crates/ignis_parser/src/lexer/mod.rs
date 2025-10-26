use ignis_diagnostics::message::DiagnosticMessage;
use ignis_token::{token::Token, token_types::TokenType};
use colored::*;
use ignis_config::IgnisConfig;

pub struct IgnisLexer<'a> {
  config: &'a IgnisConfig,
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
    config: &'a IgnisConfig,
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

      self.scan_token();
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

  fn scan_token(&mut self) {
    let c: char = self.advance();

    if c == '\n' {
      self.line += 1;
      return;
    }

    match c {
      '(' => self.add_token(TokenType::LeftParen),
      ')' => self.add_token(TokenType::RightParen),
      '{' => self.add_token(TokenType::LeftBrace),
      '}' => self.add_token(TokenType::RightBrace),
      '[' => self.add_token(TokenType::LeftBrack),
      ']' => self.add_token(TokenType::RightBrack),
      ',' => self.add_token(TokenType::Comma),
      '.' if self.match_char('.') && self.match_char('.') => self.add_token(TokenType::Variadic),
      '.' if self.match_char('.') && self.match_char('=') => self.add_token(TokenType::RangeInclusive),
      '.' if self.match_char('.') => self.add_token(TokenType::Range),
      '.' => self.add_token(TokenType::Dot),
      ' ' | '\r' | '\t' => return,
      '\n' => {
        self.line += 1;
        return;
      },
      _ => {},
    }
  }

  fn peek(&self) -> char {
    self.chars.clone().next().unwrap_or('\0')
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
