use ignis_config::IgnisConfig;
use ignis_token::{token::Token, token_types::TokenType};
use colored::*;

use crate::diagnostics::message::DiagnosticMessage;

/*
 * Lexer
 *
 * The `start` and `current` variables allow each **string** to be indexed.
 *
 * - start: points to the first character of the lexeme being scanned
 * - current: points to the character currently being checked.
 * - line: traces the source line of `current` to know the location of the **tokens**.
 */
pub struct IgnisLexer<'a> {
  config: Box<IgnisConfig>,
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
    config: Box<IgnisConfig>,
    source: &'a str,
    file: String,
  ) -> Self {
    Self {
      config,
      chars: source.chars(),
      source,
      tokens: vec![],
      start: 0,
      line: 0,
      current: 0,
      module_path: file,
      diagnostics: vec![],
    }
  }

  /**
   * The scanner checks all characters in the code and
   * enlarges tokens until it runs out of characters.
   * At the end a final token of type **EOF** is added.
   */
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
      0,
      self.module_path.clone(),
    ));
  }

  /**
   * Help function that checks that all characters have been completed.
   */
  fn is_at_end(&self) -> bool {
    self.chars.as_str().is_empty()
  }

  fn scan_token(&mut self) {
    let c: char = self.advance();

    if c == ' ' || c == '\r' || c == '\t' {
      return;
    }

    if c == '\n' {
      self.line += 1;
      return;
    }

    match c {
      '(' => {
        self.add_token(TokenType::LeftParen);
      },
      ')' => {
        self.add_token(TokenType::RightParen);
      },
      '{' => {
        self.add_token(TokenType::LeftBrace);
      },
      '}' => {
        self.add_token(TokenType::RightBrace);
      },
      '[' => {
        self.add_token(TokenType::LeftBrack);
      },
      ']' => {
        self.add_token(TokenType::RightBrack);
      },
      ',' => {
        self.add_token(TokenType::Comma);
      },
      '.' => {
        if self.match_char('.') {
          if self.match_char('.') {
            self.add_token(TokenType::Variadic);
          } else if self.match_char('=') {
            self.add_token(TokenType::RangeInclusive);
          } else {
            self.add_token(TokenType::Range);
          }
        } else {
          self.add_token(TokenType::Dot);
        }
      },
      ';' => {
        self.add_token(TokenType::SemiColon);
      },
      '@' => {
        self.add_token(TokenType::At);
      },
      '-' => {
        if self.match_char('=') {
          self.add_token(TokenType::SubtractAssign);
        } else if self.match_char('-') {
          self.add_token(TokenType::Decrement);
        } else if self.match_char('>') {
          self.add_token(TokenType::Arrow);
        } else if self.peek().is_ascii_digit() {
          if self.number() {
            self.add_token(TokenType::Float);
          } else {
            self.add_token(TokenType::Int);
          }
        } else {
          self.add_token(TokenType::Minus);
        }
      },
      '+' => {
        if self.match_char('=') {
          self.add_token(TokenType::AddAssign);
        } else if self.match_char('+') {
          self.add_token(TokenType::Increment);
        } else {
          self.add_token(TokenType::Plus);
        };
      },
      '*' => {
        self.add_token(TokenType::Asterisk);
      },
      ':' => {
        if self.match_char(':') {
          self.add_token(TokenType::DoubleColon);
        } else {
          self.add_token(TokenType::Colon);
        };
      },
      '%' => {
        self.add_token(TokenType::Mod);
      },
      '!' => {
        if self.match_char('=') {
          self.add_token(TokenType::BangEqual);
        } else {
          self.add_token(TokenType::Bang);
        };
      },
      '=' => {
        if self.match_char('=') {
          self.add_token(TokenType::EqualEqual);
        } else {
          self.add_token(TokenType::Equal);
        };
      },
      '<' => {
        if self.match_char('=') {
          self.add_token(TokenType::LessEqual);
        } else {
          self.add_token(TokenType::Less);
        };
      },
      '>' => {
        if self.match_char('=') {
          self.add_token(TokenType::GreaterEqual);
        } else {
          self.add_token(TokenType::Greater);
        };
      },
      '|' => {
        if self.match_char('|') {
          self.add_token(TokenType::Or);
        } else {
          self.add_token(TokenType::Pipe);
        };
      },
      '&' => {
        if self.match_char('&') {
          self.add_token(TokenType::And);
        } else {
          self.add_token(TokenType::Ampersand);
        };
      },
      '#' => {
        self.add_token(TokenType::Hash);
      },
      '?' => {
        self.add_token(TokenType::QuestionMark);
      },
      '/' => {
        if self.peek() == '/' || self.peek() == '*' {
          self.comments();
          return;
        } else {
          self.add_token(TokenType::Slash);
        }
      },
      '"' => {
        if let Some(value) = self.string() {
          self.add_token_string(value);
          return;
        }
      },
      '0' if self.peek() == 'x' => {
        self.hex_number();
        self.add_token(TokenType::Hex);
      },
      '0' if self.peek() == 'b' => {
        self.advance();
        self.binary_number();
        self.add_token(TokenType::Binary);
      },
      _ => {
        if c.is_ascii_digit() {
          if self.number() {
            self.add_token(TokenType::Float);
          } else {
            self.add_token(TokenType::Int);
          }
        }

        if c.is_ascii_lowercase() || c.is_ascii_uppercase() || c == '_' {
          let token = self.identifier();
          self.add_token(token);
        }
      },
    }
  }

  fn comments(&mut self) {
    if self.match_char('/') {
      while self.peek() != '\n' && !self.is_at_end() {
        self.advance();
      }

      self.add_token(TokenType::Comment);
      return;
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

      self.add_token(if is_doc_comment {
        TokenType::DocComment
      } else {
        TokenType::MultiLineComment
      });
      return;
    }

    self.diagnostics.push(DiagnosticMessage::UntermintedComment(Token::new(
      TokenType::Comment,
      self.source[self.start..self.current].to_string(),
      self.line,
      self.current - self.start,
      self.module_path.clone(),
    )));
  }

  fn is_identifier_letter(&self) -> bool {
    let c: char = self.peek();

    c.is_ascii_digit() || c.is_ascii_lowercase() || c.is_ascii_uppercase() || c == '_'
  }

  fn identifier(&mut self) -> TokenType {
    while self.is_identifier_letter() {
      self.advance();
    }

    let value: String = self.source[self.start..self.current].to_string();
    let kind: Option<TokenType> = TokenType::get_keyword_from_string(value.as_str());

    kind.unwrap_or(TokenType::Identifier)
  }

  fn string(&mut self) -> Option<String> {
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
      self.diagnostics.push(DiagnosticMessage::UnterminatedString(Token::new(
        TokenType::String,
        result.clone(),
        self.line + 1,
        self.current - self.start,
        self.module_path.clone(),
      )));

      return None;
    }

    self.advance();

    Some(result)
  }

  /**
   * This method receives a character.
   * It checks if the next character is a space or a line break
   * or if the next character does not match the one passed by parameter,
   * if these cases are met then it returns `false`.
   * Otherwise, it increments `current` by one and returns true.
   */
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

  /**
   * Method that gets the current character in the source code and increments it into a `current`.
   */
  fn advance(&mut self) -> char {
    let next_char = self.chars.next();

    if let Some(c) = next_char {
      self.current += c.len_utf8();
    }

    next_char.unwrap_or('\0')
  }

  fn number(&mut self) -> bool {
    let mut is_float: bool = false;
    while self.peek().is_ascii_digit() || self.peek() == '_' {
      if self.peek() == '_' && (!self.peek_next().is_ascii_digit() || !self.peek_prev().is_ascii_digit()) {
        return false;
      }

      self.advance();
    }

    if self.peek() == '.' && self.peek_next().is_ascii_digit() {
      self.advance();

      while self.peek().is_ascii_digit() || self.peek() == '_' {
        if self.peek() == '_' && (!self.peek_next().is_ascii_digit() || !self.peek_prev().is_ascii_digit()) {
          return false;
        }

        self.advance();
      }

      is_float = true;
    }

    is_float
  }

  fn binary_number(&mut self) {
    while self.peek() == '0' || self.peek() == '1' {
      self.advance();
    }
  }

  fn hex_number(&mut self) {
    while self.peek().is_ascii_hexdigit() {
      self.advance();
    }

    if self.peek() == 'x' && self.peek_next().is_ascii_hexdigit() {
      self.advance();

      while self.peek().is_ascii_hexdigit() {
        self.advance();
      }
    }
  }

  fn peek_next(&self) -> char {
    self.source.chars().nth(self.current + 1).unwrap_or('\0')
  }

  fn peek_prev(&self) -> char {
    self.source.chars().nth(self.current - 1).unwrap_or('\0')
  }

  /**
   * This method takes returns the current character
   * if it is not a line break.
   */
  fn peek(&self) -> char {
    self.chars.clone().next().unwrap_or('\0')
  }

  fn add_token_string(
    &mut self,
    value: String,
  ) {
    self.tokens.push(Token::new(
      TokenType::String,
      value,
      self.line + 1,
      self.current - self.start,
      self.module_path.clone(),
    ));
  }

  /**
   * Where `advance()` is for input, `addToken()` is for output.
   * It takes the text of the current lexeme and creates a new token.
   */
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
      self.current - self.start,
      self.module_path.clone(),
    ));
  }
}
