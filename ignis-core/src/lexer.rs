use ignis_config::IgnisConfig;
use ignis_token::{token::Token, token_types::TokenType};
use colored::*;

/*
 * Lexer
 *
 * The `start` and `current` variables allow each **string** to be indexed.
 *
 * - start: points to the first character of the lexeme being scanned
 * - current: points to the character currently being checked.
 * - line: traces the source line of `current` to know the location of the **tokens**.
 */
pub struct Lexer<'a> {
  config: Box<IgnisConfig>,
  source: &'a str,
  chars: std::str::Chars<'a>,
  pub tokens: Vec<Token>,
  start: usize,
  line: usize,
  current: usize,
  module_path: String,
}

impl<'a> Lexer<'a> {
  pub fn new(config: Box<IgnisConfig>, source: &'a str, file: String) -> Self {
    Self {
      config,
      chars: source.chars(),
      source,
      tokens: vec![],
      start: 0,
      line: 0,
      current: 0,
      module_path: file,
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
    let mut token: TokenType = TokenType::Bad;

    if c == ' ' || c == '\r' || c == '\t' {
      return;
    }

    if c == '\n' {
      self.line += 1;
      return;
    }

    match c {
      '(' => {
        token = TokenType::LeftParen;
      },
      ')' => {
        token = TokenType::RightParen;
      },
      '{' => {
        token = TokenType::LeftBrace;
      },
      '}' => {
        token = TokenType::RightBrace;
      },
      '[' => {
        token = TokenType::LeftBrack;
      },
      ']' => {
        token = TokenType::RightBrack;
      },
      ',' => {
        token = TokenType::Comma;
      },
      '.' => {
        token = TokenType::Dot;
      },
      ';' => {
        token = TokenType::SemiColon;
      },
      '@' => {
        token = TokenType::At;
      },
      '-' => {
        token = if self.match_char('=') {
          TokenType::SubtractAssign
        } else if self.match_char('-') {
          TokenType::Decrement
        } else if self.match_char('>') {
          TokenType::Arrow
        } else {
          TokenType::Minus
        };
      },
      '+' => {
        token = if self.match_char('=') {
          TokenType::AddAssign
        } else if self.match_char('+') {
          TokenType::Increment
        } else {
          TokenType::Plus
        };
      },
      '*' => {
        token = TokenType::Asterisk;
      },
      ':' => {
        token = TokenType::Colon;
      },
      '%' => {
        token = TokenType::Mod;
      },
      '!' => {
        token = if self.match_char('=') {
          TokenType::BangEqual
        } else {
          TokenType::Bang
        };
      },
      '=' => {
        token = if self.match_char('=') {
          TokenType::EqualEqual
        } else {
          TokenType::Equal
        };
      },
      '<' => {
        token = if self.match_char('=') {
          TokenType::LessEqual
        } else {
          TokenType::Less
        };
      },
      '>' => {
        token = if self.match_char('=') {
          TokenType::GreaterEqual
        } else {
          TokenType::Greater
        };
      },
      '|' => {
        token = if self.match_char('|') {
          TokenType::Or
        } else {
          TokenType::Pipe
        };
      },
      '&' => {
        token = if self.match_char('&') {
          TokenType::And
        } else {
          TokenType::Ampersand
        };
      },
      '#' => {
        token = TokenType::Hash;
      },
      '?' => {
        token = TokenType::QuestionMark;
      },
      '/' => {
        if self.peek() == '/' || self.peek() == '*' {
          self.comments();
          return;
        } else {
          token = TokenType::Slash;
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
        token = TokenType::Hex;
      },
      '0' if self.peek() == 'b' => {
        self.advance();
        self.binary_number();
        token = TokenType::Binary;
      },
      _ => {
        if c.is_ascii_digit() {
          if self.number() {
            token = TokenType::Float;
          } else {
            token = TokenType::Int;
          }
        }

        if c.is_ascii_lowercase() || c.is_ascii_uppercase() || c == '_' {
          token = self.identifier();
        }
      },
    }

    self.add_token(token);
  }

  fn comments(&mut self) {
    if self.match_char('/') {
      while self.peek() != '\n' && !self.is_at_end() {
        self.advance();
      }

      self.add_token(TokenType::Comment);
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
    }
  }

  fn get_keyword(key: &str) -> Option<TokenType> {
    match key {
      "class" => Some(TokenType::Class),
      "super" => Some(TokenType::Super),
      "else" => Some(TokenType::Else),
      "false" => Some(TokenType::False),
      "true" => Some(TokenType::True),
      "function" => Some(TokenType::Function),
      "for" => Some(TokenType::For),
      "in" => Some(TokenType::In),
      "of" => Some(TokenType::Of),
      "if" => Some(TokenType::If),
      "null" => Some(TokenType::Null),
      "return" => Some(TokenType::Return),
      "this" => Some(TokenType::This),
      "let" => Some(TokenType::Let),
      "const" => Some(TokenType::Const),
      "while" => Some(TokenType::While),
      "enum" => Some(TokenType::Enum),
      "export" => Some(TokenType::Export),
      "import" => Some(TokenType::Import),
      "from" => Some(TokenType::From),
      "mut" => Some(TokenType::Mut),
      "as" => Some(TokenType::As),
      "break" => Some(TokenType::Break),
      "readonly" => Some(TokenType::ReadOnly),
      "static" => Some(TokenType::Static),
      "final" => Some(TokenType::Final),
      "public" => Some(TokenType::Public),
      "private" => Some(TokenType::Private),
      "interface" => Some(TokenType::Interface),
      "extends" => Some(TokenType::Extends),
      "implements" => Some(TokenType::Implements),
      "string" => Some(TokenType::StringType),
      "boolean" => Some(TokenType::BooleanType),
      "i8" => Some(TokenType::Int8Type),
      "i16" => Some(TokenType::Int16Type),
      "i32" => Some(TokenType::Int32Type),
      "i64" => Some(TokenType::Int64Type),
      "u8" => Some(TokenType::UnsignedInt8Type),
      "u16" => Some(TokenType::UnsignedInt16Type),
      "u32" => Some(TokenType::UnsignedInt32Type),
      "u64" => Some(TokenType::UnsignedInt64Type),
      "f32" => Some(TokenType::Float32Type),
      "f64" => Some(TokenType::Float64Type),
      "char" => Some(TokenType::CharType),
      "void" => Some(TokenType::Void),
      "hex" => Some(TokenType::HexType),
      "binary" => Some(TokenType::BinaryType),
      "extern" => Some(TokenType::Extern),
      "continue" => Some(TokenType::Continue),
      "new" => Some(TokenType::New),
      "unknown" => Some(TokenType::Unknown),
      "type" => Some(TokenType::Type),
      "is" => Some(TokenType::Is),
      "record" => Some(TokenType::Record),
      "decorator" => Some(TokenType::Decorator),
      "meta" => Some(TokenType::Meta),
      "declare" => Some(TokenType::Declare),
      "namespace" => Some(TokenType::Namespace),
      _ => None,
    }
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
    let kind: Option<TokenType> = Self::get_keyword(value.as_str());

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
          _ => {},
        }
      } else {
        result.push(self.peek());
      }

      self.advance();
    }

    if self.is_at_end() {
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
