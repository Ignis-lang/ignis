pub mod text_span;
pub mod token;

use enums::token_type::TokenType;

use text_span::TextSpan;
use token::Token;

/*
 * Lexer
 *
 * The `start` and `current` variables allow each **string** to be indexed.
 *
 * - start: points to the first character of the lexeme being scanned
 * - current: points to the character currently being checked.
 * - line: traces the source line of `current` to know the location of the
 * **tokens**.
*/
pub struct Lexer<'a> {
  source: &'a str,
  pub tokens: Vec<Token>,
  start: usize,
  line: usize,
  current: usize,
  module_path: String,
}

impl<'a> Lexer<'a> {
  pub fn new(source: &'a str, module_path: String) -> Self {
    Self {
      source,
      tokens: vec![],
      start: 0,
      line: 0,
      current: 0,
      module_path,
    }
  }

  /**
  The scanner checks all characters in the code and
  enlarges tokens until it runs out of characters.
  At the end a final token of type **EOF** is added.
  */
  pub fn scan_tokens(&mut self) {
    loop {
      self.start = self.current;

      self.scan_token();
      if self.is_at_end() {
        break;
      }
    }

    self.tokens.push(Token::new(
      TokenType::Eof,
      TextSpan::new(
        0,
        0,
        self.line,
        '\0'.to_string(),
        0,
        self.module_path.clone(),
      ),
    ));
  }

  /**
  Help function that checks that all characters have been completed.
  */
  fn is_at_end(&self) -> bool {
    self.current >= self.source.len()
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
      }
      ')' => {
        token = TokenType::RightParen;
      }
      '{' => {
        token = TokenType::LeftBrace;
      }
      '}' => {
        token = TokenType::RightBrace;
      }
      '[' => {
        token = TokenType::LeftBrack;
      }
      ']' => {
        token = TokenType::RightBrack;
      }
      ',' => {
        token = TokenType::Comma;
      }
      '.' => {
        token = TokenType::Dot;
      }
      ';' => {
        token = TokenType::SemiColon;
      }
      '@' => {
        token = TokenType::At;
      }
      '-' => {
        token = if self.match_char('=') {
          TokenType::DecrementAndAssign
        } else if self.match_char('-') {
          TokenType::Decrement
        } else if self.match_char('>') {
          TokenType::Arrow
        } else {
          TokenType::Minus
        };
      }
      '+' => {
        token = if self.match_char('=') {
          TokenType::IncrementAndAssign
        } else if self.match_char('+') {
          TokenType::Increment
        } else {
          TokenType::Plus
        };
      }
      '*' => {
        token = TokenType::Asterisk;
      }
      ':' => {
        token = TokenType::Colon;
      }
      '%' => {
        token = TokenType::Mod;
      }
      '!' => {
        token = if self.match_char('=') {
          TokenType::BangEqual
        } else {
          TokenType::Bang
        };
      }
      '=' => {
        token = if self.match_char('=') {
          TokenType::EqualEqual
        } else {
          TokenType::Equal
        };
      }
      '<' => {
        token = if self.match_char('=') {
          TokenType::LessEqual
        } else {
          TokenType::Less
        };
      }
      '>' => {
        token = if self.match_char('=') {
          TokenType::GreaterEqual
        } else {
          TokenType::Greater
        };
      }
      '|' => {
        token = if self.match_char('|') {
          TokenType::Or
        } else {
          TokenType::Pipe
        };
      }
      '&' => {
        token = if self.match_char('&') {
          TokenType::And
        } else {
          TokenType::Ampersand
        };
      }
      '?' => {
        token = TokenType::QuestionMark;
      }
      '/' => {
        token = TokenType::Comment;
        if self.match_char('*') {
          while !self.is_at_end() {
            if self.match_char('*') && self.match_char('/') {
              break;
            }

            self.advance();
          }
        } else if self.match_char('/') {
          while self.peek() != '\n' && !self.is_at_end() {
            self.advance();
          }
        } else {
          token = TokenType::Slash;
        }
      }
      '"' => {
        if let Some(value) = self.string() {
          self.add_token_string(value);
          return;
        }
      }
      '`' => {
        self.start = self.current - 1; // Guardar la posición inicial del literal de plantilla
        self.template_string();
      }
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
      }
    }

    self.add_token(token);
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
      "int" => Some(TokenType::IntType),
      "float" => Some(TokenType::FloatType),
      "char" => Some(TokenType::CharType),
      "void" => Some(TokenType::Void),
      "extern" => Some(TokenType::Extern),
      "continue" => Some(TokenType::Continue),
      _ => None,
    }
  }

  fn is_identifier_letter(&self) -> bool {
    let c: char = self.peek();

    c.is_ascii_digit() || c.is_ascii_lowercase() || c.is_ascii_uppercase() || c == '_'
  }

  fn template_string(&mut self) {
    loop {
      match self.peek() {
        '`' => {
          self.add_token(TokenType::TemplateStringEnd);
          self.advance();
          break;
        }
        '$' if self.peek_next() == '{' => {
          self.add_token(TokenType::ExpressionStart);
          self.advance();
          self.advance();
        }
        _ => {
          self.advance();
        }
      }

      if self.is_at_end() {
        break;
      }
    }
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
          _ => {}
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
  This method receives a character.
  It checks if the next character is a space or a line break
  or if the next character does not match the one passed by parameter,
  if these cases are met then it returns `false`.
  Otherwise, it increments `current` by one and returns true.
  */
  fn match_char(&mut self, c: char) -> bool {
    if self.is_at_end() || self.peek() != c {
      return false;
    }

    self.current += 1;

    true
  }

  /**
  Method that gets the current character in the source code and increments it into a `current`.
  */
  fn advance(&mut self) -> char {
    self.current += 1;
    self.source.chars().nth(self.current - 1).unwrap_or('\0')
  }

  fn number(&mut self) -> bool {
    let mut is_float: bool = false;
    while self.peek().is_ascii_digit() || self.peek() == '_' {
      if self.peek() == '_' {
        if !self.peek_next().is_ascii_digit() || !self.peek_prev().is_ascii_digit() {
          return false;
        }
      }

      self.advance();
    }

    if self.peek() == '.' && self.peek_next().is_ascii_digit() {
      self.advance();

      while self.peek().is_ascii_digit() || self.peek() == '_' {
        if self.peek() == '_' {
          if !self.peek_next().is_ascii_digit() || !self.peek_prev().is_ascii_digit() {
            return false;
          }
        }

        self.advance();
      }

      is_float = true;
    }

    is_float
  }

  fn peek_next(&self) -> char {
    self.source.chars().nth(self.current + 1).unwrap_or('\0')
  }

  fn peek_prev(&self) -> char {
    self.source.chars().nth(self.current - 1).unwrap_or('\0')
  }

  /**
  This method takes returns the current character
  if it is not a line break.
  */
  fn peek(&self) -> char {
    self.source.chars().nth(self.current).unwrap_or('\0')
  }

  fn add_token_string(&mut self, value: String) {
    self.tokens.push(Token::new(
      TokenType::String,
      TextSpan::new(
        self.start + 1,
        self.current - 1,
        self.line + 1,
        value,
        self.current - self.start,
        self.module_path.clone(),
      ),
    ));
  }

  /**
  Where `advance()` is for input, `addToken()` is for output.
  It takes the text of the current lexeme and creates a new token.
  */
  fn add_token(&mut self, kind: TokenType) {
    let mut literal = self.source[self.start..self.current].to_string();

    if kind == TokenType::Comment {
      return;
    }

    if kind == TokenType::Int || kind == TokenType::Float {
      if literal.contains("_") {
        literal = literal.replace("_", "");
      }
    }

    self.tokens.push(Token::new(
      kind,
      TextSpan::new(
        self.start,
        self.current,
        self.line + 1,
        literal,
        self.current - self.start,
        self.module_path.clone(),
      ),
    ));
  }

  pub fn display_lexer(&self) {
    for token in &self.tokens {
      println!("Token: ");
      println!("  Type: {:?}", token.kind);
      println!("  Line: {}", token.span.line);
      println!("  Column: {}", token.span.column);
      println!("  Literal: {}", token.span.literal);
      println!("");
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;

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
}
