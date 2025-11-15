use ignis_token::token_types::TokenType;

use crate::parser::IgnisParser;

impl IgnisParser {
  /// Check if token can start a declaration
  /// FIRST(<declaration>) = { function, import, export, const, extern }
  pub(crate) fn is_declaration_start(
    &self,
    token_type: TokenType,
  ) -> bool {
    matches!(
      token_type,
      TokenType::Function | TokenType::Import | TokenType::Export | TokenType::Const | TokenType::Extern
    )
  }

  /// Check if token can start a statement
  /// FIRST(<statement>) = { let, if, while, return, break, continue, { } ∪ FIRST(<expression>)
  pub(crate) fn is_statement_start(
    &self,
    token_type: TokenType,
  ) -> bool {
    matches!(
      token_type,
      TokenType::Let
        | TokenType::If
        | TokenType::While
        | TokenType::Return
        | TokenType::Break
        | TokenType::Continue
        | TokenType::LeftBrace
        | TokenType::Const
    ) || self.is_expression_start(token_type)
  }

  /// Check if token can start an expression
  /// FIRST(<expression>) = { identifier, literals, (, [ }
  pub(crate) fn is_expression_start(
    &self,
    token_type: TokenType,
  ) -> bool {
    matches!(
      token_type,
      TokenType::Identifier
        | TokenType::Int
        | TokenType::Float
        | TokenType::String
        | TokenType::Char
        | TokenType::Hex
        | TokenType::Binary
        | TokenType::True
        | TokenType::False
        | TokenType::Null
        | TokenType::LeftParen
        | TokenType::LeftBrack
        | TokenType::Bang
        | TokenType::Tilde
        | TokenType::Minus
        | TokenType::Plus
        | TokenType::Increment
        | TokenType::Decrement
    )
  }

  /// Check if token marks the end of a block
  pub(crate) fn is_block_end(
    &self,
    token_type: TokenType,
  ) -> bool {
    matches!(token_type, TokenType::RightBrace | TokenType::Eof)
  }

  /// Generic synchronization: consume tokens until stop_when returns true
  /// Safety limit to prevent infinite loops
  pub(crate) fn synchronize_until<F>(
    &mut self,
    stop_when: F,
  ) where
    F: Fn(TokenType) -> bool,
  {
    if !stop_when(self.peek().type_) && !self.at(TokenType::Eof) {
      self.bump();
    }

    let mut steps = 0usize;
    const MAX_STEPS: usize = 10_000;

    while steps < MAX_STEPS {
      let current_token = self.peek().type_;

      if stop_when(current_token) {
        break;
      }

      if self.at(TokenType::Eof) {
        break;
      }

      self.bump();
      steps += 1;
    }
  }

  /// Synchronize after a failed declaration
  /// Stop at: ; or } or start of next declaration
  pub(crate) fn synchronize_after_declaration(&mut self) {
    loop {
      let current_token = self.peek().type_;

      if current_token == TokenType::SemiColon
        || current_token == TokenType::RightBrace
        || self.is_declaration_start(current_token)
        || current_token == TokenType::Eof
      {
        break;
      }

      self.bump();
    }

    if self.at(TokenType::SemiColon) {
      self.bump();
    }
  }

  /// Synchronize after a failed statement
  /// Stop at: ; or } or start of next statement
  pub(crate) fn synchronize_after_statement(&mut self) {
    loop {
      let current_token = self.peek().type_;

      if current_token == TokenType::SemiColon
        || current_token == TokenType::RightBrace
        || self.is_statement_start(current_token)
        || current_token == TokenType::Eof
      {
        break;
      }

      self.bump();
    }

    if self.at(TokenType::SemiColon) {
      self.bump();
    }
  }

  /// Synchronize to expression boundary
  /// Stop at: ; , ) ] } or start of statement
  pub(crate) fn synchronize_after_expression(&mut self) {
    loop {
      let current_token = self.peek().type_;

      if matches!(
        current_token,
        TokenType::SemiColon
          | TokenType::Comma
          | TokenType::RightParen
          | TokenType::RightBrack
          | TokenType::RightBrace
          | TokenType::Eof
      ) || self.is_statement_start(current_token)
      {
        break;
      }

      self.bump();
    }
  }

  /// Synchronize within a delimited context (arguments, vector elements, etc.)
  /// Stop at: delimiter or closing bracket
  pub(crate) fn synchronize_to_delimiter(
    &mut self,
    delimiter: TokenType,
    closing: TokenType,
  ) {
    self
      .synchronize_until(|token_type| token_type == delimiter || token_type == closing || token_type == TokenType::Eof);
  }
}
