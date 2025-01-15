use std::fmt;

use ignis_token::{token::Token, token_types::TokenType};

use super::diagnostic_report::{DiagnosticLevel, DiagnosticReport};

#[derive(Debug, Clone, PartialEq)]
pub enum DiagnosticMessage {
  // #region Lexer
  ExpectedToken(TokenType, Token),
  UnexpectedToken(TokenType, Token),
  InvalidToken(Token),
  UntermintedComment(Token),
  UntermintedCharacter(Token),
  InvalidCharacterEscapeSequence(Token),
  InvalidCharacter(Token),
  UnterminatedString(Token),
  // #endregion Lexer
  // #region Parser
  ExpectedExpression(Token),
  ExpectedVariableName(Token),
  ExpectedReturnTypeAfterFunction(Token),
  ExpectedAfterExpression(Box<TokenType>, Token, Box<Token>),
  ExpectedExpressionAfter(Token),
  InvalidAssignmentTarget(Token),
  ExpectedTypeAfterVariable(Token),
  InvalidNumberOfArguments(usize, usize, Token),
  ExpectedSemicolonAfterExpression(Token),
  InvalidEnumMember(Token),
  ExpectedTypeAfterPipe(Token),
  ExpectedDelimiter(Token),
  ExpectedType(Token),
  UnexpectedKeyword(Token),
  ExpectedIdentifier(Token),
  UninitializedConstant(Token),
  ExpectedPattern(Token),
  // #endregion Parser
  // #region Analyzer

  // #endregion Analyzer
}

impl fmt::Display for DiagnosticMessage {
  fn fmt(
    &self,
    f: &mut fmt::Formatter<'_>,
  ) -> fmt::Result {
    match self {
      DiagnosticMessage::UntermintedComment(token) => {
        write!(f, "Unterminted comment '{}'", token.lexeme)
      },
      DiagnosticMessage::UntermintedCharacter(token) => {
        write!(f, "Unterminted character '{}'", token.lexeme)
      },
      DiagnosticMessage::InvalidCharacterEscapeSequence(token) => {
        write!(f, "Invalid character escape sequence '{}'", token.lexeme)
      },
      DiagnosticMessage::InvalidCharacter(token) => {
        write!(f, "Invalid character '{}'", token.lexeme)
      },
      DiagnosticMessage::UnterminatedString(token) => {
        write!(f, "Unterminated string '{}'", token.lexeme)
      },
      DiagnosticMessage::ExpectedToken(expected_token, token) => {
        write!(f, "Expected '{}' after '{}'", expected_token, token.lexeme)
      },
      DiagnosticMessage::UnexpectedToken(kind, token) => {
        write!(f, "Unexpected token '{}' after '{}'", kind, token.lexeme)
      },
      DiagnosticMessage::InvalidToken(token) => {
        write!(f, "Invalid token '{}'", token.lexeme)
      },
      DiagnosticMessage::ExpectedExpression(token) => {
        write!(f, "Expected expression after '{}'", token.lexeme)
      },
      DiagnosticMessage::ExpectedVariableName(token) => {
        write!(f, "Expected variable name after '{}'", token.lexeme)
      },
      DiagnosticMessage::ExpectedReturnTypeAfterFunction(token) => {
        write!(f, "Expected return type after '{}'", token.lexeme)
      },
      DiagnosticMessage::ExpectedAfterExpression(expected_token_type, token, _token2) => {
        write!(f, "Expected '{}' after '{}' in expression", expected_token_type, token.lexeme)
      },
      DiagnosticMessage::ExpectedExpressionAfter(token) => {
        write!(f, "Expected expression after '{}'", token.lexeme)
      },
      DiagnosticMessage::InvalidAssignmentTarget(_) => write!(f, "Invalid assignment target"),
      DiagnosticMessage::ExpectedTypeAfterVariable(token) => {
        write!(f, "Expected type after '{}'", token.lexeme)
      },
      DiagnosticMessage::InvalidNumberOfArguments(max, num, _) => {
        write!(f, "Expected {} arguments, got {}", max, num)
      },
      DiagnosticMessage::ExpectedSemicolonAfterExpression(_) => write!(f, "Expected ';' after expression"),
      DiagnosticMessage::InvalidEnumMember(_) => write!(f, "Invalid enum member"),
      DiagnosticMessage::ExpectedTypeAfterPipe(_) => write!(f, "Expected type after '|'"),
      DiagnosticMessage::ExpectedDelimiter(_) => write!(f, "Expected ',' or '}}'"),
      DiagnosticMessage::ExpectedType(token) => write!(f, "Expected type after '{}'", token.lexeme),
      DiagnosticMessage::UnexpectedKeyword(token) => write!(f, "Unexpected keyword '{}'", token.lexeme),
      DiagnosticMessage::ExpectedIdentifier(_) => write!(f, "Expected identifier"),
      DiagnosticMessage::UninitializedConstant(token) => {
        write!(f, "Constant '{}' must be initialized with a value", token.lexeme)
      },
      DiagnosticMessage::ExpectedPattern(token) => write!(f, "Expected pattern after '{}'", token.lexeme),
    }
  }
}

impl From<&DiagnosticMessage> for Token {
  fn from(value: &DiagnosticMessage) -> Self {
    match value {
      DiagnosticMessage::ExpectedExpression(token)
      | DiagnosticMessage::ExpectedToken(_, token)
      | DiagnosticMessage::ExpectedVariableName(token)
      | DiagnosticMessage::ExpectedReturnTypeAfterFunction(token)
      | DiagnosticMessage::ExpectedAfterExpression(_, token, _)
      | DiagnosticMessage::ExpectedExpressionAfter(token)
      | DiagnosticMessage::UnexpectedToken(_, token)
      | DiagnosticMessage::InvalidAssignmentTarget(token)
      | DiagnosticMessage::ExpectedTypeAfterVariable(token)
      | DiagnosticMessage::InvalidNumberOfArguments(_, _, token)
      | DiagnosticMessage::ExpectedSemicolonAfterExpression(token)
      | DiagnosticMessage::InvalidEnumMember(token)
      | DiagnosticMessage::ExpectedTypeAfterPipe(token)
      | DiagnosticMessage::ExpectedDelimiter(token)
      | DiagnosticMessage::ExpectedType(token)
      | DiagnosticMessage::UnexpectedKeyword(token)
      | DiagnosticMessage::ExpectedIdentifier(token)
      | DiagnosticMessage::UninitializedConstant(token)
      | DiagnosticMessage::InvalidToken(token)
      | DiagnosticMessage::UntermintedComment(token)
      | DiagnosticMessage::UntermintedCharacter(token)
      | DiagnosticMessage::InvalidCharacterEscapeSequence(token)
      | DiagnosticMessage::InvalidCharacter(token)
      | DiagnosticMessage::UnterminatedString(token)
      | DiagnosticMessage::ExpectedPattern(token) => token.clone(),
    }
  }
}

impl DiagnosticMessage {
  fn code(&self) -> String {
    match self {
      DiagnosticMessage::ExpectedToken(_, _) => "I0001".to_string(),
      DiagnosticMessage::UnexpectedToken(_, _) => "I0002".to_string(),
      DiagnosticMessage::InvalidToken(_) => "I0003".to_string(),
      DiagnosticMessage::ExpectedExpression(_) => "I0001".to_string(),
      DiagnosticMessage::ExpectedVariableName(_) => "I0003".to_string(),
      DiagnosticMessage::ExpectedReturnTypeAfterFunction(_) => "I0004".to_string(),
      DiagnosticMessage::ExpectedAfterExpression(_, _, _) => "I0005".to_string(),
      DiagnosticMessage::ExpectedExpressionAfter(_) => "I0006".to_string(),
      DiagnosticMessage::InvalidAssignmentTarget(_) => "I0008".to_string(),
      DiagnosticMessage::ExpectedTypeAfterVariable(_) => "I0009".to_string(),
      DiagnosticMessage::InvalidNumberOfArguments(_, _, _) => "I0010".to_string(),
      DiagnosticMessage::ExpectedSemicolonAfterExpression(_) => "I0011".to_string(),
      DiagnosticMessage::InvalidEnumMember(_) => "I0012".to_string(),
      DiagnosticMessage::ExpectedTypeAfterPipe(_) => "I0013".to_string(),
      DiagnosticMessage::ExpectedDelimiter(_) => "I0014".to_string(),
      DiagnosticMessage::ExpectedType(_) => "I0015".to_string(),
      DiagnosticMessage::UnexpectedKeyword(_) => "I0016".to_string(),
      DiagnosticMessage::ExpectedIdentifier(_) => "I0017".to_string(),
      DiagnosticMessage::UninitializedConstant(_) => "I0018".to_string(),
      DiagnosticMessage::ExpectedPattern(_) => "I0019".to_string(),
      DiagnosticMessage::UntermintedComment(_) => "I0020".to_string(),
      DiagnosticMessage::UntermintedCharacter(_) => "I0021".to_string(),
      DiagnosticMessage::InvalidCharacterEscapeSequence(_) => "I0022".to_string(),
      DiagnosticMessage::InvalidCharacter(_) => "I0023".to_string(),
      DiagnosticMessage::UnterminatedString(_) => "I0024".to_string(),
    }
  }

  fn get_hint(&self) -> Option<DiagnosticReport> {
    let hint = match self {
      DiagnosticMessage::ExpectedDelimiter(token) => Some(DiagnosticReport::new(
        DiagnosticLevel::Hint,
        "Try adding ',' | ';' | '}' | '[' | ']' | '{' ".to_string(),
        "IH0014".to_string(),
        token.clone(),
        None,
      )),
      DiagnosticMessage::UnexpectedKeyword(token) => {
        let message = format!("Try deleting '{}' or adding a '_': '{}_'", token.lexeme, token.lexeme);

        Some(DiagnosticReport::new(
          DiagnosticLevel::Hint,
          message,
          "IH0016".to_string(),
          token.clone(),
          None,
        ))
      },
      _ => None,
    };

    hint
  }

  pub fn report(self) -> DiagnosticReport {
    DiagnosticReport::new(
      DiagnosticLevel::Error,
      self.to_string(),
      self.code(),
      (&self).into(),
      self.get_hint(),
    )
  }
}
