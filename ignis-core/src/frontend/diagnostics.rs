use ignis_token::{token::Token, token_types::TokenType};
use std::fmt;

use crate::diagnostics::diagnostic_report::{DiagnosticLevel, DiagnosticReport};

#[derive(Debug, Clone)]
pub enum ParserDiagnosticError {
  ExpectedExpression(Token),
  ExpectedToken(TokenType, Token),
  ExpectedVariableName(Token),
  ExpectedReturnTypeAfterFunction(Token),
  ExpectedAfterExpression(Box<TokenType>, Token, Box<Token>),
  ExpectedExpressionAfter(Token),
  UnexpectedToken(TokenType, Token),
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
}

impl fmt::Display for ParserDiagnosticError {
  fn fmt(
    &self,
    f: &mut fmt::Formatter<'_>,
  ) -> fmt::Result {
    match self {
      ParserDiagnosticError::ExpectedExpression(token) => {
        write!(f, "Expected expression after '{}'", token.lexeme)
      },
      ParserDiagnosticError::ExpectedToken(expected_token, token) => {
        write!(f, "Expected '{}' after '{}'", expected_token, token.lexeme)
      },
      ParserDiagnosticError::ExpectedVariableName(token) => {
        write!(f, "Expected variable name after '{}'", token.lexeme)
      },
      ParserDiagnosticError::ExpectedReturnTypeAfterFunction(token) => {
        write!(f, "Expected return type after '{}'", token.lexeme)
      },
      ParserDiagnosticError::ExpectedAfterExpression(expected_token_type, token, _token2) => {
        write!(f, "Expected '{}' after '{}' in expression", expected_token_type, token.lexeme)
      },
      ParserDiagnosticError::ExpectedExpressionAfter(token) => {
        write!(f, "Expected expression after '{}'", token.lexeme)
      },
      ParserDiagnosticError::UnexpectedToken(kind, token) => {
        write!(f, "Unexpected token '{}' after '{}'", kind, token.lexeme)
      },
      ParserDiagnosticError::InvalidAssignmentTarget(_) => write!(f, "Invalid assignment target"),
      ParserDiagnosticError::ExpectedTypeAfterVariable(token) => {
        write!(f, "Expected type after '{}'", token.lexeme)
      },
      ParserDiagnosticError::InvalidNumberOfArguments(max, num, _) => {
        write!(f, "Expected {} arguments, got {}", max, num)
      },
      ParserDiagnosticError::ExpectedSemicolonAfterExpression(_) => write!(f, "Expected ';' after expression"),
      ParserDiagnosticError::InvalidEnumMember(_) => write!(f, "Invalid enum member"),
      ParserDiagnosticError::ExpectedTypeAfterPipe(_) => write!(f, "Expected type after '|'"),
      ParserDiagnosticError::ExpectedDelimiter(_) => write!(f, "Expected ',' or '}}'"),
      ParserDiagnosticError::ExpectedType(token) => write!(f, "Expected type after '{}'", token.lexeme),
      ParserDiagnosticError::UnexpectedKeyword(token) => write!(f, "Unexpected keyword '{}'", token.lexeme),
      ParserDiagnosticError::ExpectedIdentifier(_) => write!(f, "Expected identifier"),
      ParserDiagnosticError::UninitializedConstant(token) => {
        write!(f, "Constant '{}' must be initialized with a value", token.lexeme)
      },
      ParserDiagnosticError::ExpectedPattern(token) => write!(f, "Expected pattern after '{}'", token.lexeme),
    }
  }
}

impl From<&ParserDiagnosticError> for Token {
  fn from(value: &ParserDiagnosticError) -> Self {
    match value {
      ParserDiagnosticError::ExpectedExpression(token)
      | ParserDiagnosticError::ExpectedToken(_, token)
      | ParserDiagnosticError::ExpectedVariableName(token)
      | ParserDiagnosticError::ExpectedReturnTypeAfterFunction(token)
      | ParserDiagnosticError::ExpectedAfterExpression(_, token, _)
      | ParserDiagnosticError::ExpectedExpressionAfter(token)
      | ParserDiagnosticError::UnexpectedToken(_, token)
      | ParserDiagnosticError::InvalidAssignmentTarget(token)
      | ParserDiagnosticError::ExpectedTypeAfterVariable(token)
      | ParserDiagnosticError::InvalidNumberOfArguments(_, _, token)
      | ParserDiagnosticError::ExpectedSemicolonAfterExpression(token)
      | ParserDiagnosticError::InvalidEnumMember(token)
      | ParserDiagnosticError::ExpectedTypeAfterPipe(token)
      | ParserDiagnosticError::ExpectedDelimiter(token)
      | ParserDiagnosticError::ExpectedType(token)
      | ParserDiagnosticError::UnexpectedKeyword(token)
      | ParserDiagnosticError::ExpectedIdentifier(token)
      | ParserDiagnosticError::UninitializedConstant(token)
      | ParserDiagnosticError::ExpectedPattern(token) => token.clone(),
    }
  }
}

impl ParserDiagnosticError {
  fn code(&self) -> String {
    match self {
      ParserDiagnosticError::ExpectedExpression(_) => "IP0001".to_string(),
      ParserDiagnosticError::ExpectedToken(_, _) => "IP0002".to_string(),
      ParserDiagnosticError::ExpectedVariableName(_) => "IP0003".to_string(),
      ParserDiagnosticError::ExpectedReturnTypeAfterFunction(_) => "IP0004".to_string(),
      ParserDiagnosticError::ExpectedAfterExpression(_, _, _) => "IP0005".to_string(),
      ParserDiagnosticError::ExpectedExpressionAfter(_) => "IP0006".to_string(),
      ParserDiagnosticError::UnexpectedToken(_, _) => "IP0007".to_string(),
      ParserDiagnosticError::InvalidAssignmentTarget(_) => "IP0008".to_string(),
      ParserDiagnosticError::ExpectedTypeAfterVariable(_) => "IP0009".to_string(),
      ParserDiagnosticError::InvalidNumberOfArguments(_, _, _) => "IP0010".to_string(),
      ParserDiagnosticError::ExpectedSemicolonAfterExpression(_) => "IP0011".to_string(),
      ParserDiagnosticError::InvalidEnumMember(_) => "IP0012".to_string(),
      ParserDiagnosticError::ExpectedTypeAfterPipe(_) => "IP0013".to_string(),
      ParserDiagnosticError::ExpectedDelimiter(_) => "IP0014".to_string(),
      ParserDiagnosticError::ExpectedType(_) => "IP0015".to_string(),
      ParserDiagnosticError::UnexpectedKeyword(_) => "IP0016".to_string(),
      ParserDiagnosticError::ExpectedIdentifier(_) => "IP0017".to_string(),
      ParserDiagnosticError::UninitializedConstant(_) => "IP0018".to_string(),
      ParserDiagnosticError::ExpectedPattern(_) => "IP0019".to_string(),
    }
  }
}

#[derive(Debug, Clone)]
pub struct ParserDiagnostic {
  pub error: ParserDiagnosticError,
}

impl ParserDiagnostic {
  pub fn new(error: ParserDiagnosticError) -> Self {
    Self { error }
  }

  pub fn report_diagnostic(&self) -> DiagnosticReport {
    let hint = match &self.error {
      ParserDiagnosticError::ExpectedDelimiter(token) => Some(DiagnosticReport::new(
        DiagnosticLevel::Hint,
        "Try adding ',' | ';' | '}' | '[' | ']' | '{' ".to_string(),
        "IP0014".to_string(),
        token.clone(),
        None,
      )),
      ParserDiagnosticError::UnexpectedKeyword(token) => {
        let message = format!("Try deleting '{}' or adding a '_': '{}_'", token.lexeme, token.lexeme);

        Some(DiagnosticReport::new(
          DiagnosticLevel::Hint,
          message,
          "IP0016".to_string(),
          token.clone(),
          None,
        ))
      },
      _ => None,
    };

    DiagnosticReport::new(
      DiagnosticLevel::Error,
      self.error.to_string(),
      self.error.code(),
      (&self.error).into(),
      hint,
    )
  }
}
