use diagnostic_report::{DiagnosticReport, DiagnosticLevel};
use enums::token_type::TokenType;
use token::token::Token;

pub enum ParserDiagnosticError {
  ExpectedExpression(Token),
  ExpectedToken(TokenType, Token),
  ExpectedVariableName(Token),
  ExpectedReturnTypeAfterFunction(Token),
  ExpectedAfterExpression(Box<TokenType>, Box<Token>, Box<Token>),
  ExpectedExpressionAfter(Token),
  UnexpectedToken(TokenType, Token),
  InvalidAssignmentTarget(Token),
  ExpectedTypeAfterVariable(Token),
  InvalidNumberOfArguments(usize, usize, Token),
}

pub struct ParserDiagnostic {
  pub error: ParserDiagnosticError,
  pub token_line: Vec<Token>,
}

impl ParserDiagnostic {
  pub fn new(error: ParserDiagnosticError, token_line: Vec<Token>) -> Self {
    Self { error, token_line }
  }

  pub fn report_diagnostic(&self) -> DiagnosticReport {
    match &self.error {
      ParserDiagnosticError::ExpectedExpression(token) => DiagnosticReport::new(
        format!("Expected expression after '{}'", token.span.literal),
        Box::new(token.clone()),
        self.token_line.clone(),
        DiagnosticLevel::Error,
        None,
        "IP0001".to_string(),
      ),
      ParserDiagnosticError::ExpectedToken(expected_token, token) => DiagnosticReport::new(
        format!(
          "Expected '{}' after '{}'",
          expected_token, token.span.literal
        ),
        Box::new(token.clone()),
        self.token_line.clone(),
        DiagnosticLevel::Error,
        None,
        "IP0002".to_string(),
      ),
      ParserDiagnosticError::ExpectedVariableName(token) => DiagnosticReport::new(
        format!("Expected variable name after '{}'", token.span.literal),
        Box::new(token.clone()),
        self.token_line.clone(),
        DiagnosticLevel::Error,
        None,
        "IP0003".to_string(),
      ),
      ParserDiagnosticError::ExpectedReturnTypeAfterFunction(token) => DiagnosticReport::new(
        format!("Expected return type after '{}'", token.span.literal),
        Box::new(token.clone()),
        self.token_line.clone(),
        DiagnosticLevel::Error,
        None,
        "IP0004".to_string(),
      ),
      ParserDiagnosticError::ExpectedAfterExpression(expected_token_type, token, token2) => {
        DiagnosticReport::new(
          format!(
            "Expected '{}' after '{}' in expression",
            expected_token_type, token.span.literal
          ),
          token.clone(),
          self.token_line.clone(),
          DiagnosticLevel::Error,
          None,
          "IP0005".to_string(),
        )
      }
      ParserDiagnosticError::ExpectedExpressionAfter(token) => DiagnosticReport::new(
        format!("Expected expression after '{}'", token.span.literal),
        Box::new(token.clone()),
        self.token_line.clone(),
        DiagnosticLevel::Error,
        None,
        "IP0006".to_string(),
      ),
      ParserDiagnosticError::UnexpectedToken(kind, token) => DiagnosticReport::new(
        format!("Unexpected token '{}' after '{}'", kind, token.span.literal),
        Box::new(token.clone()),
        self.token_line.clone(),
        DiagnosticLevel::Error,
        None,
        "IP0007".to_string(),
      ),
      ParserDiagnosticError::InvalidAssignmentTarget(token) => DiagnosticReport::new(
        "Invalid assignment target".to_string(),
        Box::new(token.clone()),
        self.token_line.clone(),
        DiagnosticLevel::Error,
        None,
        "IP0008".to_string(),
      ),
      ParserDiagnosticError::ExpectedTypeAfterVariable(token) => DiagnosticReport::new(
        format!("Expected type after '{}'", token.span.literal),
        Box::new(token.clone()),
        self.token_line.clone(),
        DiagnosticLevel::Error,
        None,
        "IP0009".to_string(),
      ),
      ParserDiagnosticError::InvalidNumberOfArguments(max, num, token) => DiagnosticReport::new(
        format!("Expected {} arguments, got {}", max, num),
        Box::new(token.clone()),
        self.token_line.clone(),
        DiagnosticLevel::Error,
        None,
        "IP0010".to_string(),
      ),
    }
  }
}
