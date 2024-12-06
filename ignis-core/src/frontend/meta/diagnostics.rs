use ignis_data_type::DataType;
use ignis_token::{token::Token, token_types::TokenType};

use crate::diagnostics::diagnostic_report::{DiagnosticLevel, DiagnosticReport};

#[derive(Debug, Clone)]
pub enum MetaDiagnosticError {
  UndefinedMeta(Token),
  InvalidMetaEntity(TokenType, Token),
  MissingArgument(Token),
  InvalidArgumentType(DataType, Token),
  InvalidNumberOfArguments(usize, usize, Token),
  TypeMismatch(DataType, DataType, Token),
  InvalidProperty(Token),
}

impl std::fmt::Display for MetaDiagnosticError {
  fn fmt(
    &self,
    f: &mut std::fmt::Formatter<'_>,
  ) -> std::fmt::Result {
    match self {
      MetaDiagnosticError::UndefinedMeta(token) => write!(f, "Undefined meta '{}'", token.lexeme),
      MetaDiagnosticError::InvalidMetaEntity(token, _) => write!(f, "Invalid meta entity '{}'", token),
      MetaDiagnosticError::MissingArgument(token) => write!(f, "Missing argument for meta '{}'", token.lexeme),
      MetaDiagnosticError::InvalidArgumentType(type_, token) => {
        write!(f, "Invalid argument type for meta '{}', expected '{}'", token.lexeme, type_)
      },
      MetaDiagnosticError::InvalidNumberOfArguments(max, num, token) => write!(
        f,
        "Invalid number of arguments for meta '{}', expected {} got {}",
        token.lexeme, max, num
      ),
      MetaDiagnosticError::TypeMismatch(type_, expected, token) => write!(
        f,
        "Type mismatch for meta '{}', expected '{}' got '{}'",
        token.lexeme, expected, type_
      ),
      MetaDiagnosticError::InvalidProperty(token) => write!(f, "Invalid property '{}'", token.lexeme),
    }
  }
}

impl From<&MetaDiagnosticError> for Token {
  fn from(value: &MetaDiagnosticError) -> Self {
    match value {
      MetaDiagnosticError::UndefinedMeta(token)
      | MetaDiagnosticError::InvalidMetaEntity(_, token)
      | MetaDiagnosticError::MissingArgument(token)
      | MetaDiagnosticError::InvalidArgumentType(_, token)
      | MetaDiagnosticError::InvalidNumberOfArguments(_, _, token)
      | MetaDiagnosticError::TypeMismatch(_, _, token) 
      | MetaDiagnosticError::InvalidProperty(token) => token.clone(),

    }
  }
}

impl MetaDiagnosticError {
  fn code(&self) -> String {
    match self {
      MetaDiagnosticError::UndefinedMeta(_) => "IM0001".to_string(),
      MetaDiagnosticError::InvalidMetaEntity(_, _) => "IM0002".to_string(),
      MetaDiagnosticError::MissingArgument(_) => "IM0003".to_string(),
      MetaDiagnosticError::InvalidArgumentType(_, _) => "IM0004".to_string(),
      MetaDiagnosticError::InvalidNumberOfArguments(_, _, _) => "IM0005".to_string(),
      MetaDiagnosticError::TypeMismatch(_, _, _) => "IM0006".to_string(),
      MetaDiagnosticError::InvalidProperty(_) => "IM0007".to_string(),
    }
  }
}

#[derive(Debug, Clone)]
pub struct MetaDiagnostic {
  pub error: MetaDiagnosticError,
}

impl MetaDiagnostic {
  pub fn new(error: MetaDiagnosticError) -> Self {
    Self { error }
  }

  pub fn report_diagnostic(&self) -> DiagnosticReport {
    let hint = match &self.error {
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
