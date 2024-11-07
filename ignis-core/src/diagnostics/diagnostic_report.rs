use ignis_token::token::Token;

#[derive(Debug, Clone)]
pub struct DiagnosticReport {
  pub level: DiagnosticLevel,
  pub message: String,
  pub error_code: String,
  pub token: Token,
  pub hint: Option<Box<DiagnosticReport>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum DiagnosticLevel {
  Info,
  Warning,
  Error,
  Hint,
}

impl DiagnosticReport {
  pub fn new(
    level: DiagnosticLevel,
    message: String,
    error_code: String,
    token: Token,
    hint: Option<DiagnosticReport>,
  ) -> Self {
    Self {
      level,
      message,
      error_code,
      token,
      hint: hint.map(|x| Box::new(x)),
    }
  }
}
