use token::token::Token;

pub enum DiagnosticLevel {
  Info,
  Warning,
  Error,
}

pub struct DiagnosticReport {
  pub message: String,
  pub token: Box<Token>,
  pub token_line: Vec<Token>,
  pub level: DiagnosticLevel,
  pub hint: Option<DiagnosticReport>,
  pub error_code: String, /// Format: I{module}{number}
}

impl DiagnosticReport {
  pub fn new(
    message: String,
    token: Box<Token>,
    token_line: Vec<Token>,
    level: DiagnosticLevel,
    hint: Option<DiagnosticReport>,
    error_code: String,
  ) -> Self {
    Self {
      message,
      token,
      token_line,
      level,
      hint,
      error_code,
    }
  }
}
