use token::token::Token;

pub enum DiagnosticLevel {
  Info,
  Warning,
  Error,
  Hint
}

pub struct DiagnosticReport {
  pub message: String,
  pub token: Box<Token>,
  pub token_line: Vec<Token>,
  pub level: DiagnosticLevel,
  pub hint: Option<Box<DiagnosticReport>>,
  pub error_code: String, // Format: I{module}{number}
}

impl DiagnosticReport {
  pub fn new(
    message: String,
    token: Box<Token>,
    token_line: Vec<Token>,
    level: DiagnosticLevel,
    hint: Option<Box<DiagnosticReport>>,
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
