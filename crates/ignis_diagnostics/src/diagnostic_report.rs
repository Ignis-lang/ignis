use ignis_type::span::Span;

#[derive(Debug, Clone, PartialEq)]
pub enum Severity {
  Info,
  Warning,
  Error,
  Hint,
}

#[derive(Debug, Clone)]
pub struct Label {
  pub span: Span,
  pub message: String,
}

#[derive(Debug, Clone)]
pub struct Diagnostic {
  pub severity: Severity,
  pub message: String,
  pub error_code: String,
  pub primary_span: Span,
  pub labels: Vec<Label>,
  pub notes: Vec<String>,
}

impl Diagnostic {
  pub fn new(
    severity: Severity,
    message: String,
    error_code: String,
    primary_span: Span,
  ) -> Self {
    Self {
      severity,
      message,
      error_code,
      primary_span,
      labels: Vec::new(),
      notes: Vec::new(),
    }
  }

  pub fn with_label(
    mut self,
    span: Span,
    message: String,
  ) -> Self {
    self.labels.push(Label { span, message });
    self
  }

  pub fn with_note(
    mut self,
    note: String,
  ) -> Self {
    self.notes.push(note);
    self
  }
}

pub type DiagnosticLevel = Severity;
pub type DiagnosticReport = Diagnostic;
