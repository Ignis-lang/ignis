use diagnostic_report::DiagnosticReport;

pub struct Diagnostic {}

/**
 * {level}[{error_code}]: {message}
 *  --> {file_path}:{line}:{column}
 *       |
 *{line} |       {code}
 *       |
 * */
impl Diagnostic {
  pub fn new() -> Self {
    Self {}
  }

  pub fn report(&self, diagnostic: Vec<DiagnosticReport>) {
    for report in diagnostic {
      self.print(&report);
    }
  }

  pub fn print(&self, diagnostic: &DiagnosticReport) {
    self.print_header(diagnostic);
    self.print_body(diagnostic)
  }

  fn print_header(&self, diagnostic: &DiagnosticReport) {
    match diagnostic.level {
      diagnostic_report::DiagnosticLevel::Info => {
        println!(
          "\x1b[1;34mInfo[{}]: {}\x1b[0m",
          diagnostic.error_code, diagnostic.message
        )
      }
      diagnostic_report::DiagnosticLevel::Warning => {
        println!(
          "\x1b[1;33mWarning[{}]: {}\x1b[0m",
          diagnostic.error_code, diagnostic.message
        )
      }
      diagnostic_report::DiagnosticLevel::Error => {
        println!(
          "\x1b[1;31mError[{}]: {}\x1b[0m",
          diagnostic.error_code, diagnostic.message
        )
      }
      diagnostic_report::DiagnosticLevel::Hint => {
        println!(
          "\x1b[1;32mHint[{}]: {}\x1b[0m",
          diagnostic.error_code, diagnostic.message
        )
      }
    }
  }

  fn print_body(&self, diagnostic: &DiagnosticReport) {
    println!("{:4}|", "");
    println!(
      "{:4} |{:4} {}",
      "",
      "",
      diagnostic.token.span.line,
      diagnostic
        .token_line
        .iter()
        .map(|token| token.span.literal)
        .collect::<String>()
    );
    println!("{:4}|", "");
  }
}
