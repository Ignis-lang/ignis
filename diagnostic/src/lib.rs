use diagnostic_report::DiagnosticReport;
use colored::*;

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
    self.print_body(diagnostic);
    println!();
  }

  fn print_header(&self, diagnostic: &DiagnosticReport) {
    match diagnostic.level {
      diagnostic_report::DiagnosticLevel::Info => {
        println!(
          "{}[{}]: {}",
          "Info".blue().bold(),
          diagnostic.error_code.blue(),
          diagnostic.message
        )
      }
      diagnostic_report::DiagnosticLevel::Warning => {
        println!(
          "{}[{}]: {}",
          "Warning".yellow().bold(),
          diagnostic.error_code.yellow(),
          diagnostic.message
        )
      }
      diagnostic_report::DiagnosticLevel::Error => {
        println!(
          "{}[{}]: {}",
          "Error".red().bold(),
          diagnostic.error_code.red(),
          diagnostic.message
        )
      }
      diagnostic_report::DiagnosticLevel::Hint => {
        println!(
          "{}[{}]: {}",
          "Hint".cyan().bold(),
          diagnostic.error_code.cyan(),
          diagnostic.message
        )
      }
    }
  }

  fn print_body(&self, diagnostic: &DiagnosticReport) {
    println!("{:3}--> {}", "", diagnostic.token.span.file.clone());
    println!("{:4}|", (diagnostic.token.span.line - 1).to_string().blue());
    println!(
      "{:4}|{:4} {}",
      diagnostic.token.span.line.to_string().blue(),
      "",
      diagnostic
        .token_line
        .iter()
        .map(|token| token.span.literal.clone())
        .collect::<Vec<String>>().join(" ")
    );
    println!("{:4}|", (diagnostic.token.span.line - 2).to_string().blue());
  }
}

impl Default for Diagnostic {
    fn default() -> Self {
        Self::new()
    }
}
