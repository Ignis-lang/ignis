pub mod diagnostic_report;
pub mod message;

use diagnostic_report::DiagnosticReport;
use colored::*;

/**
 *{level}[{error_code}]: {message}
 *      --> {file_path}:{line}:{column}
 *{line} |       {prev_code}
 *       |
 *{line} |       {code}
 *       |
 *{line} |       {after_code}
 * */
pub struct Diagnostic {}

impl Diagnostic {
  pub fn new() -> Self {
    Self {}
  }

  pub fn report(
    &self,
    diagnostic: &Vec<DiagnosticReport>,
  ) {
    for report in diagnostic {
      self.print(&report);
    }
  }

  pub fn print(
    &self,
    diagnostic: &DiagnosticReport,
  ) {
    self.print_header(diagnostic);
    self.print_body(diagnostic);

    if let Some(diagnostic_report) = &diagnostic.hint {
      self.print_hint(diagnostic_report);
    }

    println!();
  }

  fn print_hint(
    &self,
    diagnostic: &DiagnosticReport,
  ) {
    self.print_header(diagnostic);
  }

  fn print_header(
    &self,
    diagnostic: &DiagnosticReport,
  ) {
    let message = diagnostic.message.bold();
    let code = diagnostic.error_code.bold();

    match diagnostic.level {
      diagnostic_report::DiagnosticLevel::Info => {
        println!("{}[{}]: {}", "Info".blue().bold(), code.blue(), message)
      },
      diagnostic_report::DiagnosticLevel::Warning => {
        println!("{}[{}]: {}", "Warning".yellow().bold(), code.yellow(), message)
      },
      diagnostic_report::DiagnosticLevel::Error => {
        println!("{}[{}]: {}", "Error".red().bold(), code.red().bold(), message)
      },
      diagnostic_report::DiagnosticLevel::Hint => {
        println!("{}[{}]: {}", "Hint".cyan().bold(), code.cyan(), message)
      },
    }
  }

  fn print_body(
    &self,
    diagnostic: &DiagnosticReport,
  ) {
    let pipe = "|".blue().bold();
    let code = std::fs::read_to_string(diagnostic.token.file_name.clone());

    if code.is_err() {
      println!("{:?}", diagnostic.token);
      panic!("File not found");
    }

    let code = code.unwrap();

    let lines = code.lines().collect::<Vec<&str>>();

    println!(
      "{:2}{} {}:{}:{}",
      "",
      "-->".blue().bold(),
      diagnostic.token.file_name.bold(),
      diagnostic.token.line.to_string().bold(),
      diagnostic.token.column.to_string().bold(),
    );

    if diagnostic.token.line > 2 {
      println!("{:3}{:3}", "", pipe);
      println!(
        "{:3}{:3}{}",
        (diagnostic.token.line - 1).to_string().blue().bold(),
        pipe,
        lines[diagnostic.token.line - 2].dimmed()
      );
    }

    if diagnostic.token.line > 1 {
      println!("{:3}{:3}", "", pipe);
      println!(
        "{:3}{:3}{}",
        diagnostic.token.line.to_string().blue().bold(),
        pipe,
        lines[diagnostic.token.line - 1]
      );
    }

    if diagnostic.token.line < lines.len() {
      println!("{:3}{:3}", "", pipe);
      println!(
        "{:3}{:3}{}",
        (diagnostic.token.line + 1).to_string().blue().bold(),
        pipe,
        lines[diagnostic.token.line].dimmed()
      );
    }
  }
}

impl Default for Diagnostic {
  fn default() -> Self {
    Self::new()
  }
}

