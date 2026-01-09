pub mod diagnostic_report;
pub mod message;

use diagnostic_report::{Diagnostic, Severity};
use ignis_type::file::SourceMap;
use colored::*;

pub fn render(
  diag: &Diagnostic,
  sm: &SourceMap,
) {
  print_header(diag);
  print_body(diag, sm);

  for note in &diag.notes {
    println!("  {} {}", "note:".cyan().bold(), note);
  }

  println!();
}

pub fn render_batch(
  diagnostics: &[Diagnostic],
  sm: &SourceMap,
) {
  for diag in diagnostics {
    render(diag, sm);
  }
}

fn print_header(diag: &Diagnostic) {
  let message = diag.message.bold();
  let code = diag.error_code.bold();

  match diag.severity {
    Severity::Info => {
      println!("{}[{}]: {}", "Info".blue().bold(), code.blue(), message)
    },
    Severity::Warning => {
      println!("{}[{}]: {}", "Warning".yellow().bold(), code.yellow(), message)
    },
    Severity::Error => {
      println!("{}[{}]: {}", "Error".red().bold(), code.red().bold(), message)
    },
    Severity::Hint => {
      println!("{}[{}]: {}", "Hint".cyan().bold(), code.cyan(), message)
    },
  }
}

fn print_body(
  diag: &Diagnostic,
  sm: &SourceMap,
) {
  let file = sm.get(&diag.primary_span.file);
  let (line, col): (u32, u32) = sm.line_col(&diag.primary_span.file, diag.primary_span.start);
  let (end_line, end_col): (u32, u32) = sm.line_col(&diag.primary_span.file, diag.primary_span.end);

  println!(
    "{:2}{} {}:{}:{}",
    "",
    "-->".blue().bold(),
    file.path.display().to_string().bold(),
    line.to_string().bold(),
    col.to_string().bold(),
  );

  let pipe = "|".blue().bold();
  let lines: Vec<&str> = file.text.lines().collect();
  let line_idx = (line as usize).saturating_sub(1);

  // Mostrar línea anterior si existe
  if line > 1 && line_idx > 0 {
    println!("{:3}{:3}", "", pipe);
    println!(
      "{:3}{:3}{}",
      (line - 1).to_string().blue().bold(),
      pipe,
      lines.get(line_idx.saturating_sub(1)).unwrap_or(&"").dimmed()
    );
  }

  // Línea con el error
  println!("{:3}{:3}", "", pipe);
  if let Some(error_line) = lines.get(line_idx) {
    println!("{:3}{:3}{}", line.to_string().blue().bold(), pipe, error_line);

    // Caret apuntando al error
    let span_len = if line == end_line {
      (end_col.saturating_sub(col)).max(1) as usize
    } else {
      error_line.len().saturating_sub(col as usize).max(1)
    };

    let caret = "^".repeat(span_len).red().bold();
    println!("{:3}{:3}{}{}", "", pipe, " ".repeat(col as usize), caret);
  }

  // Mostrar línea siguiente si existe
  if line_idx + 1 < lines.len() {
    println!("{:3}{:3}", "", pipe);
    println!(
      "{:3}{:3}{}",
      (line + 1).to_string().blue().bold(),
      pipe,
      lines.get(line_idx + 1).unwrap_or(&"").dimmed()
    );
  }

  // Labels adicionales
  for label in &diag.labels {
    println!();
    let (label_line, label_col): (u32, u32) = sm.line_col(&label.span.file, label.span.start);
    println!("  {} {}", "label:".yellow().bold(), label.message);

    if let Some(label_line_text) = lines.get((label_line as usize).saturating_sub(1)) {
      println!("{:3}{:3}{}", label_line.to_string().blue().bold(), pipe, label_line_text);

      let label_caret = "^".repeat(1).yellow().bold();
      println!("{:3}{:3}{}{}", "", pipe, " ".repeat(label_col as usize), label_caret);
    }
  }
}
