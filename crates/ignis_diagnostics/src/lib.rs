pub mod diagnostic_report;
pub mod message;

use diagnostic_report::{Diagnostic, Severity};
use ignis_type::file::SourceMap;
use colored::*;

#[derive(Copy, Clone)]
enum Target {
  Stdout,
  Stderr,
}

macro_rules! emit {
  ($target:expr, $($arg:tt)*) => {
    match $target {
      Target::Stdout => println!($($arg)*),
      Target::Stderr => eprintln!($($arg)*),
    }
  };
}

pub fn render(
  diag: &Diagnostic,
  sm: &SourceMap,
) {
  render_to(diag, sm, Target::Stdout);
}

pub fn render_batch(
  diagnostics: &[Diagnostic],
  sm: &SourceMap,
) {
  for diag in diagnostics {
    render(diag, sm);
  }
}

/// Same as `render`, but writes to stderr instead of stdout.
///
/// Used for diagnostics produced before the normal compile pipeline exists
/// (such as config-file warnings), which the CLI reports on stderr like every
/// other early, pre-pipeline message.
pub fn render_to_stderr(
  diag: &Diagnostic,
  sm: &SourceMap,
) {
  render_to(diag, sm, Target::Stderr);
}

/// Same as `render_batch`, but writes to stderr instead of stdout.
pub fn render_batch_to_stderr(
  diagnostics: &[Diagnostic],
  sm: &SourceMap,
) {
  for diag in diagnostics {
    render_to_stderr(diag, sm);
  }
}

fn render_to(
  diag: &Diagnostic,
  sm: &SourceMap,
  target: Target,
) {
  print_header(diag, target);
  print_body(diag, sm, target);

  for note in &diag.notes {
    emit!(target, "  {} {}", "note:".cyan().bold(), note);
  }

  emit!(target, "");
}

fn print_header(
  diag: &Diagnostic,
  target: Target,
) {
  let message = diag.message.bold();
  let code = diag.error_code.bold();

  match diag.severity {
    Severity::Info => {
      emit!(target, "{}[{}]: {}", "Info".blue().bold(), code.blue(), message)
    },
    Severity::Warning => {
      emit!(target, "{}[{}]: {}", "Warning".yellow().bold(), code.yellow(), message)
    },
    Severity::Error => {
      emit!(target, "{}[{}]: {}", "Error".red().bold(), code.red().bold(), message)
    },
    Severity::Hint => {
      emit!(target, "{}[{}]: {}", "Hint".cyan().bold(), code.cyan(), message)
    },
  }
}

fn print_body(
  diag: &Diagnostic,
  sm: &SourceMap,
  target: Target,
) {
  let file = sm.get(&diag.primary_span.file);
  let (line, col): (u32, u32) = sm.display_line_col(&diag.primary_span.file, diag.primary_span.start);
  let (end_line, end_col): (u32, u32) = sm.display_line_col(&diag.primary_span.file, diag.primary_span.end);

  emit!(
    target,
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
    emit!(target, "{:3}{:3}", "", pipe);
    emit!(
      target,
      "{:3}{:3}{}",
      (line - 1).to_string().blue().bold(),
      pipe,
      lines.get(line_idx.saturating_sub(1)).unwrap_or(&"").dimmed()
    );
  }

  // Línea con el error
  emit!(target, "{:3}{:3}", "", pipe);
  if let Some(error_line) = lines.get(line_idx) {
    emit!(target, "{:3}{:3}{}", line.to_string().blue().bold(), pipe, error_line);

    // Caret apuntando al error
    let span_len = if line == end_line {
      (end_col.saturating_sub(col)).max(1) as usize
    } else {
      error_line.len().saturating_sub(col as usize).max(1)
    };

    let caret = "^".repeat(span_len).red().bold();
    emit!(target, "{:3}{:3}{}{}", "", pipe, " ".repeat(col as usize), caret);
  }

  // Mostrar línea siguiente si existe
  if line_idx + 1 < lines.len() {
    emit!(target, "{:3}{:3}", "", pipe);
    emit!(
      target,
      "{:3}{:3}{}",
      (line + 1).to_string().blue().bold(),
      pipe,
      lines.get(line_idx + 1).unwrap_or(&"").dimmed()
    );
  }

  // Labels adicionales
  for label in &diag.labels {
    emit!(target, "");
    let (label_line, label_col): (u32, u32) = sm.display_line_col(&label.span.file, label.span.start);
    emit!(target, "  {} {}", "label:".yellow().bold(), label.message);

    if let Some(label_line_text) = lines.get((label_line as usize).saturating_sub(1)) {
      emit!(
        target,
        "{:3}{:3}{}",
        label_line.to_string().blue().bold(),
        pipe,
        label_line_text
      );

      let label_caret = "^".to_string().yellow().bold();
      emit!(target, "{:3}{:3}{}{}", "", pipe, " ".repeat(label_col as usize), label_caret);
    }
  }
}
