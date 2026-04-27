use std::path::Path;

use crate::{FormatFile, layout::layout_file, printer::print_file};
use crate::safety::{validate_formatted_output, validate_idempotence};
use crate::{FormatterConfig, FormatterConfigError};

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct FormatOptions {
  pub check: bool,
  pub config: FormatterConfig,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FormatOutcome {
  pub changed: bool,
  pub formatted: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FormatError {
  Lex { diagnostics: Vec<String> },
  Parse { diagnostics: Vec<String> },
  Model { message: String },
  Io { message: String },
  Config { message: String },
  Safety { message: String },
  Unsupported { message: String },
}

impl std::fmt::Display for FormatError {
  fn fmt(
    &self,
    f: &mut std::fmt::Formatter<'_>,
  ) -> std::fmt::Result {
    match self {
      Self::Lex { diagnostics } => write!(f, "lexer failed: {}", diagnostics.join("; ")),
      Self::Parse { diagnostics } => write!(f, "parser failed: {}", diagnostics.join("; ")),
      Self::Model { message } => write!(f, "formatter model failed: {message}"),
      Self::Io { message } => write!(f, "formatter I/O failed: {message}"),
      Self::Config { message } => write!(f, "formatter configuration failed: {message}"),
      Self::Safety { message } => write!(f, "formatter safety validation failed: {message}"),
      Self::Unsupported { message } => write!(f, "formatter unsupported syntax: {message}"),
    }
  }
}

impl std::error::Error for FormatError {}

impl From<FormatterConfigError> for FormatError {
  fn from(value: FormatterConfigError) -> Self {
    Self::Config {
      message: value.to_string(),
    }
  }
}

pub fn format_text(
  source: &str,
  options: &FormatOptions,
) -> Result<String, FormatError> {
  let formatted = format_text_unchecked(source, &options.config)?;
  validate_formatted_output(source, &formatted, options.config.sort_imports)?;
  validate_idempotence(&formatted, |formatted| format_text_unchecked(formatted, &options.config))?;

  Ok(formatted)
}

fn format_text_unchecked(
  source: &str,
  config: &FormatterConfig,
) -> Result<String, FormatError> {
  let file = FormatFile::from_source(source)?;
  let layout = layout_file(&file, config)?;
  print_file(&layout, config)
}

pub fn format_file(
  path: &Path,
  options: &FormatOptions,
) -> Result<FormatOutcome, FormatError> {
  let source = std::fs::read_to_string(path).map_err(|error| FormatError::Io {
    message: error.to_string(),
  })?;

  let formatted = format_text(&source, options)?;
  let changed = formatted != source;

  if changed && !options.check {
    std::fs::write(path, &formatted).map_err(|error| FormatError::Io {
      message: error.to_string(),
    })?;
  }

  Ok(FormatOutcome { changed, formatted })
}

#[cfg(test)]
mod tests {
  use std::fs;
  use std::path::PathBuf;

  use super::{FormatOptions, format_text_unchecked};

  fn workspace_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
      .join("../..")
      .canonicalize()
      .expect("workspace root")
  }

  fn extract_slice(
    source: &str,
    start_marker: &str,
    end_marker: &str,
  ) -> String {
    let start = source.find(start_marker).expect("slice start");
    let end = source[start..]
      .find(end_marker)
      .map(|offset| start + offset)
      .expect("slice end");
    source[start..end].to_string()
  }

  fn wrap_record_body(body: &str) -> String {
    format!("export record HeapAllocator {{\n{body}\n}}\n")
  }

  #[test]
  fn unchecked_formatter_is_idempotent_for_supported_heap_allocator_regions() {
    let source = fs::read_to_string(workspace_root().join("example/allocator/src/heap_allocator.ign"))
      .expect("read heap allocator source");

    let record_comments = wrap_record_body(&extract_slice(
      &source,
      "/// Pointer to the first (oldest) block in the linked list.",
      "/// Creates a new, empty `HeapAllocator` with the default search mode (FirstFit).",
    ));

    let first_pass = format_text_unchecked(&record_comments, &FormatOptions::default().config)
      .expect("first pass format for record comments");
    let second_pass = format_text_unchecked(&first_pass, &FormatOptions::default().config)
      .expect("second pass format for record comments");

    if first_pass != second_pass {
      let first_lines = first_pass.lines().collect::<Vec<_>>();
      let second_lines = second_pass.lines().collect::<Vec<_>>();
      let diff_index = first_lines
        .iter()
        .zip(second_lines.iter())
        .position(|(left, right)| left != right)
        .unwrap_or(first_lines.len().min(second_lines.len()));

      panic!(
        "record-comments first diff at line {}\nfirst: {:?}\nsecond: {:?}",
        diff_index + 1,
        first_lines.get(diff_index),
        second_lines.get(diff_index)
      );
    }
  }

  #[test]
  fn unchecked_formatter_formats_heap_allocator_method_slice_with_owned_comment_preservation() {
    let source = fs::read_to_string(workspace_root().join("example/allocator/src/heap_allocator.ign"))
      .expect("read heap allocator source");

    let set_search_mode = wrap_record_body(&extract_slice(
      &source,
      "public setSearchMode(&mut self, mode: SearchMode): void {",
      "/// Searches the block list for a free block of sufficient size.",
    ));

    let formatted = format_text_unchecked(&set_search_mode, &FormatOptions::default().config)
      .expect("setSearchMode slice should now format successfully");

    assert_eq!(
      formatted,
      "export record HeapAllocator {\n  public setSearchMode(&mut self, mode: SearchMode): void {\n    self.searchMode = mode;\n\n    // Reset lastSearch when changing modes to avoid stale pointers\n    if (mode != SearchMode::NextFit) {\n      self.lastSearch = null;\n    }\n  }\n}\n"
    );
  }

  #[test]
  fn format_text_does_not_return_unsupported_for_valid_code() {
    let sources = [
      "function main(): void { return; }\n",
      "export const MY_CONST: i32 = 42;\n",
      "record Empty {}\n",
      "enum Unit { ONLY, }\n",
      "function pipe(x: i32): i32 { return x |> identity; }\n",
    ];

    for source in &sources {
      let result = super::format_text(source, &FormatOptions::default());
      match &result {
        Ok(_) => {},
        Err(super::FormatError::Unsupported { message }) => {
          panic!(
            "format_text returned FormatError::Unsupported for valid code:\n  source: {source:?}\n  message: {message}"
          );
        },
        Err(other) => {
          panic!(
            "format_text failed for valid code with non-Unsupported error:\n  source: {source:?}\n  error: {other}"
          );
        },
      }
    }
  }

  #[test]
  fn unchecked_formatter_preserves_namespace_leading_doc_comment_shape() {
    let source = "namespace Fs::Sys {\n  /// File metadata returned by `stat` and `fstat`.\n  ///\n  /// Fields correspond to POSIX `struct stat` members.\n  record Stat {\n    public size: i64;\n  }\n}\n";

    let formatted = format_text_unchecked(source, &FormatOptions::default().config)
      .expect("namespace leading doc comment should format unchecked");

    assert_eq!(formatted, source);
  }
}
