//! Conversion utilities between Ignis types and LSP types.
//!
//! Handles proper UTF-16 column calculation as required by the LSP spec.

use ignis_diagnostics::diagnostic_report::{Diagnostic, Severity};
use ignis_type::span::Span;
use ignis_type::BytePosition;
use tower_lsp::lsp_types::{self, DiagnosticSeverity, NumberOrString, Position, Range};

/// Index for fast line/column lookups with proper UTF-16 support.
///
/// LSP positions use 0-based line numbers and UTF-16 code unit offsets
/// for the character position. This struct caches line start byte offsets
/// and provides conversion from byte positions to LSP positions.
pub struct LineIndex {
  /// Byte offsets of the start of each line (0-indexed).
  line_starts: Vec<u32>,

  /// The source text.
  text: String,
}

impl LineIndex {
  /// Create a new line index from source text.
  pub fn new(text: String) -> Self {
    let mut line_starts = vec![0u32];

    for (i, byte) in text.as_bytes().iter().enumerate() {
      if *byte == b'\n' {
        line_starts.push((i + 1) as u32);
      }
    }

    Self { line_starts, text }
  }

  /// Find the 0-based line number for a byte position.
  fn line_of(
    &self,
    pos: BytePosition,
  ) -> u32 {
    let mut lo = 0usize;
    let mut hi = self.line_starts.len();

    while lo + 1 < hi {
      let mid = (lo + hi) / 2;
      if self.line_starts[mid] <= pos.0 {
        lo = mid;
      } else {
        hi = mid;
      }
    }

    lo as u32
  }

  /// Convert a byte position to (line, utf16_column), both 0-based.
  pub fn line_col_utf16(
    &self,
    pos: BytePosition,
  ) -> (u32, u32) {
    let line = self.line_of(pos);
    let line_start = self.line_starts[line as usize];

    // Slice from line start to the position
    let start = line_start as usize;
    let end = (pos.0 as usize).min(self.text.len());

    if start >= end {
      return (line, 0);
    }

    let slice = &self.text[start..end];

    // Count UTF-16 code units
    let utf16_col = slice.encode_utf16().count() as u32;

    (line, utf16_col)
  }

  /// Convert an Ignis Span to an LSP Range.
  pub fn span_to_range(
    &self,
    span: &Span,
  ) -> Range {
    let (start_line, start_col) = self.line_col_utf16(span.start);
    let (end_line, end_col) = self.line_col_utf16(span.end);

    Range {
      start: Position {
        line: start_line,
        character: start_col,
      },
      end: Position {
        line: end_line,
        character: end_col,
      },
    }
  }

  /// Convert an LSP Position (line, utf16_column) to a byte offset.
  pub fn offset(
    &self,
    position: Position,
  ) -> u32 {
    let line = position.line as usize;
    let utf16_col = position.character as usize;

    // Get the byte offset of the start of the line
    let line_start = if line < self.line_starts.len() {
      self.line_starts[line] as usize
    } else {
      // Position is past the end of the file
      return self.text.len() as u32;
    };

    // Find the end of the line
    let line_end = if line + 1 < self.line_starts.len() {
      self.line_starts[line + 1] as usize
    } else {
      self.text.len()
    };

    // Get the line text
    let line_text = &self.text[line_start..line_end];

    // Convert UTF-16 column to byte offset within the line
    let mut byte_offset = 0;
    let mut utf16_count = 0;

    for ch in line_text.chars() {
      if utf16_count >= utf16_col {
        break;
      }

      utf16_count += ch.len_utf16();
      byte_offset += ch.len_utf8();
    }

    (line_start + byte_offset) as u32
  }
}

/// Convert an Ignis Diagnostic to an LSP Diagnostic.
pub fn convert_diagnostic(
  diag: &Diagnostic,
  line_index: &LineIndex,
) -> lsp_types::Diagnostic {
  let severity = match diag.severity {
    Severity::Error => Some(DiagnosticSeverity::ERROR),
    Severity::Warning => Some(DiagnosticSeverity::WARNING),
    Severity::Info => Some(DiagnosticSeverity::INFORMATION),
    Severity::Hint => Some(DiagnosticSeverity::HINT),
  };

  let range = line_index.span_to_range(&diag.primary_span);

  // Build related information from labels
  let related_information = if diag.labels.is_empty() {
    None
  } else {
    Some(
      diag
        .labels
        .iter()
        .map(|label| lsp_types::DiagnosticRelatedInformation {
          location: lsp_types::Location {
            // We don't have the URI here, caller should fill it in
            // For now, use a placeholder that won't be displayed
            uri: lsp_types::Url::parse("file:///").unwrap(),
            range: line_index.span_to_range(&label.span),
          },
          message: label.message.clone(),
        })
        .collect(),
    )
  };

  lsp_types::Diagnostic {
    range,
    severity,
    code: Some(NumberOrString::String(diag.error_code.clone())),
    code_description: None,
    source: Some("ignis".to_string()),
    message: diag.message.clone(),
    related_information,
    tags: None,
    data: None,
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_line_index_ascii() {
    let text = "hello\nworld\n";
    let idx = LineIndex::new(text.to_string());

    // Line 0: "hello\n" -> bytes 0..6
    assert_eq!(idx.line_col_utf16(BytePosition(0)), (0, 0));
    assert_eq!(idx.line_col_utf16(BytePosition(3)), (0, 3));
    assert_eq!(idx.line_col_utf16(BytePosition(5)), (0, 5));

    // Line 1: "world\n" -> bytes 6..12
    assert_eq!(idx.line_col_utf16(BytePosition(6)), (1, 0));
    assert_eq!(idx.line_col_utf16(BytePosition(9)), (1, 3));
  }

  #[test]
  fn test_line_index_utf8() {
    // "hola" with accent: "holá" = 5 bytes (á is 2 bytes in UTF-8)
    // But in UTF-16, "holá" is 4 code units
    let text = "hol\u{00E1}\n";
    let idx = LineIndex::new(text.to_string());

    // Position after 'l' (byte 3)
    assert_eq!(idx.line_col_utf16(BytePosition(3)), (0, 3));

    // Position after 'á' (byte 5, since á is 2 bytes)
    // UTF-16 column should be 4 (h=1, o=1, l=1, á=1)
    assert_eq!(idx.line_col_utf16(BytePosition(5)), (0, 4));
  }

  #[test]
  fn test_line_index_emoji() {
    // Emoji that's 4 bytes in UTF-8 but 2 code units in UTF-16
    let text = "a\u{1F600}b\n"; // a + grinning face + b
    let idx = LineIndex::new(text.to_string());

    // 'a' is at byte 0, col 0
    assert_eq!(idx.line_col_utf16(BytePosition(0)), (0, 0));

    // After 'a' (byte 1), col 1
    assert_eq!(idx.line_col_utf16(BytePosition(1)), (0, 1));

    // After emoji (byte 5), UTF-16 col should be 3 (a=1, emoji=2)
    assert_eq!(idx.line_col_utf16(BytePosition(5)), (0, 3));

    // After 'b' (byte 6), UTF-16 col should be 4
    assert_eq!(idx.line_col_utf16(BytePosition(6)), (0, 4));
  }
}
