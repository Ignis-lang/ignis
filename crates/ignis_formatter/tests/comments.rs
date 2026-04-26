use std::fs;
use std::path::PathBuf;

use ignis_formatter::{CommentPlacement, FormatFile, FormatOptions, format_text};

fn workspace_root() -> PathBuf {
  PathBuf::from(env!("CARGO_MANIFEST_DIR"))
    .join("../..")
    .canonicalize()
    .expect("workspace root")
}

fn read_real_file(path: &str) -> String {
  fs::read_to_string(workspace_root().join(path)).unwrap_or_else(|error| panic!("read {path}: {error}"))
}

fn extract_slice(
  source: &str,
  start_marker: &str,
  end_marker: &str,
) -> String {
  let start = source
    .find(start_marker)
    .unwrap_or_else(|| panic!("missing start marker: {start_marker}\n{source}"));
  let end = source[start..]
    .find(end_marker)
    .map(|offset| start + offset)
    .unwrap_or_else(|| panic!("missing end marker: {end_marker}\n{source}"));

  source[start..end].trim_end().to_string()
}

fn wrap_record_body(body: &str) -> String {
  format!("record Metadata {{\n{body}\n}}\n")
}

#[test]
fn groups_contiguous_doc_comments_into_a_single_owned_block() {
  let file = FormatFile::from_source("/// first\n/// second\nfunction main(): void {return;}\n").expect("build file");

  let blocks = file.comment_blocks();

  assert_eq!(blocks.len(), 1);
  assert_eq!(blocks[0].lines, vec!["/// first".to_string(), "/// second".to_string()]);
  assert_eq!(blocks[0].placement, CommentPlacement::Leading);
}

#[test]
fn keeps_grouped_doc_comments_adjacent_inside_directive_branches() {
  let formatted = format_text(
    "@if(flag){/// first\n/// second\nfunction main(): void {return;}}\n",
    &FormatOptions::default(),
  )
  .expect("format grouped directive comments");

  assert_eq!(
    formatted,
    "@if(flag) {\n  /// first\n  /// second\n  function main(): void {\n    return;\n  }\n}\n"
  );
}

#[test]
fn preserves_enum_variant_doc_comment_ownership_for_real_std_io_slice() {
  let source = read_real_file("std/io/error.ign");
  let enum_window = extract_slice(
    &source,
    "/// Category of an I/O error.",
    "/// Structured error from an I/O operation.",
  );

  let formatted = format_text(&enum_window, &FormatOptions::default()).expect("format enum ownership window");

  assert_eq!(
    formatted,
    r#"/// Category of an I/O error.
///
/// Each variant maps to one or more POSIX errno values. The `OTHER`
/// variant is the catch-all for errno values not explicitly mapped.
enum ErrorKind {
  /// No such file or directory (`ENOENT`).
  NOT_FOUND,

  /// Permission denied (`EACCES`).
  PERMISSION_DENIED,

  /// File or resource already exists (`EEXIST`).
  ALREADY_EXISTS,

  /// Invalid argument (`EINVAL`).
  INVALID_INPUT,

  /// Operation interrupted by signal (`EINTR`).
  INTERRUPTED,

  /// Resource temporarily unavailable (`EAGAIN` / `EWOULDBLOCK`).
  WOULD_BLOCK,

  /// Connection or operation timed out (`ETIMEDOUT`).
  TIMED_OUT,

  /// Unmapped errno value.
  OTHER,
}
"#
  );
}

#[test]
fn rejects_trailing_field_comment_when_the_formatter_cannot_preserve_ownership() {
  let error = format_text(
    "record Example {\n    value: i32; // keep with value\n    /// docs for next\n    next: i32;\n}\n",
    &FormatOptions::default(),
  )
  .expect_err("formatter must fail rather than misattach a trailing field comment");

  assert_eq!(
    error.to_string(),
    "formatter safety validation failed: formatted output changed comment ownership or preserved trivia structure"
  );
}

#[test]
fn preserves_real_method_doc_comments_with_their_methods() {
  let source = read_real_file("std/fs/metadata.ign");
  let method_window = extract_slice(
    &source,
    "/// Returns the file size in bytes.",
    "/// Returns `true` if this is a regular file.",
  );

  let formatted = format_text(&wrap_record_body(&method_window), &FormatOptions::default())
    .expect("format metadata ownership window");

  assert_eq!(
    formatted,
    r#"record Metadata {
  /// Returns the file size in bytes.
  public len(&self): i64 {
    return self.size;
  }

  /// Returns the classified file type.
  public fileType(&self): Fs::FileType {
    return self.fileType;
  }
}
"#
  );
}

#[test]
fn preserves_comment_ownership_around_grouped_pointer_member_assignment() {
  let formatted = format_text(
    "record Block {\n    isFree: boolean;\n}\n\nfunction mark(block: *mut Block): void {\n    // Keep this comment attached to the assignment below\n    (*block).isFree = false;\n}\n",
    &FormatOptions::default(),
  )
  .expect("grouped pointer member assignment should stay formatter-safe");

  assert_eq!(
    formatted,
    "record Block {\n  isFree: boolean;\n}\n\nfunction mark(block: *mut Block): void {\n  // Keep this comment attached to the assignment below\n  (*block).isFree = false;\n}\n"
  );
}

#[test]
fn keeps_import_separator_before_documented_top_level_declaration() {
  let formatted = format_text(
    "import Block from \"./block\";\nimport Layout, Align from \"std::memory\";\nimport LibC, CType from \"std::libc\";\n/// Strategy for searching free blocks in the allocator.\n///\n/// When reusing freed memory blocks, different search strategies offer\n/// different trade-offs between allocation speed and memory utilization.\nexport enum SearchMode {\n    FirstFit,\n    NextFit,\n    BestFit,\n}\n",
    &FormatOptions::default(),
  )
  .expect("imports followed by documented enum should preserve doc ownership");

  assert_eq!(
    formatted,
    "import Block from \"./block\";\nimport Layout, Align from \"std::memory\";\nimport LibC, CType from \"std::libc\";\n\n/// Strategy for searching free blocks in the allocator.\n///\n/// When reusing freed memory blocks, different search strategies offer\n/// different trade-offs between allocation speed and memory utilization.\nexport enum SearchMode {\n  FirstFit,\n  NextFit,\n  BestFit,\n}\n"
  );
}
