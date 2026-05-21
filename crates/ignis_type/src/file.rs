use std::{collections::HashMap, path::PathBuf};
use std::hash::{Hash, Hasher};
use ahash::AHasher;
use crate::{BytePosition, Id, Store, span::Span};

pub type FileId = Id<SourceFile>;

impl FileId {
  /// Sentinel FileId for compiler-internal synthetic definitions.
  /// Uses u32::MAX to never collide with real file indices (which start at 0).
  pub const SYNTHETIC: FileId = FileId::new(u32::MAX);
}

#[derive(Default, Clone, Eq, PartialEq, Hash, Debug)]
pub struct SourceFile {
  pub path: PathBuf,
  pub text: String,
  pub line_starts: Vec<BytePosition>,
  pub hash: u64,
}

impl SourceFile {
  pub fn new(
    path: PathBuf,
    text: String,
  ) -> Self {
    let line_starts = compute_line_starts(&text);
    let hash = fxhash_u64(&text);
    Self {
      path,
      text,
      line_starts,
      hash,
    }
  }

  #[inline]
  pub fn len_bytes(&self) -> u32 {
    self.text.len() as u32
  }
}

impl std::fmt::Display for SourceFile {
  fn fmt(
    &self,
    f: &mut std::fmt::Formatter<'_>,
  ) -> std::fmt::Result {
    write!(f, "(file path: {})", self.path.display())
  }
}

impl std::fmt::Display for Id<SourceFile> {
  fn fmt(
    &self,
    f: &mut std::fmt::Formatter<'_>,
  ) -> std::fmt::Result {
    write!(f, "(file id: {})", self.index())
  }
}

pub struct SourceMap {
  files: Store<SourceFile>,
  by_path: HashMap<PathBuf, FileId>,
}

impl Default for SourceMap {
  fn default() -> Self {
    Self::new()
  }
}

impl SourceMap {
  pub fn new() -> Self {
    Self {
      files: Store::new(),
      by_path: HashMap::new(),
    }
  }

  pub fn add_file<P: Into<PathBuf>>(
    &mut self,
    path: P,
    text: String,
  ) -> FileId {
    let path = normalize_path(path.into());

    if let Some(id) = self.by_path.get(&path) {
      return *id;
    }

    let id = self.files.alloc(SourceFile::new(path.clone(), text));
    self.by_path.insert(path, id);
    id
  }

  pub fn add_virtual(
    &mut self,
    label: &str,
    text: String,
  ) -> FileId {
    let path = PathBuf::from(format!("<{}>", label));

    self.files.alloc(SourceFile::new(path, text))
  }

  #[inline]
  pub fn get(
    &self,
    id: &FileId,
  ) -> &SourceFile {
    self.files.get(id)
  }

  /// Look up a file by its path. Returns None if the file is not in the map.
  pub fn lookup_by_path<P: AsRef<std::path::Path>>(
    &self,
    path: P,
  ) -> Option<FileId> {
    let normalized = normalize_path(path.as_ref().to_path_buf());
    self.by_path.get(&normalized).cloned()
  }

  /// Iterate over all paths and their FileIds in the map.
  pub fn iter_paths(&self) -> impl Iterator<Item = (&PathBuf, &FileId)> {
    self.by_path.iter()
  }

  pub fn line_col(
    &self,
    file: &FileId,
    pos: BytePosition,
  ) -> (u32, u32) {
    self.display_line_col(file, pos)
  }

  pub fn display_line_col(
    &self,
    file: &FileId,
    pos: BytePosition,
  ) -> (u32, u32) {
    let f = self.get(file);
    let line = upper_bound_line(&f.line_starts, pos);
    let line_start = f.line_starts[line].0 as usize;
    let slice = &f.text.as_bytes()[line_start..pos.0 as usize];
    let col = unicode_column(slice);

    ((line as u32) + 1, (col as u32) + 1)
  }

  pub fn byte_line_col(
    &self,
    file: &FileId,
    pos: BytePosition,
  ) -> (u32, u32) {
    let f = self.get(file);
    let line = upper_bound_line(&f.line_starts, pos);
    let line_start = f.line_starts[line].0;
    let col = pos.0.saturating_sub(line_start) + 1;

    ((line as u32) + 1, col)
  }

  pub fn slice(
    &self,
    span: Span,
  ) -> &str {
    let f = self.get(&span.file);
    &f.text[span.start.0 as usize..span.end.0 as usize]
  }

  pub fn snippet(
    &self,
    span: Span,
  ) -> String {
    let f = self.get(&span.file);
    let start_line_idx = upper_bound_line(&f.line_starts, span.start);
    let end_anchor = if span.end.0 > span.start.0 {
      BytePosition(span.end.0 - 1)
    } else {
      span.end
    };
    let end_line_idx = upper_bound_line(&f.line_starts, end_anchor);
    let mut snippet_lines = Vec::new();

    for line_idx in start_line_idx..=end_line_idx {
      let line_number = (line_idx as u32) + 1;
      let (line_start, _line_end, content_end) = line_bounds(f, line_idx);
      let line_str = &f.text[line_start..content_end];

      let highlight_start = if line_idx == start_line_idx {
        span.start.0 as usize
      } else {
        line_start
      }
      .clamp(line_start, content_end);

      let highlight_end = if span.is_empty() {
        highlight_start
      } else if line_idx == end_line_idx {
        (span.end.0 as usize).clamp(line_start, content_end)
      } else {
        content_end
      };

      let caret_col = unicode_column(&f.text.as_bytes()[line_start..highlight_start]);
      let caret_width = if highlight_end > highlight_start {
        unicode_width(&f.text[highlight_start..highlight_end])
      } else {
        1
      };

      snippet_lines.push(format!("{:>4} | {}", line_number, line_str));
      snippet_lines.push(format!("     | {}{}", " ".repeat(caret_col), "^".repeat(caret_width.max(1))));
    }

    snippet_lines.join("\n")
  }
}

/// Normalize a path for consistent comparison.
///
/// Attempts to canonicalize the path (resolving symlinks and making it absolute).
/// Falls back to the original path if canonicalization fails (e.g., file doesn't exist).
pub fn normalize_path(mut p: PathBuf) -> PathBuf {
  if let Ok(c) = p.canonicalize() {
    p = c;
  }
  p
}

fn compute_line_starts(text: &str) -> Vec<BytePosition> {
  let bytes = text.as_bytes();
  let mut v = Vec::with_capacity(128);
  v.push(BytePosition(0));

  for (i, b) in bytes.iter().enumerate() {
    if *b == b'\n' {
      v.push(BytePosition((i + 1) as u32));
    }
  }
  v
}

fn upper_bound_line(
  starts: &[BytePosition],
  pos: BytePosition,
) -> usize {
  let mut lo = 0usize;
  let mut hi = starts.len();
  while lo + 1 < hi {
    let mid = (lo + hi) / 2;
    if starts[mid].0 <= pos.0 { lo = mid } else { hi = mid }
  }
  lo
}

fn unicode_column(slice: &[u8]) -> usize {
  std::str::from_utf8(slice)
    .map(|s| s.chars().count())
    .unwrap_or(slice.len())
}

fn unicode_width(text: &str) -> usize {
  text.chars().count()
}

fn line_bounds(
  file: &SourceFile,
  line_idx: usize,
) -> (usize, usize, usize) {
  let line_start = file.line_starts[line_idx].0 as usize;
  let line_end = file
    .line_starts
    .get(line_idx + 1)
    .map(|p| p.0 as usize)
    .unwrap_or_else(|| file.text.len());
  let content_end = file.text[line_start..line_end]
    .trim_end_matches(&['\r', '\n'][..])
    .len()
    + line_start;

  (line_start, line_end, content_end)
}

fn fxhash_u64(text: &str) -> u64 {
  let mut h = AHasher::default();
  text.hash(&mut h);
  h.finish()
}

#[cfg(test)]
mod tests {
  use super::SourceMap;
  use crate::{BytePosition, span::Span};

  #[test]
  fn snippet_uses_character_width_for_single_multibyte_span() {
    let mut source_map = SourceMap::new();
    let file = source_map.add_virtual("utf8", "aéb".to_string());
    let span = Span::new(file, BytePosition(1), BytePosition(3));

    assert_eq!(source_map.snippet(span), "   1 | aéb\n     |  ^");
  }

  #[test]
  fn snippet_uses_character_width_for_mixed_ascii_and_multibyte_span() {
    let mut source_map = SourceMap::new();
    let file = source_map.add_virtual("utf8", "aéb".to_string());
    let span = Span::new(file, BytePosition(1), BytePosition(4));

    assert_eq!(source_map.snippet(span), "   1 | aéb\n     |  ^^");
  }

  #[test]
  fn byte_line_col_uses_byte_offsets_for_multibyte_text() {
    let mut source_map = SourceMap::new();
    let file = source_map.add_virtual("utf8", "aéb".to_string());

    assert_eq!(source_map.byte_line_col(&file, BytePosition(0)), (1, 1));
    assert_eq!(source_map.byte_line_col(&file, BytePosition(1)), (1, 2));
    assert_eq!(source_map.byte_line_col(&file, BytePosition(3)), (1, 4));
    assert_eq!(source_map.line_col(&file, BytePosition(3)), (1, 3));
  }

  #[test]
  fn display_line_col_uses_unicode_columns_for_multibyte_text() {
    let mut source_map = SourceMap::new();
    let file = source_map.add_virtual("utf8", "aéb".to_string());

    assert_eq!(source_map.display_line_col(&file, BytePosition(0)), (1, 1));
    assert_eq!(source_map.display_line_col(&file, BytePosition(1)), (1, 2));
    assert_eq!(source_map.display_line_col(&file, BytePosition(3)), (1, 3));
  }

  #[test]
  fn snippet_marks_zero_length_span() {
    let mut source_map = SourceMap::new();
    let file = source_map.add_virtual("empty", "abc".to_string());
    let span = Span::new(file, BytePosition(1), BytePosition(1));

    assert_eq!(source_map.snippet(span), "   1 | abc\n     |  ^");
  }

  #[test]
  fn snippet_renders_multiline_span() {
    let mut source_map = SourceMap::new();
    let file = source_map.add_virtual("multi", "alpha\nbeta\ngamma".to_string());
    let span = Span::new(file, BytePosition(2), BytePosition(10));

    assert_eq!(source_map.snippet(span), "   1 | alpha\n     |   ^^^\n   2 | beta\n     | ^^^^");
  }

  #[test]
  fn snippet_handles_tabs_eof_and_crlf() {
    let mut source_map = SourceMap::new();
    let file = source_map.add_virtual("mixed", "\txy\r\nlast".to_string());

    let tabbed = Span::new(file, BytePosition(1), BytePosition(3));
    assert_eq!(source_map.snippet(tabbed), "   1 | \txy\n     |  ^^");

    let eof = Span::new(file, BytePosition(9), BytePosition(9));
    assert_eq!(source_map.snippet(eof), "   2 | last\n     |     ^");
  }

  #[test]
  fn snippet_counts_wide_cjk_scalar_as_single_column() {
    let mut source_map = SourceMap::new();
    // '世' is three UTF-8 bytes (offsets 1..4) but a single Unicode scalar.
    let file = source_map.add_virtual("wide", "a世b".to_string());
    let span = Span::new(file, BytePosition(1), BytePosition(4));

    assert_eq!(source_map.snippet(span), "   1 | a世b\n     |  ^");
  }

  #[test]
  fn snippet_counts_each_wide_scalar_as_one_caret() {
    let mut source_map = SourceMap::new();
    // '世' and '界' are three bytes each; the span covers both (offsets 0..6).
    let file = source_map.add_virtual("wide", "世界x".to_string());
    let span = Span::new(file, BytePosition(0), BytePosition(6));

    assert_eq!(source_map.snippet(span), "   1 | 世界x\n     | ^^");
  }

  #[test]
  fn snippet_counts_astral_emoji_scalar_as_single_column() {
    let mut source_map = SourceMap::new();
    // '😀' is a four-byte astral-plane scalar (offsets 0..4).
    let file = source_map.add_virtual("emoji", "😀x".to_string());
    let span = Span::new(file, BytePosition(0), BytePosition(4));

    assert_eq!(source_map.snippet(span), "   1 | 😀x\n     | ^");
  }

  #[test]
  fn snippet_offsets_caret_past_leading_wide_scalar() {
    let mut source_map = SourceMap::new();
    // The caret must sit at column 1: the leading '世' counts as one column.
    let file = source_map.add_virtual("wide", "世a".to_string());
    let span = Span::new(file, BytePosition(3), BytePosition(4));

    assert_eq!(source_map.snippet(span), "   1 | 世a\n     |  ^");
  }
}
