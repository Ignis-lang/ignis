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
    let f = self.get(file);
    let line = upper_bound_line(&f.line_starts, pos);
    let line_start = f.line_starts[line].0 as usize;
    let slice = &f.text.as_bytes()[line_start..pos.0 as usize];
    let col = unicode_column(slice);

    ((line as u32) + 1, (col as u32) + 1)
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
    let (line, col) = self.line_col(&span.file, span.start);
    let f = self.get(&span.file);
    let line_idx = (line - 1) as usize;
    let line_start = f.line_starts[line_idx].0 as usize;
    let line_end = f
      .line_starts
      .get(line_idx + 1)
      .map(|p| p.0 as usize)
      .unwrap_or_else(|| f.text.len());

    let line_str = &f.text[line_start..line_end].trim_end_matches(&['\r', '\n'][..]);
    let caret = " ".repeat((col - 1) as usize) + &"^".repeat(span.len());

    format!("{:>4} | {}\n     | {}", line, line_str, caret)
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

fn fxhash_u64(text: &str) -> u64 {
  let mut h = AHasher::default();
  text.hash(&mut h);
  h.finish()
}
