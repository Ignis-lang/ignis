use crate::{BytePosition, file::FileId};

#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct Span {
  pub start: BytePosition,
  pub end: BytePosition,
  pub file: FileId,
}

impl Span {
  /// Create a new span with validation.
  ///
  /// # Panics
  /// Panics in debug mode if `start > end`.
  pub fn new(
    file: FileId,
    start: BytePosition,
    end: BytePosition,
  ) -> Self {
    debug_assert!(
      start <= end,
      "Span::new() called with invalid range: start {} > end {}",
      start,
      end
    );
    Self { file, start, end }
  }

  pub fn merge(
    a: &Self,
    b: &Self,
  ) -> Self {
    debug_assert_eq!(a.file, b.file, "Cannot merge spans from different files");
    Self {
      file: a.file,
      start: a.start.min(b.start),
      end: a.end.max(b.end),
    }
  }

  pub fn empty_at(
    file: FileId,
    pos: BytePosition,
  ) -> Self {
    Self {
      file,
      start: pos,
      end: pos,
    }
  }

  /// Returns the length of the span in bytes.
  ///
  /// # Note
  /// Returns 0 if `start > end` (invalid span) to avoid underflow.
  pub fn len(&self) -> usize {
    if self.end.0 >= self.start.0 {
      (self.end.0 - self.start.0) as usize
    } else {
      0
    }
  }

  pub fn is_empty(&self) -> bool {
    self.len() == 0
  }

  /// Returns true if this span is valid (start <= end).
  pub fn is_valid(&self) -> bool {
    self.start <= self.end
  }
}

impl std::fmt::Display for Span {
  fn fmt(
    &self,
    f: &mut std::fmt::Formatter<'_>,
  ) -> std::fmt::Result {
    write!(f, "(span start: {} end: {} file: {})", self.start, self.end, self.file)
  }
}
