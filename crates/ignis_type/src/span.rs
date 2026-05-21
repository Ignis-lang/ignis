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

  /// Returns the file-independent byte range of this span.
  ///
  /// A `Span` is a `TextRange` paired with the file it belongs to; this drops
  /// the file association when only the byte offsets are relevant.
  pub fn range(&self) -> TextRange {
    TextRange {
      start: self.start,
      end: self.end,
    }
  }

  /// Builds a span by attaching a file to a file-independent byte range.
  pub fn with_range(
    file: FileId,
    range: TextRange,
  ) -> Self {
    Self {
      file,
      start: range.start,
      end: range.end,
    }
  }
}

/// A half-open byte range `[start, end)` within some source text, independent
/// of any particular file.
///
/// `Span` is the file-qualified counterpart: `Span = TextRange + FileId`. Use
/// `TextRange` when offsets are computed or compared without caring which file
/// they came from (e.g. relative to a single buffer being lexed).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct TextRange {
  pub start: BytePosition,
  pub end: BytePosition,
}

impl TextRange {
  /// Creates a new range.
  ///
  /// # Panics
  /// Panics in debug mode if `start > end`.
  pub fn new(
    start: BytePosition,
    end: BytePosition,
  ) -> Self {
    debug_assert!(
      start <= end,
      "TextRange::new() called with invalid range: start {} > end {}",
      start,
      end
    );
    Self { start, end }
  }

  /// Creates an empty range positioned at `pos`.
  pub fn empty_at(pos: BytePosition) -> Self {
    Self { start: pos, end: pos }
  }

  /// Returns the smallest range covering both `a` and `b`.
  pub fn merge(
    a: &Self,
    b: &Self,
  ) -> Self {
    Self {
      start: a.start.min(b.start),
      end: a.end.max(b.end),
    }
  }

  /// Returns the length of the range in bytes.
  ///
  /// # Note
  /// Returns 0 for an invalid range (`start > end`) to avoid underflow.
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

  /// Returns true if the range is valid (start <= end).
  pub fn is_valid(&self) -> bool {
    self.start <= self.end
  }

  /// Returns true if `pos` falls within the half-open range `[start, end)`.
  pub fn contains(
    &self,
    pos: BytePosition,
  ) -> bool {
    self.start <= pos && pos < self.end
  }
}

impl std::fmt::Display for TextRange {
  fn fmt(
    &self,
    f: &mut std::fmt::Formatter<'_>,
  ) -> std::fmt::Result {
    write!(f, "(range start: {} end: {})", self.start, self.end)
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

#[cfg(test)]
mod tests {
  use super::{Span, TextRange};
  use crate::{BytePosition, file::FileId};

  #[test]
  fn text_range_reports_length_and_emptiness() {
    let range = TextRange::new(BytePosition(2), BytePosition(5));
    assert_eq!(range.len(), 3);
    assert!(!range.is_empty());

    let empty = TextRange::empty_at(BytePosition(7));
    assert_eq!(empty.len(), 0);
    assert!(empty.is_empty());
  }

  #[test]
  fn text_range_contains_uses_half_open_bounds() {
    let range = TextRange::new(BytePosition(2), BytePosition(5));

    assert!(!range.contains(BytePosition(1)));
    assert!(range.contains(BytePosition(2)));
    assert!(range.contains(BytePosition(4)));
    assert!(!range.contains(BytePosition(5)));
  }

  #[test]
  fn text_range_merge_covers_both_ranges() {
    let left = TextRange::new(BytePosition(2), BytePosition(4));
    let right = TextRange::new(BytePosition(6), BytePosition(9));

    let merged = TextRange::merge(&left, &right);

    assert_eq!(merged, TextRange::new(BytePosition(2), BytePosition(9)));
  }

  #[test]
  fn span_range_round_trips_through_with_range() {
    let file = FileId::new(3);
    let span = Span::new(file, BytePosition(4), BytePosition(10));

    let range = span.range();
    assert_eq!(range, TextRange::new(BytePosition(4), BytePosition(10)));

    let rebuilt = Span::with_range(file, range);
    assert_eq!(rebuilt, span);
  }
}
