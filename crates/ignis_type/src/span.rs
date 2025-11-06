use crate::{BytePosition, file::FileId};

#[derive(Debug, Clone, PartialEq)]
pub struct Span {
  pub start: BytePosition,
  pub end: BytePosition,
  pub file: FileId,
}

impl Default for Span {
  fn default() -> Self {
    Self {
      start: BytePosition::default(),
      end: BytePosition::default(),
      file: FileId::default(),
    }
  }
}

impl Span {
  pub fn merge(
    a: &Self,
    b: &Self,
  ) -> Self {
    debug_assert_eq!(a.file, b.file, "Cannot merge spans from different files");
    Self {
      file: a.file.clone(),
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

  pub fn len(&self) -> usize {
    (self.end.0 - self.start.0) as usize
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
