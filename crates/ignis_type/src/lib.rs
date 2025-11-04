use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, PartialEq, Default)]
pub struct Span {
  pub start: usize,
  pub end: usize,
}

impl Display for Span {
  fn fmt(
    &self,
    f: &mut Formatter<'_>,
  ) -> std::fmt::Result {
    write!(f, "(span start: {} end: {})", self.start, self.end)
  }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct NodeId(u32);

impl Display for NodeId {
  fn fmt(
    &self,
    f: &mut Formatter<'_>,
  ) -> std::fmt::Result {
    write!(f, "(node id: {})", self.0)
  }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct TypeId(u32);

impl Display for TypeId {
  fn fmt(
    &self,
    f: &mut Formatter<'_>,
  ) -> std::fmt::Result {
    write!(f, "(type id: {})", self.0)
  }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct FileId(u32);

impl Display for FileId {
  fn fmt(
    &self,
    f: &mut Formatter<'_>,
  ) -> std::fmt::Result {
    write!(f, "(file id: {})", self.0)
  }
}
