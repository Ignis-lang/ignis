use core::marker::PhantomData;
use std::hash::{Hash, Hasher};

pub mod attribute;
pub mod compilation_context;
pub mod definition;
pub mod file;
pub mod module;
pub mod namespace;
pub mod span;
pub mod symbol;
pub mod types;
pub mod value;

#[repr(transparent)]
#[derive(Debug)]
pub struct Id<T> {
  index: u32,
  _phantom: PhantomData<fn() -> T>,
}

impl<T> Default for Id<T> {
  fn default() -> Self {
    Self {
      index: 0,
      _phantom: PhantomData,
    }
  }
}

impl<T> Copy for Id<T> {}

impl<T> Clone for Id<T> {
  fn clone(&self) -> Self {
    *self
  }
}

impl<T> PartialEq for Id<T> {
  fn eq(
    &self,
    other: &Self,
  ) -> bool {
    self.index == other.index
  }
}

impl<T> Eq for Id<T> {}

impl<T> Hash for Id<T> {
  fn hash<H: Hasher>(
    &self,
    state: &mut H,
  ) {
    self.index.hash(state);
  }
}

impl<T> Id<T> {
  pub const fn new(index: u32) -> Self {
    Self {
      index,
      _phantom: PhantomData,
    }
  }

  pub fn index(&self) -> u32 {
    self.index
  }
}

unsafe impl<T> Send for Id<T> {}
unsafe impl<T> Sync for Id<T> {}

#[derive(Default, Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BytePosition(pub u32);

impl std::fmt::Display for BytePosition {
  fn fmt(
    &self,
    f: &mut std::fmt::Formatter<'_>,
  ) -> std::fmt::Result {
    write!(f, "{}", self.0)
  }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Store<T> {
  data: Vec<T>,
}

impl<T> Store<T> {
  pub fn new() -> Self {
    Self { data: Vec::new() }
  }

  pub fn alloc(
    &mut self,
    v: T,
  ) -> Id<T> {
    let id = Id::new(self.data.len() as u32);
    self.data.push(v);
    id
  }

  /// Get a reference to the value at the given ID.
  ///
  /// # Panics
  /// Panics if the ID is out of bounds. Use `try_get()` for a non-panicking version.
  pub fn get(
    &self,
    id: &Id<T>,
  ) -> &T {
    let index = id.index() as usize;
    assert!(
      index < self.data.len(),
      "Store::get() called with invalid ID: index {} but store only has {} elements",
      index,
      self.data.len()
    );
    &self.data[index]
  }

  /// Try to get a reference to the value at the given ID.
  ///
  /// Returns `None` if the ID is out of bounds.
  pub fn try_get(
    &self,
    id: &Id<T>,
  ) -> Option<&T> {
    self.data.get(id.index() as usize)
  }

  /// Get a mutable reference to the value at the given ID.
  ///
  /// # Panics
  /// Panics if the ID is out of bounds.
  pub fn get_mut(
    &mut self,
    id: &Id<T>,
  ) -> &mut T {
    let index = id.index() as usize;
    assert!(
      index < self.data.len(),
      "Store::get_mut() called with invalid ID: index {} but store only has {} elements",
      index,
      self.data.len()
    );
    &mut self.data[index]
  }

  pub fn get_all(&self) -> &[T] {
    &self.data
  }

  pub fn len(&self) -> usize {
    self.data.len()
  }

  pub fn is_empty(&self) -> bool {
    self.data.is_empty()
  }

  pub fn iter(&self) -> impl Iterator<Item = (Id<T>, &T)> {
    self.data.iter().enumerate().map(|(i, v)| (Id::new(i as u32), v))
  }
}

impl<T> IntoIterator for Store<T> {
  type Item = T;
  type IntoIter = std::vec::IntoIter<T>;

  fn into_iter(self) -> Self::IntoIter {
    self.data.into_iter()
  }
}
