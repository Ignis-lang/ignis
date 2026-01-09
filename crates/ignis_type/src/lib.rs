use core::marker::PhantomData;

pub mod definition;
pub mod file;
pub mod module;
pub mod span;
pub mod symbol;
pub mod types;
pub mod value;

#[repr(transparent)]
#[derive(Eq, PartialEq, Hash, Debug, Default)]
pub struct Id<T> {
  index: u32,
  _phantom: PhantomData<fn() -> T>,
}

impl<T> Id<T> {
  pub fn new(index: u32) -> Self {
    Self {
      index,
      _phantom: PhantomData,
    }
  }

  pub fn index(&self) -> u32 {
    self.index
  }
}

impl<T> Copy for Id<T> {}

impl<T> Clone for Id<T> {
  fn clone(&self) -> Self {
    *self
  }
}

// Manually impl Send/Sync for Id
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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

  pub fn get(
    &self,
    id: &Id<T>,
  ) -> &T {
    &self.data[id.index() as usize]
  }

  pub fn get_mut(
    &mut self,
    id: &Id<T>,
  ) -> &mut T {
    &mut self.data[id.index() as usize]
  }

  pub fn get_all(&self) -> &[T] {
    &self.data
  }
}
