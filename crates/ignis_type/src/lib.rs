use core::marker::PhantomData;

pub mod file;
pub mod span;
pub mod symbol;

#[repr(transparent)]
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Default)]
pub struct Id<T>(pub u32, PhantomData<*const T>);

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
    let id = Id(self.data.len() as u32, PhantomData);
    self.data.push(v);
    id
  }

  pub fn get(
    &self,
    id: &Id<T>,
  ) -> &T {
    &self.data[id.0 as usize]
  }

  pub fn get_mut(
    &mut self,
    id: Id<T>,
  ) -> &mut T {
    &mut self.data[id.0 as usize]
  }
}
