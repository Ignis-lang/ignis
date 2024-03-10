pub trait BackendTrait<T> {
  fn process(&mut self) -> T;
}
