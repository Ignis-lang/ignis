use std::sync::Arc;

pub struct IgnisSourceFile {
  pub name: String,
  pub source: Arc<String>,
}

impl IgnisSourceFile {
  pub fn new(
    name: String,
    source: Arc<String>,
  ) -> Self {
    Self { name, source }
  }

  pub fn new_arc(
    name: String,
    source: Arc<String>,
  ) -> Arc<Self> {
    Arc::new(Self::new(name, source))
  }
}

impl std::fmt::Display for IgnisSourceFile {
  fn fmt(
    &self,
    f: &mut std::fmt::Formatter<'_>,
  ) -> std::fmt::Result {
    write!(f, "{}", self.name)
  }
}

impl std::fmt::Debug for IgnisSourceFile {
  fn fmt(
    &self,
    f: &mut std::fmt::Formatter<'_>,
  ) -> std::fmt::Result {
    write!(f, "{}", self.name)
  }
}
