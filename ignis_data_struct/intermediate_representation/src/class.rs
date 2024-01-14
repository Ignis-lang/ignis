use super::function::IRFunction;

#[derive(Debug, Clone)]
pub struct IRClass {
  pub name: String,
  pub methods: Vec<IRFunction>,
}

impl IRClass {
  pub fn new(name: String, methods: Vec<IRFunction>) -> Self {
    Self { name, methods }
  }
}
