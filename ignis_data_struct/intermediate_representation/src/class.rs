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

  pub fn to_json(&self) -> serde_json::Value {
    serde_json::json!({
      "type": "class",
      "name": self.name,
      "methods": self.methods.iter().map(|x| x.to_json()).collect::<Vec<_>>(),
    })
  }
}
