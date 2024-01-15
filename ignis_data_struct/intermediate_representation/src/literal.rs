use crate::analyzer_value::AnalyzerValue;

#[derive(Debug, Clone)]
pub struct IRLiteral {
  pub value: AnalyzerValue,
}

impl IRLiteral {
  pub fn new(value: AnalyzerValue) -> Self {
    Self { value }
  }

  pub fn to_json(&self) -> serde_json::Value {
    serde_json::json!({
      "type": "literal",
      "value": self.value.to_string()
    })
  }
}
