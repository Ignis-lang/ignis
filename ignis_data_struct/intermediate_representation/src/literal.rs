use serde_json::json;

use crate::analyzer_value::AnalyzerValue;

use super::IRInstructionTrait;

#[derive(Debug, Clone)]
pub struct IRLiteral {
  pub value: AnalyzerValue,
}

impl IRLiteral {
  pub fn new(value: AnalyzerValue) -> Self {
    Self { value }
  }
}

impl IRInstructionTrait for IRLiteral {
  fn to_json(&self) -> serde_json::Value {
    json!({
      "type": "IRLiteral",
      "value": self.value.to_string(),
    })
  }
}
