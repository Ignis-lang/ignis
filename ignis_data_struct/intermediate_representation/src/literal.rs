use crate::analyzer_value::AnalyzerValue;

#[derive(Debug, Clone)]
pub struct IRLiteral {
  pub value: AnalyzerValue,
}

impl IRLiteral {
  pub fn new(value: AnalyzerValue) -> Self {
    Self { value }
  }
}
