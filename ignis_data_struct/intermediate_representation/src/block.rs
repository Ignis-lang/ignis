use super::{IRInstruction, variable::IRVariable};

#[derive(Debug, Clone)]
pub struct IRBlock {
  pub instructions: Vec<IRInstruction>,
  pub scopes_variables: Vec<IRVariable>,
}

impl IRBlock {
  pub fn new(instructions: Vec<IRInstruction>, scopes_variables: Vec<IRVariable>) -> Self {
    Self {
      instructions,
      scopes_variables,
    }
  }

  pub fn to_json(&self) -> serde_json::Value {
    serde_json::json!({
      "type": "block",
      "instructions": self.instructions.iter().map(|instruction| instruction.to_json()).collect::<Vec<serde_json::Value>>(),
      "scopes_variables": self.scopes_variables.iter().map(|variable| variable.to_json()).collect::<Vec<serde_json::Value>>(),
    })
  }
}
