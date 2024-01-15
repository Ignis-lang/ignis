use super::IRInstruction;

#[derive(Debug, Clone)]
pub struct IRTernary {
  pub condition: Box<IRInstruction>,
  pub then_branch: Box<IRInstruction>,
  pub else_branch: Box<IRInstruction>,
}

impl IRTernary {
  pub fn new(
    condition: Box<IRInstruction>,
    then_branch: Box<IRInstruction>,
    else_branch: Box<IRInstruction>,
  ) -> Self {
    Self {
      condition,
      then_branch,
      else_branch,
    }
  }

  pub fn to_json(&self) -> serde_json::Value {
    serde_json::json!({
      "type": "ternary",
      "condition": self.condition.to_json(),
      "then_branch": self.then_branch.to_json(),
      "else_branch": self.else_branch.to_json(),
    })
  }
}
