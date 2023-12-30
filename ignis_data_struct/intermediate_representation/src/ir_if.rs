use super::{IRInstruction, IRInstructionTrait};

#[derive(Debug, Clone)]
pub struct IRIf {
  pub condition: Box<IRInstruction>,
  pub then_branch: Box<IRInstruction>,
  pub else_branch: Option<Box<IRInstruction>>,
}

impl IRIf {
  pub fn new(
    condition: Box<IRInstruction>,
    then_branch: Box<IRInstruction>,
    else_branch: Option<Box<IRInstruction>>,
  ) -> Self {
    Self {
      condition,
      then_branch,
      else_branch,
    }
  }
}

impl IRInstructionTrait for IRIf {
  fn to_json(&self) -> serde_json::Value {
    serde_json::json!({
      "type": "IRIf",
      "condition": self.condition.to_json(),
      "then_branch": self.then_branch.to_json(),
      "else_branch": if let Some(else_branch) = &self.else_branch {
        else_branch.to_json()
      } else {
        serde_json::Value::Null
      },
    })
  }
}
