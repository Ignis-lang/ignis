use std::collections::HashMap;

use diagnostic_report::DiagnosticReport;
use intermediate_representation::IRInstruction;
use to_lua::TranspilerToLua;

pub enum BackendTarget {
  Lua,
}

pub struct CodeResult {
  pub code: String,
  pub file_name: String,
}

impl CodeResult {
  pub fn new(code: String, file_name: String) -> Self {
    Self { code, file_name }
  }
}

pub struct IgnisBackend {
  backend: BackendTarget,
}

impl IgnisBackend {
  pub fn new(backend: BackendTarget) -> Self {
    Self { backend }
  }

  pub fn process(
    &self,
    ir: HashMap<String, Vec<IRInstruction>>,
  ) -> Result<Vec<CodeResult>, Vec<DiagnosticReport>> {
    Ok(match self.backend {
      BackendTarget::Lua => self.to_lua(ir),
    })
  }

  fn to_lua(&self, irs: HashMap<String, Vec<IRInstruction>>) -> Vec<CodeResult> {
    let mut transpiler = TranspilerToLua::new();
    let mut code_results = Vec::new();

    for result in irs.iter() {
      transpiler.transpile(result.1);

      code_results.push(CodeResult::new(transpiler.code.clone(), result.0.clone()));
    }

    code_results
  }
}
