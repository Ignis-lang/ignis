use std::collections::HashMap;

use diagnostic_report::DiagnosticReport;
use intermediate_representation::IRInstruction;
use to_c::TranspilerToC;
use to_lua::TranspilerToLua;

pub enum BackendTarget {
  Lua,
  C,
  Llvm,
  Bytecode,
  Evaluator,
}

pub struct IgnisBackend {
  backend: BackendTarget,
  irs: HashMap<String, Vec<IRInstruction>>,
}

impl IgnisBackend {
  pub fn new(
    backend: BackendTarget,
    irs: HashMap<String, Vec<IRInstruction>>,
  ) -> Self {
    Self { backend, irs }
  }

  pub fn process(&self) -> Result<(), Vec<DiagnosticReport>> {
    match self.backend {
      BackendTarget::Lua => self.to_lua(),
      BackendTarget::C => self.to_c(),
      _ => todo!("Backend target not implemented"),
    };

    Ok(())
  }

  fn to_c(&self) {
    let irs = &self.irs;
    let mut transpiler = TranspilerToC::new(irs.clone());
    transpiler.process();
  }

  fn to_lua(&self) {
    let irs = &self.irs;
    let mut transpiler = TranspilerToLua::new(irs.clone());
    transpiler.process();
  }
}
