use std::collections::HashMap;

use diagnostic_report::DiagnosticReport;
use intermediate_representation::IRInstruction;
use to_c::TranspilerToC;
use to_lua::TranspilerToLua;
use backend_trait::BackendTrait;

type BackendResult = Result<(), Vec<DiagnosticReport>>;

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

impl BackendTrait<BackendResult> for IgnisBackend {
  fn process(&mut self) -> BackendResult {
    let mut backend: Box<dyn BackendTrait<()>> = match self.backend {
      BackendTarget::Lua => Box::new(TranspilerToLua::new(self.irs.clone())),
      BackendTarget::C => Box::new(TranspilerToC::new(self.irs.clone())),
      _ => todo!("Backend target not implemented"),
    };

    backend.process();

    Ok(())
  }
}

impl IgnisBackend {
  pub fn new(backend: BackendTarget, irs: HashMap<String, Vec<IRInstruction>>) -> Self {
    Self { backend, irs }
  }
}
