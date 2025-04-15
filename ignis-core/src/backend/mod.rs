use std::collections::HashMap;

use ignis_config::{DebugPrint, IgnisConfig};
use ignis_hir::HIRInstruction;
use ir::{ir::IRProgramInstruction, IRGenerator};

pub mod ir;

pub struct IgnisBackend<'a> {
  config: &'a IgnisConfig,
  hir_ffi_std: HashMap<String, Vec<HIRInstruction>>,
  hirs: HashMap<String, Vec<HIRInstruction>>,
  irs: HashMap<String, Vec<IRProgramInstruction>>,
  ir_std: HashMap<String, Vec<IRProgramInstruction>>,
}

impl<'a> IgnisBackend<'a> {
  pub fn new(
    config: &'a IgnisConfig,
    hir_ffi_std: HashMap<String, Vec<HIRInstruction>>,
    hirs: HashMap<String, Vec<HIRInstruction>>,
  ) -> Self {
    Self {
      config,
      hir_ffi_std,
      hirs,
      irs: HashMap::new(),
      ir_std: HashMap::new(),
    }
  }

  pub fn process(&mut self) {
    let mut ir_generator = IRGenerator::new();

    if !self.hir_ffi_std.is_empty() {
      ir_generator.process(&self.hir_ffi_std);
      self.ir_std.clone_from(&ir_generator.programs_by_file);
    }

    ir_generator.process(&self.hirs);

    self.irs.clone_from(&ir_generator.programs_by_file);

    if self.config.debug.contains(&DebugPrint::Ir) {
      if self.config.verbose == 0 {
        ir_generator.print();
      }

      if self.config.verbose > 0 {
        for (name, ir) in &self.ir_std {
          println!("File {}:", name);
          println!("-------------------");
          println!("{:#?}", ir);
        }

        for (name, ir) in &self.irs {
          println!("File {}:", name);
          println!("-------------------");
          println!("{:#?}", ir);
        }
      }
    }
  }
}
