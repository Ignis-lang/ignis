use std::collections::HashMap;

use ignis_config::{DebugPrint, IgnisConfig};
use ignis_hir::HIRInstruction;
use ir::{ir::IRProgramInstruction, IRGenerator};

pub mod c;
pub mod ir;

pub struct IgnisBackend<'a> {
  config: &'a IgnisConfig,
  hir_ffi_std: HashMap<String, Vec<HIRInstruction>>,
  hirs: HashMap<String, Vec<HIRInstruction>>,
  irs: HashMap<String, Vec<IRProgramInstruction>>,
  ir_std: HashMap<String, Vec<IRProgramInstruction>>,
  header_map: HashMap<String, Vec<String>>,
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
      header_map: HashMap::new(),
    }
  }

  pub fn process(&mut self) {
    let mut ir_generator = IRGenerator::new(&self.config);

    if !self.hir_ffi_std.is_empty() {
      ir_generator.process(&self.hir_ffi_std);
      self.ir_std.clone_from(&ir_generator.programs_by_file);
    }

    ir_generator.process(&self.hirs);

    self.irs.clone_from(&ir_generator.programs_by_file);
    self.header_map.clone_from(&ir_generator.header_map);

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

    self.compile();
  }

  pub fn compile(&mut self) {
    let target = self.config.build_config.as_ref().unwrap().target.clone();

    if !self.config.build_config.as_ref().unwrap().output_dir.is_empty() {
      std::fs::create_dir_all(self.config.build_config.as_ref().unwrap().output_dir.clone());
    }

    match target {
      ignis_config::TargetBackend::C => {
        let mut c = c::IgnisCBackend::new(self.config, &self.irs, &self.header_map);
        c.compile();
      },
      ignis_config::TargetBackend::Bytecode => todo!(),
      ignis_config::TargetBackend::Iir => todo!(),
      ignis_config::TargetBackend::None => todo!(),
    }
  }
}
