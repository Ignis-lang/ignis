pub mod backend;
pub mod diagnostics;
pub mod frontend;

use ignis_config::IgnisConfig;
use frontend::IgnisFrontend;

pub struct IgnisCore {
  config: Box<IgnisConfig>,
}

impl IgnisCore {
  pub fn new(config: &IgnisConfig) -> Self {
    Self {
      config: Box::new(config.clone()),
    }
  }

  pub fn run(&mut self) {
    if self.config.build {
      self.build();
      return;
    }

    if self.config.init {
      self.init();
      return;
    }
  }

  pub fn build(&mut self) {
    let mut frontend = IgnisFrontend::new(&self.config);
    frontend.process();

    let mut backend = backend::IgnisBackend::new(&self.config, frontend.hir_ffi_std, frontend.hirs);
    backend.process();
  }

  pub fn init(&mut self) {
    todo!()
  }

  pub fn lsp(&mut self) {
    todo!()
  }
}
