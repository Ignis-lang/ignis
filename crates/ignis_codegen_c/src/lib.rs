mod classify;
mod emit;

use ignis_lir::LirProgram;
use ignis_type::definition::DefinitionStore;
use ignis_type::types::TypeStore;

/// Bump this when the C output format changes in a way that requires rebuild.
pub const CODEGEN_ABI_VERSION: u32 = 2;

#[derive(Clone, Copy)]
pub struct EmitInput<'a> {
  pub program: &'a LirProgram,
  pub types: &'a TypeStore,
  pub defs: &'a DefinitionStore,
}

impl<'a> EmitInput<'a> {
  pub fn new(
    program: &'a LirProgram,
    types: &'a TypeStore,
    defs: &'a DefinitionStore,
  ) -> Self {
    Self { program, types, defs }
  }
}

pub use classify::{build_module_paths, classify_def, DefKind, EmitTarget};
pub use emit::{
  emit_c, emit_c_from_input, emit_std_header, emit_std_header_from_input, emit_std_module_c,
  emit_std_module_c_from_input, emit_std_module_h, emit_std_module_h_from_input, emit_user_c, emit_user_c_from_input,
  emit_user_module_c, emit_user_module_c_from_input, emit_user_module_h, emit_user_module_h_from_input, format_c_type,
  CEmitter,
};
