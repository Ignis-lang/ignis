mod classify;
mod emit;
mod toolchain;

/// Bump this when the C output format changes in a way that requires rebuild.
pub const CODEGEN_ABI_VERSION: u32 = 2;

pub use classify::{build_module_paths, classify_def, DefKind, EmitTarget};
pub use emit::{
  emit_c, emit_entry_wrapper_c, emit_std_header, emit_std_module_c, emit_std_module_h, emit_user_c, emit_user_module_c,
  emit_user_module_h, format_c_type, mangle_symbol_name, CEmitter,
};
pub use toolchain::compile_c_to_object;
