mod classify;
mod emit;

pub use classify::{build_module_paths, classify_def, DefKind, EmitTarget};
pub use emit::{
  emit_c, emit_std_header, emit_std_module_c, emit_std_module_h, emit_user_c, emit_user_module_c, emit_user_module_h,
  format_c_type, CEmitter,
};
