use std::collections::HashMap;
use std::path::Path;

use ignis_config::{CHeader, TargetBackend};
use ignis_lir::LirProgram;
use ignis_type::{
  definition::DefinitionStore,
  module::{ModuleId, ModulePath},
  namespace::NamespaceStore,
  symbol::SymbolTable,
  types::TypeStore,
};

use crate::link::LinkPlan;

pub struct BackendEmitContext<'a> {
  pub program: &'a LirProgram,
  pub types: &'a TypeStore,
  pub defs: &'a DefinitionStore,
  pub namespaces: &'a NamespaceStore,
  pub symbols: &'a SymbolTable,
  pub headers: &'a [CHeader],
  pub module_paths: &'a HashMap<ModuleId, ModulePath>,
  pub emit_user_only: bool,
}

pub trait CodegenBackend {
  fn source_extension(&self) -> &'static str;

  fn emit_program(
    &self,
    context: &BackendEmitContext<'_>,
  ) -> Result<String, String>;

  fn emit_entry_wrapper(
    &self,
    _context: &BackendEmitContext<'_>,
  ) -> Result<Option<String>, String> {
    Ok(None)
  }

  fn compile_source_to_object(
    &self,
    source_path: &Path,
    object_path: &Path,
    link_plan: &LinkPlan,
    quiet: bool,
  ) -> Result<(), String>;
}

pub struct CBackend;

impl CodegenBackend for CBackend {
  fn source_extension(&self) -> &'static str {
    "c"
  }

  fn emit_program(
    &self,
    context: &BackendEmitContext<'_>,
  ) -> Result<String, String> {
    if context.emit_user_only {
      Ok(ignis_codegen_c::emit_user_c(
        context.program,
        context.types,
        context.defs,
        context.namespaces,
        context.symbols,
        context.headers,
        context.module_paths,
      ))
    } else {
      Ok(ignis_codegen_c::emit_c(
        context.program,
        context.types,
        context.defs,
        context.namespaces,
        context.symbols,
        context.headers,
      ))
    }
  }

  fn compile_source_to_object(
    &self,
    source_path: &Path,
    object_path: &Path,
    link_plan: &LinkPlan,
    quiet: bool,
  ) -> Result<(), String> {
    ignis_codegen_c::compile_c_to_object(
      source_path,
      object_path,
      &link_plan.cc,
      &link_plan.cflags,
      &link_plan.include_dirs,
      quiet,
    )
  }
}

pub struct QbeBackend;

impl CodegenBackend for QbeBackend {
  fn source_extension(&self) -> &'static str {
    "qbe"
  }

  fn emit_program(
    &self,
    context: &BackendEmitContext<'_>,
  ) -> Result<String, String> {
    ignis_codegen_qbe::emit_qbe(
      context.program,
      context.types,
      context.defs,
      context.namespaces,
      context.symbols,
      &ignis_codegen_qbe::QbeEmitOptions::default(),
    )
    .map_err(|error| error.to_string())
  }

  fn emit_entry_wrapper(
    &self,
    _context: &BackendEmitContext<'_>,
  ) -> Result<Option<String>, String> {
    let wrapper = "\
#include <stdlib.h>\n\
extern int __ignis_user_main(void);\n\
int main(int argc, char** argv) {\n\
    (void)argc; (void)argv;\n\
    return __ignis_user_main();\n\
}\n";
    Ok(Some(wrapper.to_string()))
  }

  fn compile_source_to_object(
    &self,
    source_path: &Path,
    object_path: &Path,
    link_plan: &LinkPlan,
    quiet: bool,
  ) -> Result<(), String> {
    ignis_codegen_qbe::compile_qbe_to_object(source_path, object_path, &link_plan.cc, &link_plan.cflags, quiet)
  }
}

pub fn backend_for_target(target: TargetBackend) -> Box<dyn CodegenBackend> {
  match target {
    TargetBackend::Qbe => Box::new(QbeBackend),
    TargetBackend::C | TargetBackend::Iir | TargetBackend::None => Box::new(CBackend),
  }
}
