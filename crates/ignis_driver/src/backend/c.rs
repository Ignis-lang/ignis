use ignis_codegen_c::{
  emit_c, emit_std_header, emit_std_module_c, emit_std_module_h, emit_user_c, emit_user_module_c, emit_user_module_h,
};

use super::{Backend, BackendRequest, BackendResult};
use crate::stages::{BackendInput, StageError};

pub struct CBackend;

impl Backend for CBackend {
  fn run(
    &self,
    input: BackendInput<'_>,
    request: BackendRequest<'_>,
  ) -> Result<BackendResult, StageError> {
    let contents = match request {
      BackendRequest::EmitCombined {
        namespaces,
        symbols,
        headers,
      } => emit_c(input.program, input.types, input.defs, namespaces, symbols, headers),
      BackendRequest::EmitUserCombined {
        namespaces,
        symbols,
        headers,
        module_paths,
      } => emit_user_c(
        input.program,
        input.types,
        input.defs,
        namespaces,
        symbols,
        headers,
        module_paths,
      ),
      BackendRequest::EmitUserModule {
        module_id,
        namespaces,
        symbols,
        headers,
        module_paths,
        user_module_headers,
      } => emit_user_module_c(
        module_id,
        input.program,
        input.types,
        input.defs,
        namespaces,
        symbols,
        headers,
        module_paths,
        user_module_headers,
      ),
      BackendRequest::EmitUserModuleHeader {
        module_id,
        source_path,
        namespaces,
        symbols,
      } => emit_user_module_h(module_id, source_path, input.defs, input.types, symbols, namespaces),
      BackendRequest::EmitStdModule {
        module_name,
        namespaces,
        symbols,
        headers,
        module_paths,
        umbrella_header_path,
        std_path,
      } => emit_std_module_c(
        module_name,
        input.program,
        input.types,
        input.defs,
        namespaces,
        symbols,
        headers,
        module_paths,
        umbrella_header_path,
        std_path,
      ),
      BackendRequest::EmitStdModuleHeader {
        module_name,
        namespaces,
        symbols,
        module_paths,
      } => emit_std_module_h(module_name, input.defs, input.types, symbols, namespaces, module_paths),
      BackendRequest::EmitStdHeader { namespaces, symbols } => {
        emit_std_header(input.defs, input.types, symbols, namespaces)
      },
    };

    Ok(BackendResult { contents })
  }
}

#[cfg(test)]
mod tests {
  use std::collections::HashMap;
  use std::path::Path;

  use ignis_lir::LirProgram;
  use ignis_type::definition::DefinitionStore;
  use ignis_type::module::{ModuleId, ModulePath};
  use ignis_type::namespace::NamespaceStore;
  use ignis_type::symbol::SymbolTable;
  use ignis_type::types::TypeStore;

  use super::*;
  use crate::backend::{emit_text, BackendRequest};

  #[test]
  fn c_backend_emit_combined_matches_legacy_codegen() {
    let program = LirProgram::new();
    let types = TypeStore::new();
    let defs = DefinitionStore::new();
    let namespaces = NamespaceStore::new();
    let symbols = SymbolTable::new();
    let backend = CBackend;

    let input = BackendInput {
      root_id: ModuleId::new(0),
      types: &types,
      defs: &defs,
      program: &program,
    };

    let emitted = emit_text(
      &backend,
      input,
      BackendRequest::EmitCombined {
        namespaces: &namespaces,
        symbols: &symbols,
        headers: &[],
      },
    )
    .expect("combined emission should succeed");

    assert_eq!(emitted, emit_c(&program, &types, &defs, &namespaces, &symbols, &[]));
  }

  #[test]
  fn c_backend_emit_user_module_header_matches_legacy_codegen() {
    let program = LirProgram::new();
    let types = TypeStore::new();
    let defs = DefinitionStore::new();
    let namespaces = NamespaceStore::new();
    let symbols = SymbolTable::new();
    let backend = CBackend;
    let source_path = Path::new("src/main.ign");

    let input = BackendInput {
      root_id: ModuleId::new(1),
      types: &types,
      defs: &defs,
      program: &program,
    };

    let emitted = emit_text(
      &backend,
      input,
      BackendRequest::EmitUserModuleHeader {
        module_id: ModuleId::new(1),
        source_path,
        namespaces: &namespaces,
        symbols: &symbols,
      },
    )
    .expect("user module header emission should succeed");

    assert_eq!(
      emitted,
      emit_user_module_h(ModuleId::new(1), source_path, &defs, &types, &symbols, &namespaces)
    );
  }

  #[test]
  fn c_backend_emit_std_module_matches_legacy_codegen() {
    let program = LirProgram::new();
    let types = TypeStore::new();
    let defs = DefinitionStore::new();
    let namespaces = NamespaceStore::new();
    let symbols = SymbolTable::new();
    let backend = CBackend;
    let module_paths: HashMap<ModuleId, ModulePath> = HashMap::new();

    let input = BackendInput {
      root_id: ModuleId::new(0),
      types: &types,
      defs: &defs,
      program: &program,
    };

    let emitted = emit_text(
      &backend,
      input,
      BackendRequest::EmitStdModule {
        module_name: "math",
        namespaces: &namespaces,
        symbols: &symbols,
        headers: &[],
        module_paths: &module_paths,
        umbrella_header_path: None,
        std_path: Path::new("std"),
      },
    )
    .expect("std module emission should succeed");

    assert_eq!(
      emitted,
      emit_std_module_c(
        "math",
        &program,
        &types,
        &defs,
        &namespaces,
        &symbols,
        &[],
        &module_paths,
        None,
        Path::new("std"),
      )
    );
  }
}
