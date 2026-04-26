use std::sync::OnceLock;

use ignis_codegen_c::{
  emit_c_from_input, emit_std_header_from_input, emit_std_module_c_from_input, emit_std_module_h_from_input,
  emit_user_c_from_input, emit_user_module_c_from_input, emit_user_module_h_from_input,
  emit_user_test_harness_from_input, EmitInput, TestHarnessEntry,
};
use ignis_lir::LirProgram;
use ignis_type::definition::DefinitionStore;
use ignis_type::types::TypeStore;

use super::{Backend, BackendRequest, BackendResult, HeaderBackendRequest, LoweredBackendRequest};
use crate::stages::{BackendInput, StageError};

pub struct CBackend;

fn header_emit_input<'a>(
  program: Option<&'a LirProgram>,
  types: &'a TypeStore,
  defs: &'a DefinitionStore,
) -> EmitInput<'a> {
  static EMPTY_PROGRAM: OnceLock<LirProgram> = OnceLock::new();

  EmitInput::new(
    program.unwrap_or_else(|| EMPTY_PROGRAM.get_or_init(LirProgram::new)),
    types,
    defs,
  )
}

impl Backend for CBackend {
  fn run(
    &self,
    input: BackendInput<'_>,
    request: BackendRequest<'_>,
  ) -> Result<BackendResult, StageError> {
    let contents = match request {
      BackendRequest::Header(request) => {
        let emit_input = match input {
          BackendInput::Header { types, defs } => header_emit_input(None, types, defs),
          BackendInput::Lowered {
            types, defs, program, ..
          } => header_emit_input(Some(program), types, defs),
        };

        match request {
          HeaderBackendRequest::EmitUserModuleHeader {
            module_id,
            source_path,
            namespaces,
            symbols,
          } => emit_user_module_h_from_input(module_id, source_path, emit_input, symbols, namespaces),
          HeaderBackendRequest::EmitStdModuleHeader {
            module_name,
            namespaces,
            symbols,
            module_paths,
          } => emit_std_module_h_from_input(module_name, emit_input, symbols, namespaces, module_paths),
          HeaderBackendRequest::EmitStdHeader { namespaces, symbols } => {
            emit_std_header_from_input(emit_input, symbols, namespaces)
          },
        }
      },
      BackendRequest::Lowered(request) => {
        let BackendInput::Lowered {
          types, defs, program, ..
        } = input
        else {
          return Err(StageError::BackendRequestRequiresLoweredInput);
        };

        let emit_input = EmitInput::new(program, types, defs);

        match request {
          LoweredBackendRequest::EmitCombined {
            namespaces,
            symbols,
            headers,
          } => emit_c_from_input(emit_input, namespaces, symbols, headers),
          LoweredBackendRequest::EmitUserCombined {
            namespaces,
            symbols,
            headers,
            module_paths,
          } => emit_user_c_from_input(emit_input, namespaces, symbols, headers, module_paths),
          LoweredBackendRequest::EmitUserTestHarness {
            namespaces,
            symbols,
            headers,
            module_paths,
            plan,
          } => emit_user_test_harness_from_input(
            emit_input,
            namespaces,
            symbols,
            headers,
            module_paths,
            &plan
              .tests
              .iter()
              .map(|test| TestHarnessEntry {
                def_id: test.def_id,
                fq_name: test.fq_name.clone(),
              })
              .collect::<Vec<_>>(),
          ),
          LoweredBackendRequest::EmitUserModule {
            module_id,
            namespaces,
            symbols,
            headers,
            module_paths,
            user_module_headers,
            std_path,
          } => emit_user_module_c_from_input(
            module_id,
            emit_input,
            namespaces,
            symbols,
            headers,
            module_paths,
            user_module_headers,
            std_path,
          ),
          LoweredBackendRequest::EmitStdModule {
            module_name,
            namespaces,
            symbols,
            headers,
            module_paths,
            umbrella_header_path,
            std_path,
          } => emit_std_module_c_from_input(
            module_name,
            emit_input,
            namespaces,
            symbols,
            headers,
            module_paths,
            umbrella_header_path,
            std_path,
          ),
        }
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
  use ignis_codegen_c::{emit_c, emit_std_module_c, emit_user_module_h};
  use ignis_type::definition::{Definition, DefinitionKind, DefinitionStore, Visibility};
  use ignis_type::module::{ModuleId, ModulePath};
  use ignis_type::namespace::NamespaceStore;
  use ignis_type::span::Span;
  use ignis_type::symbol::SymbolTable;
  use ignis_type::types::TypeStore;

  use super::*;
  use crate::backend::{emit_text, BackendRequest, HeaderBackendRequest, LoweredBackendRequest};

  fn alloc_placeholder_definition(
    defs: &mut DefinitionStore,
    symbols: &mut SymbolTable,
    owner_module: ModuleId,
  ) {
    let name = symbols.intern("__test_placeholder");

    defs.alloc(Definition {
      kind: DefinitionKind::Placeholder,
      name,
      span: Span::default(),
      name_span: Span::default(),
      visibility: Visibility::Private,
      owner_module,
      owner_namespace: None,
      doc: None,
    });
  }

  #[test]
  fn c_backend_emit_combined_matches_legacy_codegen() {
    let program = LirProgram::new();
    let types = TypeStore::new();
    let mut defs = DefinitionStore::new();
    let namespaces = NamespaceStore::new();
    let mut symbols = SymbolTable::new();
    let backend = CBackend;

    alloc_placeholder_definition(&mut defs, &mut symbols, ModuleId::new(0));

    let input = BackendInput::Lowered {
      root_id: ModuleId::new(0),
      types: &types,
      defs: &defs,
      program: &program,
    };

    let emitted = emit_text(
      &backend,
      input,
      BackendRequest::Lowered(LoweredBackendRequest::EmitCombined {
        namespaces: &namespaces,
        symbols: &symbols,
        headers: &[],
      }),
    )
    .expect("combined emission should succeed");

    assert_eq!(emitted, emit_c(&program, &types, &defs, &namespaces, &symbols, &[]));

    let codegen_input = EmitInput::new(&program, &types, &defs);
    assert_eq!(emitted, emit_c_from_input(codegen_input, &namespaces, &symbols, &[]));
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

    let emitted = emit_text(
      &backend,
      BackendInput::Header {
        types: &types,
        defs: &defs,
      },
      BackendRequest::Header(HeaderBackendRequest::EmitUserModuleHeader {
        module_id: ModuleId::new(1),
        source_path,
        namespaces: &namespaces,
        symbols: &symbols,
      }),
    )
    .expect("user module header emission should succeed");

    assert_eq!(
      emitted,
      emit_user_module_h(ModuleId::new(1), source_path, &defs, &types, &symbols, &namespaces)
    );

    let codegen_input = EmitInput::new(&program, &types, &defs);
    assert_eq!(
      emitted,
      emit_user_module_h_from_input(ModuleId::new(1), source_path, codegen_input, &symbols, &namespaces)
    );
  }

  #[test]
  fn c_backend_emit_std_module_matches_legacy_codegen() {
    let program = LirProgram::new();
    let types = TypeStore::new();
    let mut defs = DefinitionStore::new();
    let namespaces = NamespaceStore::new();
    let mut symbols = SymbolTable::new();
    let backend = CBackend;
    let module_paths: HashMap<ModuleId, ModulePath> = HashMap::new();

    alloc_placeholder_definition(&mut defs, &mut symbols, ModuleId::new(0));

    let input = BackendInput::Lowered {
      root_id: ModuleId::new(0),
      types: &types,
      defs: &defs,
      program: &program,
    };

    let emitted = emit_text(
      &backend,
      input,
      BackendRequest::Lowered(LoweredBackendRequest::EmitStdModule {
        module_name: "math",
        namespaces: &namespaces,
        symbols: &symbols,
        headers: &[],
        module_paths: &module_paths,
        umbrella_header_path: None,
        std_path: Path::new("std"),
      }),
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

    let codegen_input = EmitInput::new(&program, &types, &defs);
    assert_eq!(
      emitted,
      emit_std_module_c_from_input(
        "math",
        codegen_input,
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
