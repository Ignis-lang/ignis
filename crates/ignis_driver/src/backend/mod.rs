pub mod c;

use std::collections::HashMap;
use std::path::Path;

use ignis_config::{CHeader, TargetBackend};
use ignis_type::module::{ModuleId, ModulePath};
use ignis_type::namespace::NamespaceStore;
use ignis_type::symbol::SymbolTable;

use crate::stages::{BackendInput, StageError};

pub enum SelectedBackend {
  C(c::CBackend),
}

impl Backend for SelectedBackend {
  fn run(
    &self,
    input: BackendInput<'_>,
    request: BackendRequest<'_>,
  ) -> Result<BackendResult, StageError> {
    match self {
      Self::C(backend) => backend.run(input, request),
    }
  }
}

pub struct BackendResult {
  pub contents: String,
}

pub enum BackendRequest<'a> {
  Header(HeaderBackendRequest<'a>),
  Lowered(LoweredBackendRequest<'a>),
}

pub enum HeaderBackendRequest<'a> {
  EmitUserModuleHeader {
    module_id: ModuleId,
    source_path: &'a Path,
    namespaces: &'a NamespaceStore,
    symbols: &'a SymbolTable,
  },
  EmitStdModuleHeader {
    module_name: &'a str,
    namespaces: &'a NamespaceStore,
    symbols: &'a SymbolTable,
    module_paths: &'a HashMap<ModuleId, ModulePath>,
  },
  EmitStdHeader {
    namespaces: &'a NamespaceStore,
    symbols: &'a SymbolTable,
  },
}

pub enum LoweredBackendRequest<'a> {
  EmitCombined {
    namespaces: &'a NamespaceStore,
    symbols: &'a SymbolTable,
    headers: &'a [CHeader],
  },
  EmitUserCombined {
    namespaces: &'a NamespaceStore,
    symbols: &'a SymbolTable,
    headers: &'a [CHeader],
    module_paths: &'a HashMap<ModuleId, ModulePath>,
  },
  EmitUserModule {
    module_id: ModuleId,
    namespaces: &'a NamespaceStore,
    symbols: &'a SymbolTable,
    headers: &'a [CHeader],
    module_paths: &'a HashMap<ModuleId, ModulePath>,
    user_module_headers: &'a [CHeader],
  },
  EmitStdModule {
    module_name: &'a str,
    namespaces: &'a NamespaceStore,
    symbols: &'a SymbolTable,
    headers: &'a [CHeader],
    module_paths: &'a HashMap<ModuleId, ModulePath>,
    umbrella_header_path: Option<&'a str>,
    std_path: &'a Path,
  },
}

pub trait Backend {
  fn run(
    &self,
    input: BackendInput<'_>,
    request: BackendRequest<'_>,
  ) -> Result<BackendResult, StageError>;
}

pub fn emit_text<B: Backend + ?Sized>(
  backend: &B,
  input: BackendInput<'_>,
  request: BackendRequest<'_>,
) -> Result<String, StageError> {
  input.verify_for(&request)?;
  backend.run(input, request).map(|result| result.contents)
}

pub fn select_backend(target: TargetBackend) -> Result<SelectedBackend, StageError> {
  match target {
    TargetBackend::C => Ok(SelectedBackend::C(c::CBackend)),
    backend => Err(StageError::UnsupportedBackend { backend }),
  }
}

#[cfg(test)]
mod tests {
  use ignis_lir::LirProgram;
  use ignis_type::definition::{Definition, DefinitionKind, DefinitionStore, Visibility};
  use ignis_type::module::ModuleId;
  use ignis_type::namespace::NamespaceStore;
  use ignis_type::span::Span;
  use ignis_type::symbol::SymbolTable;
  use ignis_type::types::TypeStore;

  use ignis_config::TargetBackend;

  use super::{emit_text, select_backend, Backend, BackendRequest, BackendResult, LoweredBackendRequest};
  use crate::stages::{BackendInput, StageError};

  struct RecordingBackend;

  fn lowered_input_fixture<'a>(
    types: &'a TypeStore,
    defs: &'a DefinitionStore,
    program: &'a LirProgram,
  ) -> BackendInput<'a> {
    let root_id = ModuleId::new(0);

    BackendInput::Lowered {
      root_id,
      types,
      defs,
      program,
    }
  }

  fn alloc_placeholder_definition(
    defs: &mut DefinitionStore,
    owner_module: ModuleId,
  ) {
    defs.alloc(Definition {
      kind: DefinitionKind::Placeholder,
      name: Default::default(),
      span: Span::default(),
      name_span: Span::default(),
      visibility: Visibility::Private,
      owner_module,
      owner_namespace: None,
      doc: None,
    });
  }

  impl Backend for RecordingBackend {
    fn run(
      &self,
      _input: BackendInput<'_>,
      _request: BackendRequest<'_>,
    ) -> Result<BackendResult, StageError> {
      Ok(BackendResult {
        contents: "ok".to_string(),
      })
    }
  }

  #[test]
  fn select_backend_accepts_c_backend() {
    assert!(select_backend(TargetBackend::C).is_ok());
  }

  #[test]
  fn select_backend_rejects_unimplemented_backend() {
    let result = select_backend(TargetBackend::Iir);

    assert!(matches!(
      result,
      Err(StageError::UnsupportedBackend {
        backend: TargetBackend::Iir,
      })
    ));
  }

  #[test]
  fn emit_text_rejects_header_input_for_lowered_requests_before_backend_runs() {
    let backend = RecordingBackend;
    let types = TypeStore::new();
    let defs = DefinitionStore::new();
    let namespaces = NamespaceStore::new();
    let symbols = SymbolTable::new();

    let result = emit_text(
      &backend,
      BackendInput::Header {
        types: &types,
        defs: &defs,
      },
      BackendRequest::Lowered(LoweredBackendRequest::EmitCombined {
        namespaces: &namespaces,
        symbols: &symbols,
        headers: &[],
      }),
    );

    assert_eq!(result, Err(StageError::BackendRequestRequiresLoweredInput));
  }

  #[test]
  fn emit_text_allows_lowered_input_for_lowered_requests() {
    let backend = RecordingBackend;
    let types = TypeStore::new();
    let mut defs = DefinitionStore::new();
    let namespaces = NamespaceStore::new();
    let symbols = SymbolTable::new();
    let program = LirProgram::new();

    alloc_placeholder_definition(&mut defs, ModuleId::new(0));

    let result = emit_text(
      &backend,
      lowered_input_fixture(&types, &defs, &program),
      BackendRequest::Lowered(LoweredBackendRequest::EmitCombined {
        namespaces: &namespaces,
        symbols: &symbols,
        headers: &[],
      }),
    );

    assert_eq!(result, Ok("ok".to_string()));
  }
}
