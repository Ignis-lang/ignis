pub mod c;

use std::collections::HashMap;
use std::path::Path;

use ignis_config::CHeader;
use ignis_type::module::{ModuleId, ModulePath};
use ignis_type::namespace::NamespaceStore;
use ignis_type::symbol::SymbolTable;

use crate::stages::{BackendInput, StageError};

pub struct BackendResult {
  pub contents: String,
}

pub enum BackendRequest<'a> {
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
  EmitUserModuleHeader {
    module_id: ModuleId,
    source_path: &'a Path,
    namespaces: &'a NamespaceStore,
    symbols: &'a SymbolTable,
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
  backend.run(input, request).map(|result| result.contents)
}
