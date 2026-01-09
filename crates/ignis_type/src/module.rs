use std::path::PathBuf;

use crate::{Id, Store, definition::DefinitionId, file::FileId, span::Span, symbol::SymbolId};

pub type ModuleId = Id<Module>;

/// State used for cycle detection during module discovery
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ModuleState {
  /// Not yet visited
  Unprocessed,
  /// Currently being processed (on the DFS stack)
  InProgress,
  /// Fully processed
  Processed,
}

/// Canonical path to a module
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ModulePath {
  /// Standard library module: std::io -> Std("io")
  Std(String),
  /// Project-relative path (resolved to absolute)
  Project(PathBuf),
}

impl ModulePath {
  pub fn from_import_path(
    import_from: &str,
    current_file: &std::path::Path,
    project_root: Option<&std::path::Path>,
  ) -> Result<Self, ModulePathError> {
    // 1. Standard library
    if let Some(module_name) = import_from.strip_prefix("std::") {
      return Ok(ModulePath::Std(module_name.to_string()));
    }

    // 2. Relative path (./ or ../)
    if import_from.starts_with("./") || import_from.starts_with("../") {
      let current_dir = current_file
        .parent()
        .ok_or(ModulePathError::InvalidCurrentFile)?;

      let resolved = current_dir.join(import_from);
      let with_ext = if resolved.extension().is_none() {
        resolved.with_extension("ign")
      } else {
        resolved
      };

      return Ok(ModulePath::Project(with_ext));
    }

    // 3. Absolute from project root
    if let Some(root) = project_root {
      let resolved = root.join(import_from);
      let with_ext = if resolved.extension().is_none() {
        resolved.with_extension("ign")
      } else {
        resolved
      };

      return Ok(ModulePath::Project(with_ext));
    }

    Err(ModulePathError::NoProjectRoot(import_from.to_string()))
  }

  /// Resolve to actual filesystem path (basic, no manifest lookup)
  pub fn to_fs_path(
    &self,
    std_path: &std::path::Path,
  ) -> PathBuf {
    self.to_fs_path_with_manifest_path(std_path, None)
  }

  /// Resolve to filesystem path, optionally using a manifest-provided relative path
  ///
  /// If `manifest_rel_path` is Some, it's used directly (joined with std_path).
  /// Otherwise, falls back to convention:
  /// 1. Try {std_path}/{module}/mod.ign
  /// 2. Fall back to {std_path}/{module}.ign
  pub fn to_fs_path_with_manifest_path(
    &self,
    std_path: &std::path::Path,
    manifest_rel_path: Option<&str>,
  ) -> PathBuf {
    match self {
      ModulePath::Std(name) => {
        // If manifest provides explicit path, use it
        if let Some(rel_path) = manifest_rel_path {
          return std_path.join(rel_path);
        }

        // Convention fallback: try mod.ign in subdirectory first
        let subpath = name.replace("::", std::path::MAIN_SEPARATOR_STR);
        let mod_path = std_path.join(&subpath).join("mod.ign");
        if mod_path.exists() {
          return mod_path;
        }

        // Fall back to direct .ign file
        std_path.join(subpath).with_extension("ign")
      },
      ModulePath::Project(path) => path.clone(),
    }
  }

  /// Get the module name for std modules
  pub fn std_module_name(&self) -> Option<&str> {
    match self {
      ModulePath::Std(name) => Some(name),
      ModulePath::Project(_) => None,
    }
  }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ModulePathError {
  InvalidCurrentFile,
  NoProjectRoot(String),
}

/// Information about a single import statement
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ImportInfo {
  /// Names being imported
  pub items: Vec<SymbolId>,
  /// Source module index (stored as u32 to avoid recursive trait bounds)
  source_module_idx: u32,
  /// Source span for error reporting
  pub span: Span,
}

impl ImportInfo {
  pub fn new(
    items: Vec<SymbolId>,
    source_module: ModuleId,
    span: Span,
  ) -> Self {
    Self {
      items,
      source_module_idx: source_module.index(),
      span,
    }
  }

  pub fn source_module(&self) -> ModuleId {
    ModuleId::new(self.source_module_idx)
  }
}

/// A compilation module (typically one source file)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Module {
  /// Source file for this module
  pub file_id: FileId,
  /// Canonical path
  pub path: ModulePath,
  /// Exported definitions (populated after bind phase)
  pub exports: Vec<DefinitionId>,
  /// Import statements (populated during discovery)
  pub imports: Vec<ImportInfo>,
  /// State for cycle detection
  pub state: ModuleState,
}

impl Module {
  pub fn new(
    file_id: FileId,
    path: ModulePath,
  ) -> Self {
    Self {
      file_id,
      path,
      exports: Vec::new(),
      imports: Vec::new(),
      state: ModuleState::Unprocessed,
    }
  }
}

/// Storage for all modules in a compilation
#[derive(Debug, Clone)]
pub struct ModuleStore {
  modules: Store<Module>,
}

impl ModuleStore {
  pub fn new() -> Self {
    Self {
      modules: Store::new(),
    }
  }

  pub fn alloc(
    &mut self,
    module: Module,
  ) -> ModuleId {
    self.modules.alloc(module)
  }

  pub fn get(
    &self,
    id: &ModuleId,
  ) -> &Module {
    self.modules.get(id)
  }

  pub fn get_mut(
    &mut self,
    id: &ModuleId,
  ) -> &mut Module {
    self.modules.get_mut(id)
  }

  pub fn iter(&self) -> impl Iterator<Item = (ModuleId, &Module)> {
    self
      .modules
      .get_all()
      .iter()
      .enumerate()
      .map(|(i, m)| (Id::new(i as u32), m))
  }
}

impl Default for ModuleStore {
  fn default() -> Self {
    Self::new()
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use std::path::Path;

  #[test]
  fn std_path_parsing() {
    let result = ModulePath::from_import_path("std::io", Path::new("/project/main.ign"), None);
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), ModulePath::Std("io".to_string()));
  }

  #[test]
  fn std_nested_path_parsing() {
    let result = ModulePath::from_import_path("std::io::file", Path::new("/project/main.ign"), None);
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), ModulePath::Std("io::file".to_string()));
  }

  #[test]
  fn relative_path_current_dir() {
    let result = ModulePath::from_import_path("./utils", Path::new("/project/src/main.ign"), None);
    assert!(result.is_ok());
    match result.unwrap() {
      ModulePath::Project(path) => {
        assert!(path.ends_with("utils.ign"));
        assert!(path.to_string_lossy().contains("src"));
      },
      _ => panic!("Expected Project path"),
    }
  }

  #[test]
  fn relative_path_parent_dir() {
    let result = ModulePath::from_import_path("../lib", Path::new("/project/src/main.ign"), None);
    assert!(result.is_ok());
    match result.unwrap() {
      ModulePath::Project(path) => {
        assert!(path.ends_with("lib.ign"));
      },
      _ => panic!("Expected Project path"),
    }
  }

  #[test]
  fn absolute_path_with_project_root() {
    let result = ModulePath::from_import_path(
      "utils/math",
      Path::new("/project/src/main.ign"),
      Some(Path::new("/project")),
    );
    assert!(result.is_ok());
    match result.unwrap() {
      ModulePath::Project(path) => {
        assert!(path.ends_with("math.ign"));
        assert!(path.to_string_lossy().contains("utils"));
      },
      _ => panic!("Expected Project path"),
    }
  }

  #[test]
  fn absolute_path_without_project_root_fails() {
    let result = ModulePath::from_import_path("utils/math", Path::new("/project/src/main.ign"), None);
    assert!(result.is_err());
    assert_eq!(
      result.unwrap_err(),
      ModulePathError::NoProjectRoot("utils/math".to_string())
    );
  }

  #[test]
  fn to_fs_path_std() {
    let path = ModulePath::Std("io".to_string());
    let fs_path = path.to_fs_path(Path::new("/usr/lib/ignis/std"));
    assert_eq!(fs_path, PathBuf::from("/usr/lib/ignis/std/io.ign"));
  }

  #[test]
  fn to_fs_path_std_nested() {
    let path = ModulePath::Std("io::file".to_string());
    let fs_path = path.to_fs_path(Path::new("/usr/lib/ignis/std"));
    // On Unix: /usr/lib/ignis/std/io/file.ign
    assert!(fs_path.ends_with("file.ign"));
    assert!(fs_path.to_string_lossy().contains("io"));
  }

  #[test]
  fn to_fs_path_project() {
    let path = ModulePath::Project(PathBuf::from("/project/src/utils.ign"));
    let fs_path = path.to_fs_path(Path::new("/usr/lib/ignis/std"));
    assert_eq!(fs_path, PathBuf::from("/project/src/utils.ign"));
  }

  #[test]
  fn path_with_extension_preserved() {
    let result = ModulePath::from_import_path("./utils.ign", Path::new("/project/src/main.ign"), None);
    assert!(result.is_ok());
    match result.unwrap() {
      ModulePath::Project(path) => {
        // Should keep the .ign extension, not add another
        assert!(path.to_string_lossy().ends_with("utils.ign"));
        assert!(!path.to_string_lossy().ends_with("utils.ign.ign"));
      },
      _ => panic!("Expected Project path"),
    }
  }

  #[test]
  fn module_store_alloc_and_get() {
    use crate::file::SourceMap;

    let mut sm = SourceMap::new();
    let file_id = sm.add_file("test.ign", "".to_string());

    let mut store = ModuleStore::new();
    let module = Module::new(file_id, ModulePath::Std("test".to_string()));
    let id = store.alloc(module);

    let retrieved = store.get(&id);
    assert_eq!(retrieved.path, ModulePath::Std("test".to_string()));
    assert_eq!(retrieved.state, ModuleState::Unprocessed);
  }

  #[test]
  fn import_info_source_module_roundtrip() {
    let module_id = ModuleId::new(42);
    let info = ImportInfo::new(vec![], module_id, Span::default());
    assert_eq!(info.source_module().index(), 42);
  }

  #[test]
  fn std_module_name_returns_name_for_std() {
    let path = ModulePath::Std("io".to_string());
    assert_eq!(path.std_module_name(), Some("io"));
  }

  #[test]
  fn std_module_name_returns_none_for_project() {
    let path = ModulePath::Project(PathBuf::from("/project/utils.ign"));
    assert_eq!(path.std_module_name(), None);
  }

  #[test]
  fn to_fs_path_with_manifest_uses_manifest_path() {
    let path = ModulePath::Std("io".to_string());
    let fs_path = path.to_fs_path_with_manifest_path(
      Path::new("/usr/lib/ignis/std"),
      Some("io/mod.ign"),
    );
    assert_eq!(fs_path, PathBuf::from("/usr/lib/ignis/std/io/mod.ign"));
  }

  #[test]
  fn to_fs_path_with_manifest_fallback_no_manifest() {
    let path = ModulePath::Std("io".to_string());
    let fs_path = path.to_fs_path_with_manifest_path(Path::new("/usr/lib/ignis/std"), None);
    // Without manifest and without existing mod.ign directory, falls back to .ign
    assert_eq!(fs_path, PathBuf::from("/usr/lib/ignis/std/io.ign"));
  }

  #[test]
  fn to_fs_path_with_manifest_project_ignores_manifest() {
    let path = ModulePath::Project(PathBuf::from("/project/utils.ign"));
    let fs_path = path.to_fs_path_with_manifest_path(
      Path::new("/usr/lib/ignis/std"),
      Some("should/be/ignored.ign"),
    );
    // Project paths ignore manifest
    assert_eq!(fs_path, PathBuf::from("/project/utils.ign"));
  }

  #[test]
  fn to_fs_path_with_manifest_nested_std_module() {
    let path = ModulePath::Std("collections::vec".to_string());
    let fs_path = path.to_fs_path_with_manifest_path(
      Path::new("/usr/lib/ignis/std"),
      Some("collections/vec/mod.ign"),
    );
    assert_eq!(
      fs_path,
      PathBuf::from("/usr/lib/ignis/std/collections/vec/mod.ign")
    );
  }
}
