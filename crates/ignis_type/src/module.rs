use std::collections::HashMap;
use std::path::{Component, PathBuf};

use crate::{Id, Store, definition::DefinitionId, file::FileId, span::Span, symbol::SymbolId};

/// Normalize a path by resolving `.` and `..` components without requiring the file to exist.
///
/// Unlike `std::fs::canonicalize`, this does not follow symlinks or check file existence.
fn normalize_path(path: PathBuf) -> PathBuf {
  let mut components = Vec::new();

  for component in path.components() {
    match component {
      Component::CurDir => {
        // Skip `.`
      },
      Component::ParentDir => {
        // Pop the last component if it's a normal component
        if let Some(Component::Normal(_)) = components.last() {
          components.pop();
        } else {
          // Keep `..` if we can't go up (e.g., at root or only `..` remain)
          components.push(component);
        }
      },
      _ => {
        components.push(component);
      },
    }
  }

  components.iter().collect()
}

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
  /// Resolve an import specifier to a canonical `ModulePath`.
  ///
  /// Resolution order:
  /// 1. **Alias match** — first segment (before `::`) is checked against `aliases`.
  ///    Exact segment match is tried first (`"mylib"` in `"mylib::utils"`), then
  ///    prefix match for shorter alias keys (`"@"` in `"@token::token"`).
  ///    `"std"` produces `ModulePath::Std`; other aliases produce `ModulePath::Project`
  ///    with `.ign` / `mod.ign` convention fallback.
  /// 2. **Relative path** (`./`, `../`) — resolved from `current_file`'s parent.
  /// 3. **Bare path** — resolved from `project_root` (errors if absent).
  pub fn from_import_path(
    import_from: &str,
    current_file: &std::path::Path,
    project_root: Option<&std::path::Path>,
    aliases: &HashMap<String, PathBuf>,
  ) -> Result<Self, ModulePathError> {
    // 1. Alias match: split on first "::" and check the alias map.
    //
    // Two matching strategies:
    //   a) Exact segment match — first segment == alias key (e.g. "mylib::utils" with alias "mylib")
    //   b) Prefix match — alias key is a proper prefix of first segment (e.g. "@token::token"
    //      with alias "@"). The remainder after stripping the prefix becomes an extra path segment.
    let (first_segment, rest) = match import_from.split_once("::") {
      Some((first, rest)) => (first, Some(rest)),
      None => (import_from, None),
    };

    // Try exact segment match first
    let alias_match = aliases.get(first_segment).map(|base| (base, ""));

    // If no exact match, try prefix match: find the longest alias key that is a
    // proper prefix of first_segment. Longest-first avoids ambiguity when both
    // "@" and "@token" exist as aliases.
    let alias_match = alias_match.or_else(|| {
      aliases
        .iter()
        .filter(|(key, _)| key.len() < first_segment.len() && first_segment.starts_with(key.as_str()))
        .max_by_key(|(key, _)| key.len())
        .map(|(key, base)| (base, &first_segment[key.len()..]))
    });

    if let Some((base_path, prefix_remainder)) = alias_match {
      // "std" alias preserves ModulePath::Std semantics
      if first_segment == "std" {
        let module_name = rest.ok_or(ModulePathError::EmptyStdModuleName)?;
        if module_name.is_empty() {
          return Err(ModulePathError::EmptyStdModuleName);
        }
        return Ok(ModulePath::Std(module_name.to_string()));
      }

      // Build the sub-path by combining the prefix remainder with the rest.
      // For "@token::token" with alias "@" → prefix_remainder="token", rest="token"
      // → segments = ["token", "token"] → subpath = "token/token"
      let subpath = match (prefix_remainder.is_empty(), rest) {
        (true, Some(r)) => r.replace("::", std::path::MAIN_SEPARATOR_STR),
        (false, Some(r)) => {
          let combined = format!("{}::{}", prefix_remainder, r);
          combined.replace("::", std::path::MAIN_SEPARATOR_STR)
        },
        (true, None) => String::new(),
        (false, None) => prefix_remainder.replace("::", std::path::MAIN_SEPARATOR_STR),
      };

      let resolved = if subpath.is_empty() {
        let mod_path = base_path.join("mod.ign");
        if mod_path.exists() {
          return Ok(ModulePath::Project(mod_path));
        }
        base_path.with_extension("ign")
      } else {
        let full = base_path.join(&subpath);
        let mod_path = full.join("mod.ign");
        if mod_path.exists() {
          return Ok(ModulePath::Project(mod_path));
        }
        full.with_extension("ign")
      };

      return Ok(ModulePath::Project(normalize_path(resolved)));
    }

    // 2. Relative path (./ or ../)
    if import_from.starts_with("./") || import_from.starts_with("../") {
      let current_dir = current_file.parent().ok_or(ModulePathError::InvalidCurrentFile)?;

      let resolved = current_dir.join(import_from);
      let with_ext = if resolved.extension().is_none() {
        resolved.with_extension("ign")
      } else {
        resolved
      };

      return Ok(ModulePath::Project(normalize_path(with_ext)));
    }

    // 3. Bare path from project root
    if let Some(root) = project_root {
      let resolved = root.join(import_from);
      let with_ext = if resolved.extension().is_none() {
        resolved.with_extension("ign")
      } else {
        resolved
      };

      return Ok(ModulePath::Project(normalize_path(with_ext)));
    }

    Err(ModulePathError::NoProjectRoot(import_from.to_string()))
  }

  /// Check if this path refers to a file inside the given directory.
  pub fn is_inside_dir(
    &self,
    dir: &std::path::Path,
  ) -> bool {
    match self {
      ModulePath::Project(path) => path.starts_with(dir),
      ModulePath::Std(_) => false,
    }
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

  pub fn is_std(&self) -> bool {
    matches!(self, ModulePath::Std(_))
  }

  pub fn is_project(&self) -> bool {
    matches!(self, ModulePath::Project(_))
  }

  /// Returns a clean module name for file naming.
  /// For "mod.ign" files, uses the parent directory name.
  pub fn module_name(&self) -> String {
    match self {
      ModulePath::Std(name) => name.replace("::", "_"),
      ModulePath::Project(path) => {
        let stem = path.file_stem().and_then(|s| s.to_str()).unwrap_or("module");
        if stem == "mod"
          && let Some(parent) = path.parent()
          && let Some(dir_name) = parent.file_name().and_then(|s| s.to_str())
        {
          return dir_name.to_string();
        }
        stem.to_string()
      },
    }
  }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ModulePathError {
  InvalidCurrentFile,
  NoProjectRoot(String),
  /// Empty std module name (e.g., "std::" without a module name)
  EmptyStdModuleName,
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
    Self { modules: Store::new() }
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

  fn std_aliases() -> HashMap<String, PathBuf> {
    HashMap::from([("std".to_string(), PathBuf::from("/usr/lib/ignis/std"))])
  }

  #[test]
  fn std_path_parsing() {
    let result = ModulePath::from_import_path("std::io", Path::new("/project/main.ign"), None, &std_aliases());
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), ModulePath::Std("io".to_string()));
  }

  #[test]
  fn std_nested_path_parsing() {
    let result = ModulePath::from_import_path("std::io::file", Path::new("/project/main.ign"), None, &std_aliases());
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), ModulePath::Std("io::file".to_string()));
  }

  #[test]
  fn relative_path_current_dir() {
    let result = ModulePath::from_import_path("./utils", Path::new("/project/src/main.ign"), None, &HashMap::new());
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
    let result = ModulePath::from_import_path("../lib", Path::new("/project/src/main.ign"), None, &HashMap::new());
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
      &HashMap::new(),
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
    let result = ModulePath::from_import_path("utils/math", Path::new("/project/src/main.ign"), None, &HashMap::new());
    assert!(result.is_err());
    assert_eq!(result.unwrap_err(), ModulePathError::NoProjectRoot("utils/math".to_string()));
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
    let result = ModulePath::from_import_path("./utils.ign", Path::new("/project/src/main.ign"), None, &HashMap::new());
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
    let fs_path = path.to_fs_path_with_manifest_path(Path::new("/usr/lib/ignis/std"), Some("io/mod.ign"));
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
    let fs_path = path.to_fs_path_with_manifest_path(Path::new("/usr/lib/ignis/std"), Some("should/be/ignored.ign"));
    // Project paths ignore manifest
    assert_eq!(fs_path, PathBuf::from("/project/utils.ign"));
  }

  #[test]
  fn to_fs_path_with_manifest_nested_std_module() {
    let path = ModulePath::Std("collections::vec".to_string());
    let fs_path = path.to_fs_path_with_manifest_path(Path::new("/usr/lib/ignis/std"), Some("collections/vec/mod.ign"));
    assert_eq!(fs_path, PathBuf::from("/usr/lib/ignis/std/collections/vec/mod.ign"));
  }

  #[test]
  fn normalize_path_removes_dot_components() {
    let path = normalize_path(PathBuf::from("/project/./src/./main.ign"));
    assert_eq!(path, PathBuf::from("/project/src/main.ign"));
  }

  #[test]
  fn normalize_path_resolves_parent_components() {
    let path = normalize_path(PathBuf::from("/project/src/../lib/utils.ign"));
    assert_eq!(path, PathBuf::from("/project/lib/utils.ign"));
  }

  #[test]
  fn normalize_path_handles_multiple_parent_components() {
    let path = normalize_path(PathBuf::from("/project/a/b/../../c/d.ign"));
    assert_eq!(path, PathBuf::from("/project/c/d.ign"));
  }

  #[test]
  fn normalize_path_handles_mixed_components() {
    let path = normalize_path(PathBuf::from("/std/memory/../libc/./primitives.ign"));
    assert_eq!(path, PathBuf::from("/std/libc/primitives.ign"));
  }

  #[test]
  fn relative_paths_with_parent_are_normalized() {
    // Two different import paths that should resolve to the same file
    let no_aliases = HashMap::new();
    let path1 = ModulePath::from_import_path("./primitives", Path::new("/std/libc/mod.ign"), None, &no_aliases);
    let path2 = ModulePath::from_import_path("../libc/primitives", Path::new("/std/memory/mod.ign"), None, &no_aliases);

    assert!(path1.is_ok());
    assert!(path2.is_ok());

    // Both should resolve to the same normalized path
    match (path1.unwrap(), path2.unwrap()) {
      (ModulePath::Project(p1), ModulePath::Project(p2)) => {
        assert_eq!(p1, p2);
      },
      _ => panic!("Expected Project paths"),
    }
  }

  #[test]
  fn alias_resolves_to_project_path() {
    let aliases = HashMap::from([("mylib".to_string(), PathBuf::from("/libs/mylib"))]);
    let result = ModulePath::from_import_path("mylib::utils", Path::new("/project/main.ign"), None, &aliases);
    assert!(result.is_ok());
    match result.unwrap() {
      ModulePath::Project(path) => {
        assert_eq!(path, PathBuf::from("/libs/mylib/utils.ign"));
      },
      _ => panic!("Expected Project path for user alias"),
    }
  }

  #[test]
  fn alias_nested_subpath() {
    let aliases = HashMap::from([("ext".to_string(), PathBuf::from("/ext/packages"))]);
    let result = ModulePath::from_import_path("ext::net::http", Path::new("/project/main.ign"), None, &aliases);
    assert!(result.is_ok());
    match result.unwrap() {
      ModulePath::Project(path) => {
        // "net::http" -> "net/http" -> base.join("net/http").with_extension("ign")
        assert!(path.to_string_lossy().contains("net"));
        assert!(path.to_string_lossy().ends_with("http.ign"));
      },
      _ => panic!("Expected Project path"),
    }
  }

  #[test]
  fn std_alias_produces_std_path() {
    let aliases = HashMap::from([("std".to_string(), PathBuf::from("/usr/lib/ignis/std"))]);
    let result = ModulePath::from_import_path("std::io", Path::new("/project/main.ign"), None, &aliases);
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), ModulePath::Std("io".to_string()));
  }

  #[test]
  fn std_alias_without_module_name_fails() {
    let aliases = HashMap::from([("std".to_string(), PathBuf::from("/std"))]);
    let result = ModulePath::from_import_path("std", Path::new("/project/main.ign"), None, &aliases);
    assert!(result.is_err());
    assert_eq!(result.unwrap_err(), ModulePathError::EmptyStdModuleName);
  }

  #[test]
  fn unmatched_alias_falls_through_to_project_root() {
    let aliases = HashMap::from([("std".to_string(), PathBuf::from("/std"))]);
    let result = ModulePath::from_import_path(
      "utils/math",
      Path::new("/project/main.ign"),
      Some(Path::new("/project")),
      &aliases,
    );
    assert!(result.is_ok());
    match result.unwrap() {
      ModulePath::Project(path) => {
        assert!(path.ends_with("math.ign"));
      },
      _ => panic!("Expected Project path"),
    }
  }

  #[test]
  fn alias_prefix_match_single_char() {
    let aliases = HashMap::from([("@".to_string(), PathBuf::from("/project/src"))]);
    let result = ModulePath::from_import_path("@token::token", Path::new("/project/main.ign"), None, &aliases);
    assert!(result.is_ok());
    match result.unwrap() {
      ModulePath::Project(path) => {
        assert_eq!(path, PathBuf::from("/project/src/token/token.ign"));
      },
      _ => panic!("Expected Project path for prefix alias"),
    }
  }

  #[test]
  fn alias_prefix_match_no_rest() {
    // "@token" with no further :: segments
    let aliases = HashMap::from([("@".to_string(), PathBuf::from("/project/src"))]);
    let result = ModulePath::from_import_path("@token", Path::new("/project/main.ign"), None, &aliases);
    assert!(result.is_ok());
    match result.unwrap() {
      ModulePath::Project(path) => {
        assert_eq!(path, PathBuf::from("/project/src/token.ign"));
      },
      _ => panic!("Expected Project path for prefix alias without rest"),
    }
  }

  #[test]
  fn alias_prefix_match_longest_wins() {
    // Both "@" and "@lib" are aliases; "@lib" should win for "@lib::utils"
    let aliases = HashMap::from([
      ("@".to_string(), PathBuf::from("/project/src")),
      ("@lib".to_string(), PathBuf::from("/project/libs")),
    ]);
    let result = ModulePath::from_import_path("@lib::utils", Path::new("/project/main.ign"), None, &aliases);
    assert!(result.is_ok());
    match result.unwrap() {
      ModulePath::Project(path) => {
        // Exact match on "@lib" should win, so base = /project/libs, subpath = utils
        assert_eq!(path, PathBuf::from("/project/libs/utils.ign"));
      },
      _ => panic!("Expected Project path"),
    }
  }

  #[test]
  fn alias_exact_match_preferred_over_prefix() {
    // "@token" as exact alias should take priority over "@" prefix match
    let aliases = HashMap::from([
      ("@".to_string(), PathBuf::from("/project/src")),
      ("@token".to_string(), PathBuf::from("/project/tokens")),
    ]);
    let result = ModulePath::from_import_path("@token::lexer", Path::new("/project/main.ign"), None, &aliases);
    assert!(result.is_ok());
    match result.unwrap() {
      ModulePath::Project(path) => {
        // Exact match "@token" wins, base = /project/tokens, rest = lexer
        assert_eq!(path, PathBuf::from("/project/tokens/lexer.ign"));
      },
      _ => panic!("Expected Project path"),
    }
  }

  #[test]
  fn relative_import_ignores_aliases() {
    let aliases = HashMap::from([("mylib".to_string(), PathBuf::from("/libs/mylib"))]);
    let result = ModulePath::from_import_path("./mylib", Path::new("/project/src/main.ign"), None, &aliases);
    assert!(result.is_ok());
    match result.unwrap() {
      // "./mylib" is a relative path, NOT an alias lookup
      ModulePath::Project(path) => {
        assert!(path.ends_with("mylib.ign"));
        assert!(path.to_string_lossy().contains("src"));
      },
      _ => panic!("Expected Project path from relative resolution"),
    }
  }
}
