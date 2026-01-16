use std::collections::HashMap;
use std::path::{Path, PathBuf};

use ignis_config::IgnisSTDManifest;
use ignis_type::module::{ImportInfo, Module, ModuleId, ModulePath, ModulePathError, ModuleState, ModuleStore};
use ignis_type::span::Span;
use ignis_type::symbol::SymbolId;

/// Error during module resolution
#[derive(Debug, Clone)]
pub enum ModuleError {
  /// File not found at expected path
  FileNotFound { path: PathBuf, import_span: Span },
  /// Standard library module not found
  StdModuleNotFound { module: String, import_span: Span },
  /// No project root (ignis.toml) found for absolute imports
  NoProjectRoot { import_path: String, import_span: Span },
  /// IO error reading file
  IoError { path: PathBuf, message: String },
  /// Circular dependency detected
  CircularDependency { cycle: Vec<PathBuf> },
  /// Path resolution error
  PathError { error: ModulePathError, import_span: Span },
}

/// Graph of all modules in a compilation unit
#[derive(Debug)]
pub struct ModuleGraph {
  pub modules: ModuleStore,
  /// Map from canonical path to module ID (for deduplication)
  pub by_path: HashMap<ModulePath, ModuleId>,
  /// Root module (entry point)
  pub root: Option<ModuleId>,
  /// Project root directory (where ignis.toml is)
  pub project_root: Option<PathBuf>,
  /// Standard library path
  pub std_path: PathBuf,
  /// Standard library manifest (for path resolution and linking info)
  pub manifest: Option<IgnisSTDManifest>,
}

impl ModuleGraph {
  pub fn new(
    project_root: Option<PathBuf>,
    std_path: PathBuf,
  ) -> Self {
    Self {
      modules: ModuleStore::new(),
      by_path: HashMap::new(),
      root: None,
      project_root,
      std_path,
      manifest: None,
    }
  }

  pub fn with_manifest(
    project_root: Option<PathBuf>,
    std_path: PathBuf,
    manifest: IgnisSTDManifest,
  ) -> Self {
    Self {
      modules: ModuleStore::new(),
      by_path: HashMap::new(),
      root: None,
      project_root,
      std_path,
      manifest: Some(manifest),
    }
  }

  /// Register a module in the graph
  pub fn register(
    &mut self,
    module: Module,
  ) -> ModuleId {
    let path = module.path.clone();

    // Check if already registered
    if let Some(id) = self.by_path.get(&path) {
      return *id;
    }

    let id = self.modules.alloc(module);
    self.by_path.insert(path, id);
    id
  }

  /// Get module by path if it exists
  pub fn get_by_path(
    &self,
    path: &ModulePath,
  ) -> Option<ModuleId> {
    self.by_path.get(path).copied()
  }

  /// Resolve an import path to a ModulePath
  pub fn resolve_import_path(
    &self,
    import_from: &str,
    current_file: &Path,
  ) -> Result<ModulePath, ModulePathError> {
    ModulePath::from_import_path(import_from, current_file, self.project_root.as_deref())
  }

  /// Resolve ModulePath to filesystem path, using manifest if available
  pub fn to_fs_path(
    &self,
    path: &ModulePath,
  ) -> PathBuf {
    // For std modules, try to get path from manifest first
    let manifest_path = path.std_module_name().and_then(|name| {
      self
        .manifest
        .as_ref()
        .and_then(|m| m.get_module_path(name).map(|s| s.as_str()))
    });

    path.to_fs_path_with_manifest_path(&self.std_path, manifest_path)
  }

  /// Get linking info for a std module (header and object file)
  pub fn get_linking_info(
    &self,
    module_name: &str,
  ) -> Option<&ignis_config::StdLinkingInfo> {
    self.manifest.as_ref().and_then(|m| m.get_linking_info(module_name))
  }

  /// Check if a file exists for the given module path
  pub fn file_exists(
    &self,
    path: &ModulePath,
  ) -> bool {
    self.to_fs_path(path).exists()
  }

  /// Add an import relationship to a module
  pub fn add_import(
    &mut self,
    module_id: ModuleId,
    items: Vec<SymbolId>,
    source_module: ModuleId,
    span: Span,
  ) {
    let module = self.modules.get_mut(&module_id);
    module.imports.push(ImportInfo::new(items, source_module, span));
  }

  /// Detect circular dependencies using DFS with 3-color marking
  pub fn detect_cycles(&mut self) -> Result<(), ModuleError> {
    // Reset all states
    let ids: Vec<ModuleId> = self.by_path.values().copied().collect();
    for id in &ids {
      self.modules.get_mut(id).state = ModuleState::Unprocessed;
    }

    // DFS from root
    if let Some(root) = self.root {
      let mut path_stack = Vec::new();
      self.dfs_detect_cycle(root, &mut path_stack)?;
    }

    Ok(())
  }

  fn dfs_detect_cycle(
    &mut self,
    module_id: ModuleId,
    path_stack: &mut Vec<PathBuf>,
  ) -> Result<(), ModuleError> {
    let module = self.modules.get(&module_id);
    let state = module.state;
    let module_path = self.to_fs_path(&module.path);

    match state {
      ModuleState::Processed => Ok(()),
      ModuleState::InProgress => {
        // Cycle detected! Build the cycle path
        path_stack.push(module_path);
        Err(ModuleError::CircularDependency {
          cycle: path_stack.clone(),
        })
      },
      ModuleState::Unprocessed => {
        // Mark as in-progress
        self.modules.get_mut(&module_id).state = ModuleState::InProgress;
        path_stack.push(module_path);

        // Get imports (need to clone to avoid borrow issues)
        let imports: Vec<ModuleId> = self
          .modules
          .get(&module_id)
          .imports
          .iter()
          .map(|i| i.source_module())
          .collect();

        // Visit all dependencies
        for dep_id in imports {
          self.dfs_detect_cycle(dep_id, path_stack)?;
        }

        // Mark as processed
        path_stack.pop();
        self.modules.get_mut(&module_id).state = ModuleState::Processed;
        Ok(())
      },
    }
  }

  /// Return modules in topological order (dependencies first)
  pub fn topological_sort(&self) -> Vec<ModuleId> {
    let mut result = Vec::new();
    let mut visited = std::collections::HashSet::new();

    if let Some(root) = self.root {
      self.topo_visit(root, &mut visited, &mut result);
    }

    result
  }

  /// Return all modules in topological order, regardless of reachability from root.
  pub fn all_modules_topological(&self) -> Vec<ModuleId> {
    let mut result = Vec::new();
    let mut visited = std::collections::HashSet::new();

    for (module_id, _) in self.modules.iter() {
      if !visited.contains(&module_id) {
        self.topo_visit(module_id, &mut visited, &mut result);
      }
    }

    result
  }

  fn topo_visit(
    &self,
    module_id: ModuleId,
    visited: &mut std::collections::HashSet<ModuleId>,
    result: &mut Vec<ModuleId>,
  ) {
    if visited.contains(&module_id) {
      return;
    }
    visited.insert(module_id);

    // Visit dependencies first
    let module = self.modules.get(&module_id);
    for import in &module.imports {
      self.topo_visit(import.source_module(), visited, result);
    }

    // Then add self
    result.push(module_id);
  }

  /// Returns transitive dependencies of a module (not including itself).
  pub fn transitive_deps(
    &self,
    module_id: ModuleId,
  ) -> Vec<ModuleId> {
    let mut result = Vec::new();
    let mut visited = std::collections::HashSet::new();
    self.collect_transitive_deps(module_id, &mut visited, &mut result);
    result
  }

  fn collect_transitive_deps(
    &self,
    module_id: ModuleId,
    visited: &mut std::collections::HashSet<ModuleId>,
    result: &mut Vec<ModuleId>,
  ) {
    let module = self.modules.get(&module_id);
    for import in &module.imports {
      let dep_id = import.source_module();
      if visited.insert(dep_id) {
        self.collect_transitive_deps(dep_id, visited, result);
        result.push(dep_id);
      }
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use ignis_type::file::{FileId, SourceMap};
  use std::path::PathBuf;

  fn dummy_file_id() -> FileId {
    let mut sm = SourceMap::new();
    sm.add_file("test.ign", "".to_string())
  }

  #[test]
  fn test_resolve_std_path() {
    let graph = ModuleGraph::new(None, PathBuf::from("/usr/lib/ignis/std"));
    let result = graph.resolve_import_path("std::io", Path::new("/project/main.ign"));

    assert!(result.is_ok());
    match result.unwrap() {
      ModulePath::Std(name) => assert_eq!(name, "io"),
      _ => panic!("expected Std path"),
    }
  }

  #[test]
  fn test_resolve_relative_path() {
    let graph = ModuleGraph::new(Some(PathBuf::from("/project")), PathBuf::from("/usr/lib/ignis/std"));
    let result = graph.resolve_import_path("./utils", Path::new("/project/src/main.ign"));

    assert!(result.is_ok());
    match result.unwrap() {
      ModulePath::Project(path) => {
        assert!(path.ends_with("utils.ign"));
      },
      _ => panic!("expected Project path"),
    }
  }

  #[test]
  fn test_resolve_absolute_path() {
    let graph = ModuleGraph::new(Some(PathBuf::from("/project")), PathBuf::from("/usr/lib/ignis/std"));
    let result = graph.resolve_import_path("utils/math", Path::new("/project/src/main.ign"));

    assert!(result.is_ok());
    match result.unwrap() {
      ModulePath::Project(path) => {
        assert!(path.to_string_lossy().contains("utils"));
        assert!(path.to_string_lossy().contains("math"));
      },
      _ => panic!("expected Project path"),
    }
  }

  #[test]
  fn test_no_project_root_error() {
    let graph = ModuleGraph::new(None, PathBuf::from("/usr/lib/ignis/std"));
    let result = graph.resolve_import_path("utils/math", Path::new("/some/file.ign"));

    assert!(result.is_err());
  }

  #[test]
  fn test_topological_sort_simple() {
    let mut graph = ModuleGraph::new(None, PathBuf::from("/std"));

    // Create modules: main -> lib
    let lib_module = Module::new(dummy_file_id(), ModulePath::Project(PathBuf::from("/lib.ign")));
    let lib_id = graph.register(lib_module);

    let main_module = Module::new(dummy_file_id(), ModulePath::Project(PathBuf::from("/main.ign")));
    let main_id = graph.register(main_module);

    // Add import from main to lib
    graph.add_import(main_id, vec![], lib_id, Span::default());

    graph.root = Some(main_id);

    let order = graph.topological_sort();

    // lib should come before main
    let lib_pos = order.iter().position(|&id| id == lib_id).unwrap();
    let main_pos = order.iter().position(|&id| id == main_id).unwrap();
    assert!(lib_pos < main_pos);
  }

  #[test]
  fn test_topological_sort_chain() {
    let mut graph = ModuleGraph::new(None, PathBuf::from("/std"));

    // Create chain: main -> b -> c
    let c_module = Module::new(dummy_file_id(), ModulePath::Project(PathBuf::from("/c.ign")));
    let c_id = graph.register(c_module);

    let b_module = Module::new(dummy_file_id(), ModulePath::Project(PathBuf::from("/b.ign")));
    let b_id = graph.register(b_module);

    let main_module = Module::new(dummy_file_id(), ModulePath::Project(PathBuf::from("/main.ign")));
    let main_id = graph.register(main_module);

    // main -> b -> c
    graph.add_import(main_id, vec![], b_id, Span::default());
    graph.add_import(b_id, vec![], c_id, Span::default());

    graph.root = Some(main_id);

    let order = graph.topological_sort();

    // c should come before b, b before main
    let c_pos = order.iter().position(|&id| id == c_id).unwrap();
    let b_pos = order.iter().position(|&id| id == b_id).unwrap();
    let main_pos = order.iter().position(|&id| id == main_id).unwrap();

    assert!(c_pos < b_pos);
    assert!(b_pos < main_pos);
  }

  #[test]
  fn test_topological_sort_diamond() {
    let mut graph = ModuleGraph::new(None, PathBuf::from("/std"));

    // Diamond: main -> {a, b} -> c
    let c_module = Module::new(dummy_file_id(), ModulePath::Project(PathBuf::from("/c.ign")));
    let c_id = graph.register(c_module);

    let a_module = Module::new(dummy_file_id(), ModulePath::Project(PathBuf::from("/a.ign")));
    let a_id = graph.register(a_module);

    let b_module = Module::new(dummy_file_id(), ModulePath::Project(PathBuf::from("/b.ign")));
    let b_id = graph.register(b_module);

    let main_module = Module::new(dummy_file_id(), ModulePath::Project(PathBuf::from("/main.ign")));
    let main_id = graph.register(main_module);

    // main -> a, main -> b, a -> c, b -> c
    graph.add_import(main_id, vec![], a_id, Span::default());
    graph.add_import(main_id, vec![], b_id, Span::default());
    graph.add_import(a_id, vec![], c_id, Span::default());
    graph.add_import(b_id, vec![], c_id, Span::default());

    graph.root = Some(main_id);

    let order = graph.topological_sort();

    // c should come before both a and b, both before main
    let c_pos = order.iter().position(|&id| id == c_id).unwrap();
    let a_pos = order.iter().position(|&id| id == a_id).unwrap();
    let b_pos = order.iter().position(|&id| id == b_id).unwrap();
    let main_pos = order.iter().position(|&id| id == main_id).unwrap();

    assert!(c_pos < a_pos);
    assert!(c_pos < b_pos);
    assert!(a_pos < main_pos);
    assert!(b_pos < main_pos);
  }

  #[test]
  fn test_detect_cycle_simple() {
    let mut graph = ModuleGraph::new(None, PathBuf::from("/std"));

    // Cycle: a -> b -> a
    let a_module = Module::new(dummy_file_id(), ModulePath::Project(PathBuf::from("/a.ign")));
    let a_id = graph.register(a_module);

    let b_module = Module::new(dummy_file_id(), ModulePath::Project(PathBuf::from("/b.ign")));
    let b_id = graph.register(b_module);

    // a -> b, b -> a (cycle!)
    graph.add_import(a_id, vec![], b_id, Span::default());
    graph.add_import(b_id, vec![], a_id, Span::default());

    graph.root = Some(a_id);

    let result = graph.detect_cycles();
    assert!(result.is_err());

    match result.unwrap_err() {
      ModuleError::CircularDependency { cycle } => {
        assert!(cycle.len() >= 2);
      },
      _ => panic!("Expected CircularDependency error"),
    }
  }

  #[test]
  fn test_detect_cycle_three_nodes() {
    let mut graph = ModuleGraph::new(None, PathBuf::from("/std"));

    // Cycle: a -> b -> c -> a
    let a_module = Module::new(dummy_file_id(), ModulePath::Project(PathBuf::from("/a.ign")));
    let a_id = graph.register(a_module);

    let b_module = Module::new(dummy_file_id(), ModulePath::Project(PathBuf::from("/b.ign")));
    let b_id = graph.register(b_module);

    let c_module = Module::new(dummy_file_id(), ModulePath::Project(PathBuf::from("/c.ign")));
    let c_id = graph.register(c_module);

    // a -> b -> c -> a (cycle!)
    graph.add_import(a_id, vec![], b_id, Span::default());
    graph.add_import(b_id, vec![], c_id, Span::default());
    graph.add_import(c_id, vec![], a_id, Span::default());

    graph.root = Some(a_id);

    let result = graph.detect_cycles();
    assert!(result.is_err());

    match result.unwrap_err() {
      ModuleError::CircularDependency { cycle } => {
        assert!(cycle.len() >= 3);
      },
      _ => panic!("Expected CircularDependency error"),
    }
  }

  #[test]
  fn test_no_cycle_with_shared_dependency() {
    let mut graph = ModuleGraph::new(None, PathBuf::from("/std"));

    // Not a cycle: main -> {a, b}, both a and b -> c
    let c_module = Module::new(dummy_file_id(), ModulePath::Project(PathBuf::from("/c.ign")));
    let c_id = graph.register(c_module);

    let a_module = Module::new(dummy_file_id(), ModulePath::Project(PathBuf::from("/a.ign")));
    let a_id = graph.register(a_module);

    let b_module = Module::new(dummy_file_id(), ModulePath::Project(PathBuf::from("/b.ign")));
    let b_id = graph.register(b_module);

    let main_module = Module::new(dummy_file_id(), ModulePath::Project(PathBuf::from("/main.ign")));
    let main_id = graph.register(main_module);

    graph.add_import(main_id, vec![], a_id, Span::default());
    graph.add_import(main_id, vec![], b_id, Span::default());
    graph.add_import(a_id, vec![], c_id, Span::default());
    graph.add_import(b_id, vec![], c_id, Span::default());

    graph.root = Some(main_id);

    let result = graph.detect_cycles();
    assert!(result.is_ok());
  }

  #[test]
  fn test_module_deduplication() {
    let mut graph = ModuleGraph::new(None, PathBuf::from("/std"));

    let path = ModulePath::Project(PathBuf::from("/lib.ign"));

    let module1 = Module::new(dummy_file_id(), path.clone());
    let id1 = graph.register(module1);

    // Try to register same path again
    let module2 = Module::new(dummy_file_id(), path.clone());
    let id2 = graph.register(module2);

    // Should return the same ID
    assert_eq!(id1, id2);
  }

  #[test]
  fn test_with_manifest_constructor() {
    use ignis_config::{IgnisSTDManifest, StdLinkingInfo};
    use std::collections::HashMap;

    let mut modules = HashMap::new();
    modules.insert("io".to_string(), "io/mod.ign".to_string());

    let mut linking = HashMap::new();
    linking.insert(
      "io".to_string(),
      StdLinkingInfo {
        header: Some("runtime/io.h".to_string()),
        header_quoted: Some(true),
        object: Some("runtime/libignis_io.o".to_string()),
        lib: None,
      },
    );

    let manifest = IgnisSTDManifest {
      toolchain: Default::default(),
      modules,
      linking,
      auto_load: None,
    };

    let graph = ModuleGraph::with_manifest(None, PathBuf::from("/std"), manifest);

    assert!(graph.manifest.is_some());
    assert_eq!(
      graph.manifest.as_ref().unwrap().get_module_path("io"),
      Some(&"io/mod.ign".to_string())
    );
  }

  #[test]
  fn test_to_fs_path_with_manifest() {
    use ignis_config::IgnisSTDManifest;
    use std::collections::HashMap;

    let mut modules = HashMap::new();
    modules.insert("io".to_string(), "io/mod.ign".to_string());
    modules.insert("math".to_string(), "math/mod.ign".to_string());

    let manifest = IgnisSTDManifest {
      toolchain: Default::default(),
      modules,
      linking: HashMap::new(),
      auto_load: None,
    };

    let graph = ModuleGraph::with_manifest(None, PathBuf::from("/usr/lib/ignis/std"), manifest);

    // Test std path resolution with manifest
    let io_path = ModulePath::Std("io".to_string());
    let fs_path = graph.to_fs_path(&io_path);
    assert_eq!(fs_path, PathBuf::from("/usr/lib/ignis/std/io/mod.ign"));

    // Test project path (should be unchanged)
    let project_path = ModulePath::Project(PathBuf::from("/project/utils.ign"));
    let fs_path = graph.to_fs_path(&project_path);
    assert_eq!(fs_path, PathBuf::from("/project/utils.ign"));
  }

  #[test]
  fn test_to_fs_path_without_manifest_fallback() {
    let graph = ModuleGraph::new(None, PathBuf::from("/usr/lib/ignis/std"));

    // Without manifest, should use convention (falls back to .ign)
    let io_path = ModulePath::Std("io".to_string());
    let fs_path = graph.to_fs_path(&io_path);
    assert_eq!(fs_path, PathBuf::from("/usr/lib/ignis/std/io.ign"));
  }

  #[test]
  fn test_get_linking_info_with_manifest() {
    use ignis_config::{IgnisSTDManifest, StdLinkingInfo};
    use std::collections::HashMap;

    let mut linking = HashMap::new();
    linking.insert(
      "io".to_string(),
      StdLinkingInfo {
        header: Some("runtime/io.h".to_string()),
        header_quoted: Some(true),
        object: Some("runtime/libignis_io.o".to_string()),
        lib: None,
      },
    );
    linking.insert(
      "math".to_string(),
      StdLinkingInfo {
        header: Some("math.h".to_string()),
        header_quoted: Some(false),
        object: None,
        lib: Some("m".to_string()),
      },
    );

    let manifest = IgnisSTDManifest {
      toolchain: Default::default(),
      modules: HashMap::new(),
      linking,
      auto_load: None,
    };

    let graph = ModuleGraph::with_manifest(None, PathBuf::from("/std"), manifest);

    // Test io linking info
    let io_linking = graph.get_linking_info("io");
    assert!(io_linking.is_some());
    let io_linking = io_linking.unwrap();
    assert_eq!(io_linking.header, Some("runtime/io.h".to_string()));
    assert_eq!(io_linking.object, Some("runtime/libignis_io.o".to_string()));

    // Test math linking info (no object)
    let math_linking = graph.get_linking_info("math");
    assert!(math_linking.is_some());
    let math_linking = math_linking.unwrap();
    assert_eq!(math_linking.header, Some("math.h".to_string()));
    assert_eq!(math_linking.object, None);

    // Test non-existent module
    let unknown = graph.get_linking_info("unknown");
    assert!(unknown.is_none());
  }

  #[test]
  fn test_get_linking_info_without_manifest() {
    let graph = ModuleGraph::new(None, PathBuf::from("/std"));

    // Without manifest, should return None
    let result = graph.get_linking_info("io");
    assert!(result.is_none());
  }
}
