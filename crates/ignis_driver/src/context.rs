use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::{cell::RefCell, rc::Rc};

use colored::*;
use ignis_analyzer::imports::ExportTable;
use ignis_analyzer::modules::{ModuleError, ModuleGraph};
use ignis_ast::statements::export_statement::ASTExport;
use ignis_ast::statements::import_statement::ImportItemKind;
use ignis_ast::{ASTNode, NodeId, statements::ASTStatement};
use ignis_config::{DebugTrace, IgnisConfig, IgnisSTDManifest};
use ignis_diagnostics::diagnostic_report::Diagnostic;

use ignis_log::{log_dbg, log_trc, phase_log, trace_dbg};
use ignis_parser::{IgnisLexer, IgnisParser};
use ignis_hir::HIR;
use ignis_type::compilation_context::CompilationContext as TypeCompilationContext;
use ignis_type::definition::{DefinitionKind, DefinitionStore};
use ignis_type::file::{FileId, SourceMap};
use ignis_type::module::{Module, ModuleId, ModulePath};
use ignis_type::span::Span;
use ignis_type::symbol::{SymbolId, SymbolTable};
use ignis_type::types::TypeStore;
use ignis_type::Store;

/// Parsed module data
pub(crate) struct ParsedModule {
  pub file_id: FileId,
  pub nodes: Store<ASTNode>,
  pub roots: Vec<NodeId>,
  pub import_paths: Vec<(Vec<SymbolId>, String, ignis_type::span::Span)>,
}

pub(crate) struct ModuleSemanticData {
  pub node_defs: HashMap<NodeId, ignis_type::definition::DefinitionId>,
  pub node_types: HashMap<NodeId, ignis_type::types::TypeId>,
  pub node_spans: HashMap<NodeId, Span>,
  pub resolved_calls: HashMap<NodeId, ignis_type::definition::DefinitionId>,
  pub import_item_defs: HashMap<Span, ignis_type::definition::DefinitionId>,
  pub import_module_files: HashMap<Span, FileId>,
}

/// Discovery and compilation context
pub struct CompilationContext {
  pub source_map: SourceMap,
  pub symbol_table: Rc<RefCell<SymbolTable>>,
  pub module_graph: ModuleGraph,
  pub(crate) parsed_modules: HashMap<ModuleId, ParsedModule>,
  module_for_path: HashMap<String, ModuleId>,

  /// Prelude module IDs discovered via auto_load. Stored separately from
  /// the module graph to avoid polluting the import DAG with back-edges
  /// that would create cycles. The analyzer receives these as
  /// `implicit_imports` for each module being analyzed.
  prelude_module_ids: Vec<ModuleId>,

  /// Diagnostics collected during module discovery (lex/parse errors).
  /// Used by LSP mode to collect all errors without failing early.
  pub discovery_diagnostics: Vec<Diagnostic>,

  /// Pre-loaded file contents (path -> text).
  /// Used by LSP to provide in-memory file content instead of reading from disk.
  preloaded_files: HashMap<PathBuf, String>,
}

impl CompilationContext {
  fn load_std_manifest_if_needed(config: &IgnisConfig) -> Option<IgnisSTDManifest> {
    if config.std_path.is_empty() || !config.manifest.modules.is_empty() {
      return None;
    }

    let manifest_path = Path::new(&config.std_path).join("manifest.toml");
    let text = std::fs::read_to_string(&manifest_path).ok()?;
    toml::from_str(&text).ok()
  }

  fn normalize_discovered_module_path(
    &self,
    module_path: ModulePath,
  ) -> ModulePath {
    match module_path {
      ModulePath::Project(path) => {
        let path_string = path.to_string_lossy();

        if let Some(std_name) = self.try_resolve_std_module_name(&path_string) {
          ModulePath::Std(std_name)
        } else {
          ModulePath::Project(path)
        }
      },
      other => other,
    }
  }

  pub fn new(config: &IgnisConfig) -> Self {
    let project_root = config
      .project_config
      .as_ref()
      .map(|p| PathBuf::from(&p.build.source_dir));

    let std_path = PathBuf::from(&config.std_path);

    let mut aliases = HashMap::new();
    if !config.std_path.is_empty() {
      aliases.insert("std".to_string(), std_path.clone());
    }
    aliases.extend(config.aliases.iter().map(|(k, v)| (k.clone(), v.clone())));

    let manifest = Self::load_std_manifest_if_needed(config).unwrap_or_else(|| config.manifest.clone());

    let module_graph = if manifest.modules.is_empty() {
      ModuleGraph::new(project_root, std_path, aliases)
    } else {
      ModuleGraph::with_manifest(project_root, std_path, manifest, aliases)
    };

    Self {
      source_map: SourceMap::new(),
      symbol_table: Rc::new(RefCell::new(SymbolTable::new())),
      module_graph,
      parsed_modules: HashMap::new(),
      module_for_path: HashMap::new(),
      prelude_module_ids: Vec::new(),
      discovery_diagnostics: Vec::new(),
      preloaded_files: HashMap::new(),
    }
  }

  /// If `fs_path` points to a file inside `std_path`, return its canonical std
  /// module name. Used to avoid registering the same file as both
  /// `ModulePath::Project` and `ModulePath::Std`.
  pub(crate) fn try_resolve_std_module_name(
    &self,
    fs_path: &str,
  ) -> Option<String> {
    let std_path = self.module_graph.std_path();
    if std_path.as_os_str().is_empty() {
      return None;
    }

    let std_canon = std_path.canonicalize().ok()?;
    let file_canon = Path::new(fs_path).canonicalize().ok()?;

    if !file_canon.starts_with(&std_canon) {
      return None;
    }

    if file_canon.file_name().is_some_and(|file_name| file_name == "tests.ign") {
      return None;
    }

    if let Some(manifest) = self.module_graph.manifest.as_ref() {
      for (module_name, rel_path) in &manifest.modules {
        let module_canon = std_canon.join(rel_path);
        if let Ok(module_canon) = module_canon.canonicalize()
          && module_canon == file_canon
        {
          return Some(module_name.clone());
        }
      }
    }

    Self::std_module_name_from_path(&std_canon, &file_canon)
  }

  fn std_module_name_from_path(
    std_root: &Path,
    file_path: &Path,
  ) -> Option<String> {
    if file_path.extension().and_then(|ext| ext.to_str()) != Some("ign") {
      return None;
    }

    let relative = file_path.strip_prefix(std_root).ok()?;
    let mut parts: Vec<String> = relative
      .parent()
      .map(|parent| {
        parent
          .components()
          .map(|component| component.as_os_str().to_string_lossy().into_owned())
          .collect()
      })
      .unwrap_or_default();

    let file_stem = relative.file_stem()?.to_string_lossy();
    if file_stem != "mod" {
      parts.push(file_stem.into_owned());
    }

    if parts.is_empty() { None } else { Some(parts.join("::")) }
  }

  fn module_path_alias_keys(
    import_path: &str,
    module_path: &ModulePath,
  ) -> Vec<String> {
    let mut keys = Vec::new();

    if let ModulePath::Std(name) = module_path {
      keys.push(format!("std::{}", name));
    }

    if !import_path.is_empty() && !keys.iter().any(|key| key == import_path) {
      keys.push(import_path.to_string());
    }

    keys
  }

  fn insert_module_path_aliases(
    &mut self,
    import_path: &str,
    module_path: &ModulePath,
    module_id: ModuleId,
  ) -> Vec<String> {
    let mut inserted_keys = Vec::new();

    for key in Self::module_path_alias_keys(import_path, module_path) {
      if let Some(existing_id) = self.module_for_path.get(&key)
        && *existing_id != module_id
      {
        continue;
      }

      self.module_for_path.insert(key.clone(), module_id);
      inserted_keys.push(key);
    }

    inserted_keys
  }

  fn remove_module_path_aliases(
    &mut self,
    keys: Vec<String>,
    module_id: ModuleId,
  ) {
    for key in keys {
      if self.module_for_path.get(&key) == Some(&module_id) {
        self.module_for_path.remove(&key);
      }
    }
  }

  /// Pre-load a file's content for LSP mode.
  ///
  /// When discovering modules, this content will be used instead of reading from disk.
  pub fn preload_file(
    &mut self,
    path: PathBuf,
    text: String,
  ) {
    self.preloaded_files.insert(path, text);
  }

  /// Discover and parse all modules starting from entry point
  pub fn discover_modules(
    &mut self,
    entry_path: &str,
    config: &IgnisConfig,
  ) -> Result<ModuleId, ()> {
    log_dbg!(config, "starting module discovery from {}", entry_path);

    self.discover_recursive(entry_path, None, config)
  }

  fn prelude_std_modules(config: &IgnisConfig) -> Vec<&str> {
    config.manifest.get_auto_load_modules()
  }

  /// Discover prelude std modules and add safe DAG edges for ordering.
  ///
  /// Prelude IDs go into `prelude_module_ids` for implicit-import injection.
  /// Edges `M → P` are added only where they don't create cycles (checked
  /// against a snapshot of the explicit-only DAG).
  pub fn discover_prelude_modules(
    &mut self,
    _root_id: ModuleId,
    config: &IgnisConfig,
  ) {
    self.discover_and_register_preludes(config, false);
    self.add_safe_prelude_edges();
  }

  /// Variant for the std-library build (no root module).
  pub fn discover_prelude_modules_for_all(
    &mut self,
    config: &IgnisConfig,
  ) {
    self.discover_and_register_preludes(config, false);
    self.add_safe_prelude_edges();
  }

  /// LSP variant.
  pub fn discover_prelude_modules_lsp(
    &mut self,
    _root_id: ModuleId,
    config: &IgnisConfig,
  ) {
    self.discover_and_register_preludes(config, true);
    self.add_safe_prelude_edges();
  }

  /// LSP variant (all modules).
  pub fn discover_prelude_modules_for_all_lsp(
    &mut self,
    config: &IgnisConfig,
  ) {
    self.discover_and_register_preludes(config, true);
    self.add_safe_prelude_edges();
  }

  fn discover_and_register_preludes(
    &mut self,
    config: &IgnisConfig,
    lsp: bool,
  ) {
    for module_name in Self::prelude_std_modules(config) {
      let result = if lsp {
        self.discover_std_module_lsp(module_name, config)
      } else {
        self.discover_std_module(module_name, config)
      };

      if let Ok(prelude_id) = result
        && !self.prelude_module_ids.contains(&prelude_id)
      {
        self.prelude_module_ids.push(prelude_id);
      }
    }
  }

  /// Add prelude import edges to the module DAG without creating cycles.
  ///
  /// Uses a two-pass approach to prevent indirect cycles through prelude
  /// chains. For example, if `vector → memory` is an explicit edge, a
  /// naive single-pass could add both `memory → string` and
  /// `string → vector` (each safe individually), creating the cycle
  /// `vector → memory → string → vector`.
  ///
  /// Pass 1 adds prelude-to-prelude edges using the pre-prelude graph.
  /// Pass 2 recomputes reachability (now including prelude chains) and
  /// uses it to safely add non-prelude-to-prelude edges.
  fn add_safe_prelude_edges(&mut self) {
    use std::collections::HashSet;
    use ignis_type::BytePosition;

    let prelude_set: HashSet<ModuleId> = self.prelude_module_ids.iter().copied().collect();
    let all_module_ids: Vec<ModuleId> = self.module_graph.modules.iter().map(|(mid, _)| mid).collect();

    // Pass 1: prelude-to-prelude edges.
    for &module_id in &self.prelude_module_ids {
      let file_id = self.module_graph.modules.get(&module_id).file_id;
      let dummy_span = Span::empty_at(file_id, BytePosition::default());

      for &prelude_id in &self.prelude_module_ids {
        if module_id == prelude_id {
          continue;
        }

        let reachable_from_prelude: HashSet<ModuleId> =
          self.module_graph.transitive_deps(prelude_id).into_iter().collect();
        if reachable_from_prelude.contains(&module_id) {
          continue;
        }

        let already = self
          .module_graph
          .modules
          .get(&module_id)
          .imports
          .iter()
          .any(|imp| imp.items.is_empty() && imp.source_module() == prelude_id);

        if already {
          continue;
        }

        self
          .module_graph
          .add_import(module_id, Vec::new(), prelude_id, dummy_span.clone());
      }
    }

    // Pass 2: non-prelude-to-prelude edges.
    // Reachability is checked against the current graph for each candidate,
    // including prelude-to-prelude edges accepted in pass 1.

    for &module_id in &all_module_ids {
      if prelude_set.contains(&module_id) {
        continue;
      }

      let file_id = self.module_graph.modules.get(&module_id).file_id;
      let dummy_span = Span::empty_at(file_id, BytePosition::default());

      for &prelude_id in &self.prelude_module_ids {
        let reachable_from_prelude: HashSet<ModuleId> =
          self.module_graph.transitive_deps(prelude_id).into_iter().collect();
        if reachable_from_prelude.contains(&module_id) {
          continue;
        }

        let already = self
          .module_graph
          .modules
          .get(&module_id)
          .imports
          .iter()
          .any(|imp| imp.items.is_empty() && imp.source_module() == prelude_id);

        if already {
          continue;
        }

        self
          .module_graph
          .add_import(module_id, Vec::new(), prelude_id, dummy_span.clone());
      }
    }
  }

  /// Discover every std module in the manifest (sorted by name).
  ///
  /// Called before entry-point discovery when the entry is itself a std
  /// file, so the slotmap insertion order matches `check-std` and the
  /// topological sort produces a valid analysis order despite
  /// prelude-induced cycles.
  pub fn discover_all_std_modules(
    &mut self,
    config: &IgnisConfig,
  ) {
    let mut module_names: Vec<String> = config.manifest.modules.keys().cloned().collect();
    module_names.sort();

    for module_name in &module_names {
      let _ = self.discover_std_module(module_name, config);
    }
  }

  /// LSP variant of [`Self::discover_all_std_modules`].
  pub fn discover_all_std_modules_lsp(
    &mut self,
    config: &IgnisConfig,
  ) {
    let mut module_names: Vec<String> = config.manifest.modules.keys().cloned().collect();
    module_names.sort();

    for module_name in &module_names {
      let _ = self.discover_std_module_lsp(module_name, config);
    }
  }

  /// Discover a std module by name (e.g., "io", "string").
  /// Creates `ModulePath::Std` entries for proper std/project distinction.
  pub fn discover_std_module(
    &mut self,
    module_name: &str,
    config: &IgnisConfig,
  ) -> Result<ModuleId, ()> {
    let module_path = ModulePath::Std(module_name.to_string());

    if let Some(id) = self.module_graph.get_by_path(&module_path) {
      self.insert_module_path_aliases(module_name, &module_path, id);
      return Ok(id);
    }

    log_dbg!(config, "discovering std module {:?}", module_path);

    let fs_path = self.module_graph.to_fs_path(&module_path);
    let parsed = self.parse_file(&fs_path, config)?;
    let file_id = parsed.file_id;
    let import_paths = parsed.import_paths.clone();

    let module = Module::new(file_id, module_path.clone());
    let module_id = self.module_graph.register(module);
    self.insert_module_path_aliases(module_name, &module_path, module_id);
    self.parsed_modules.insert(module_id, parsed);

    for (items, import_from, span) in import_paths {
      let dep_id = self.discover_recursive(&import_from, Some(&fs_path), config)?;
      self.module_graph.add_import(module_id, items, dep_id, span);
    }

    Ok(module_id)
  }

  fn discover_recursive(
    &mut self,
    path: &str,
    current_file: Option<&Path>,
    config: &IgnisConfig,
  ) -> Result<ModuleId, ()> {
    let module_path = if let Some(current_file) = current_file {
      match self.module_graph.resolve_import_path(path, current_file) {
        Ok(p) => p,
        Err(e) => {
          eprintln!("{} Failed to resolve import path '{}': {:?}", "Error:".red().bold(), path, e);
          return Err(());
        },
      }
    } else if let Some(std_name) = self.try_resolve_std_module_name(path) {
      ModulePath::Std(std_name)
    } else {
      ModulePath::Project(PathBuf::from(path))
    };

    let module_path = self.normalize_discovered_module_path(module_path);

    if let Some(id) = self.module_graph.get_by_path(&module_path) {
      self.insert_module_path_aliases(path, &module_path, id);
      return Ok(id);
    }

    log_dbg!(config, "discovering module {:?}", module_path);

    let placeholder_id = ModuleId::new(self.module_graph.modules.iter().count() as u32);
    let placeholder_keys = self.insert_module_path_aliases(path, &module_path, placeholder_id);

    let fs_path = self.module_graph.to_fs_path(&module_path);
    let parsed = match self.parse_file(&fs_path, config) {
      Ok(p) => p,
      Err(()) => {
        self.remove_module_path_aliases(placeholder_keys, placeholder_id);
        return Err(());
      },
    };
    let file_id = parsed.file_id;
    let import_paths = parsed.import_paths.clone();

    let module = Module::new(file_id, module_path.clone());
    let module_id = self.module_graph.register(module);
    self.remove_module_path_aliases(placeholder_keys, placeholder_id);
    self.insert_module_path_aliases(path, &module_path, module_id);
    self.parsed_modules.insert(module_id, parsed);

    for (items, import_from, span) in import_paths {
      let dep_id = self.discover_recursive(&import_from, Some(&fs_path), config)?;
      self.module_graph.add_import(module_id, items, dep_id, span);
    }

    Ok(module_id)
  }

  fn parse_file(
    &mut self,
    path: &Path,
    config: &IgnisConfig,
  ) -> Result<ParsedModule, ()> {
    let text = match std::fs::read_to_string(path) {
      Ok(content) => content,
      Err(e) => {
        eprintln!("{} Failed to read file '{}': {}", "Error:".red().bold(), path.display(), e);
        return Err(());
      },
    };

    phase_log!(config, "Scanning... {}", path.display());

    let file_id = self.source_map.add_file(path, text);
    let src = &self.source_map.get(&file_id).text.clone();

    let mut lexer = IgnisLexer::new(file_id, src);
    lexer.scan_tokens();

    trace_dbg!(
      config,
      DebugTrace::Lexer,
      "produced {} tokens for {}",
      lexer.tokens.len(),
      path.display()
    );

    if !lexer.diagnostics.is_empty() {
      for diag_msg in &lexer.diagnostics {
        ignis_diagnostics::render(&diag_msg.report(), &self.source_map);
      }
      return Err(());
    }

    phase_log!(config, "Parsing... {}", path.display());

    log_dbg!(config, "parsing {}", path.display());

    let compilation_ctx = build_type_compilation_context(config);
    let mut parser = IgnisParser::new_with_compilation_ctx(lexer.tokens, self.symbol_table.clone(), compilation_ctx);
    let parse_result = parser.parse();

    let (nodes, roots) = match parse_result {
      Ok(r) => r,
      Err(errs) => {
        for diag_msg in &errs {
          ignis_diagnostics::render(&diag_msg.report(), &self.source_map);
        }
        return Err(());
      },
    };

    trace_dbg!(
      config,
      DebugTrace::Parser,
      "parsed {} nodes for {}",
      nodes.len(),
      path.display()
    );

    let import_paths = self.extract_imports(&nodes, &roots);

    log_trc!(config, "{} imports found in {}", import_paths.len(), path.display());

    Ok(ParsedModule {
      file_id,
      nodes,
      roots,
      import_paths,
    })
  }

  fn extract_imports(
    &self,
    nodes: &Store<ASTNode>,
    roots: &[NodeId],
  ) -> Vec<(Vec<SymbolId>, String, ignis_type::span::Span)> {
    let mut imports = Vec::new();

    for root in roots {
      match nodes.get(root) {
        ASTNode::Statement(ASTStatement::Import(import)) => {
          let symbol_ids: Vec<SymbolId> = import
            .items
            .iter()
            .filter_map(|item| match &item.kind {
              ImportItemKind::Named(name) => Some(*name),
              ImportItemKind::Discard => None,
            })
            .collect();
          imports.push((symbol_ids, import.from.clone(), import.span.clone()));
        },
        ASTNode::Statement(ASTStatement::Export(ASTExport::ReExportFrom { items, from, span, .. })) => {
          let symbol_ids: Vec<SymbolId> = items
            .iter()
            .filter_map(|item| match &item.kind {
              ImportItemKind::Named(name) => Some(*name),
              ImportItemKind::Discard => None,
            })
            .collect();
          imports.push((symbol_ids, from.clone(), span.clone()));
        },
        _ => {},
      }
    }

    imports
  }

  /// Discover modules for LSP mode - collects all diagnostics without failing early.
  ///
  /// Unlike `discover_modules`, this method:
  /// - Continues even when files have lex/parse errors
  /// - Collects diagnostics in `self.discovery_diagnostics` instead of rendering
  /// - Returns `Ok(root_id)` if the entry file was at least partially parsed
  pub fn discover_modules_lsp(
    &mut self,
    entry_path: &str,
    config: &IgnisConfig,
  ) -> Result<ModuleId, ()> {
    log_dbg!(config, "starting LSP module discovery from {}", entry_path);

    self.discover_recursive_lsp(entry_path, None, config)
  }

  fn discover_recursive_lsp(
    &mut self,
    path: &str,
    current_file: Option<&Path>,
    config: &IgnisConfig,
  ) -> Result<ModuleId, ()> {
    let module_path = if let Some(current_file) = current_file {
      match self.module_graph.resolve_import_path(path, current_file) {
        Ok(p) => p,
        Err(e) => {
          log_dbg!(config, "failed to resolve import path '{}': {:?}", path, e);
          return Err(());
        },
      }
    } else if let Some(std_name) = self.try_resolve_std_module_name(path) {
      ModulePath::Std(std_name)
    } else {
      ModulePath::Project(PathBuf::from(path))
    };

    let module_path = self.normalize_discovered_module_path(module_path);

    if let Some(id) = self.module_graph.get_by_path(&module_path) {
      self.insert_module_path_aliases(path, &module_path, id);
      return Ok(id);
    }

    log_dbg!(config, "discovering module {:?} (LSP mode)", module_path);

    // Placeholder to prevent infinite recursion on cyclic imports
    let placeholder_id = ModuleId::new(self.module_graph.modules.iter().count() as u32);
    let placeholder_keys = self.insert_module_path_aliases(path, &module_path, placeholder_id);

    let fs_path = self.module_graph.to_fs_path(&module_path);

    let parsed = match self.parse_file_lsp(&fs_path, config) {
      Ok(p) => p,
      Err(()) => {
        self.remove_module_path_aliases(placeholder_keys, placeholder_id);
        return Err(());
      },
    };

    let file_id = parsed.file_id;
    let import_paths = parsed.import_paths.clone();

    let module = Module::new(file_id, module_path.clone());
    let module_id = self.module_graph.register(module);
    self.remove_module_path_aliases(placeholder_keys, placeholder_id);
    self.insert_module_path_aliases(path, &module_path, module_id);
    self.parsed_modules.insert(module_id, parsed);

    // Continue discovering imports even if this file had errors
    for (items, import_from, span) in import_paths {
      if let Ok(dep_id) = self.discover_recursive_lsp(&import_from, Some(&fs_path), config) {
        self.module_graph.add_import(module_id, items, dep_id, span);
      }
      // If import discovery fails, we continue with other imports
    }

    Ok(module_id)
  }

  /// Parse a file for LSP mode - collects diagnostics instead of rendering.
  ///
  /// Returns `Ok(ParsedModule)` even if there are lex/parse errors,
  /// as long as the file could be read. Errors are stored in `self.discovery_diagnostics`.
  fn parse_file_lsp(
    &mut self,
    path: &Path,
    config: &IgnisConfig,
  ) -> Result<ParsedModule, ()> {
    // Check if we have preloaded content for this file
    let text = if let Some(preloaded) = self.preloaded_files.remove(path) {
      preloaded
    } else {
      match std::fs::read_to_string(path) {
        Ok(content) => content,
        Err(e) => {
          log_dbg!(config, "failed to read file '{}': {}", path.display(), e);
          return Err(());
        },
      }
    };

    let file_id = self.source_map.add_file(path, text);
    let src = self.source_map.get(&file_id).text.clone();

    // Lexer phase
    let mut lexer = IgnisLexer::new(file_id, &src);
    lexer.scan_tokens();

    trace_dbg!(
      config,
      DebugTrace::Lexer,
      "produced {} tokens for {} (LSP)",
      lexer.tokens.len(),
      path.display()
    );

    // Collect lexer diagnostics
    for diag_msg in &lexer.diagnostics {
      self.discovery_diagnostics.push(diag_msg.report());
    }

    if !lexer.diagnostics.is_empty() {
      // Return a minimal ParsedModule so we can still show the errors
      return Ok(ParsedModule {
        file_id,
        nodes: Store::new(),
        roots: Vec::new(),
        import_paths: Vec::new(),
      });
    }

    // Parser phase
    let compilation_ctx = build_type_compilation_context(config);
    let mut parser = IgnisParser::new_with_compilation_ctx(lexer.tokens, self.symbol_table.clone(), compilation_ctx);
    let parse_result = parser.parse();

    let (nodes, roots) = match parse_result {
      Ok(r) => r,
      Err(errs) => {
        // Collect parser diagnostics
        for diag_msg in &errs {
          self.discovery_diagnostics.push(diag_msg.report());
        }

        // Return a minimal ParsedModule
        return Ok(ParsedModule {
          file_id,
          nodes: Store::new(),
          roots: Vec::new(),
          import_paths: Vec::new(),
        });
      },
    };

    trace_dbg!(
      config,
      DebugTrace::Parser,
      "parsed {} nodes for {} (LSP)",
      nodes.len(),
      path.display()
    );

    let import_paths = self.extract_imports(&nodes, &roots);

    log_trc!(config, "{} imports found in {} (LSP)", import_paths.len(), path.display());

    Ok(ParsedModule {
      file_id,
      nodes,
      roots,
      import_paths,
    })
  }

  /// Discover a std module for LSP mode.
  pub fn discover_std_module_lsp(
    &mut self,
    module_name: &str,
    config: &IgnisConfig,
  ) -> Result<ModuleId, ()> {
    let module_path = ModulePath::Std(module_name.to_string());

    if let Some(id) = self.module_graph.get_by_path(&module_path) {
      self.insert_module_path_aliases(module_name, &module_path, id);
      return Ok(id);
    }

    log_dbg!(config, "discovering std module {:?} (LSP)", module_path);

    let fs_path = self.module_graph.to_fs_path(&module_path);
    let parsed = match self.parse_file_lsp(&fs_path, config) {
      Ok(p) => p,
      Err(()) => return Err(()),
    };

    let file_id = parsed.file_id;
    let import_paths = parsed.import_paths.clone();

    let module = Module::new(file_id, module_path.clone());
    let module_id = self.module_graph.register(module);
    self.insert_module_path_aliases(module_name, &module_path, module_id);
    self.parsed_modules.insert(module_id, parsed);

    for (items, import_from, span) in import_paths {
      if let Ok(dep_id) = self.discover_recursive_lsp(&import_from, Some(&fs_path), config) {
        self.module_graph.add_import(module_id, items, dep_id, span);
      }
    }

    Ok(module_id)
  }

  pub fn compile(
    &mut self,
    root_id: ModuleId,
    config: &IgnisConfig,
  ) -> Result<ignis_analyzer::AnalyzerOutput, ()> {
    let (output, has_errors) = self.compile_collect_all(root_id, config)?;

    if has_errors {
      return Err(());
    }

    Ok(output)
  }

  pub(crate) fn compile_collect_all(
    &mut self,
    root_id: ModuleId,
    config: &IgnisConfig,
  ) -> Result<(ignis_analyzer::AnalyzerOutput, bool), ()> {
    let root_is_std = self.module_graph.modules.get(&root_id).path.is_std();

    // Prelude edges create cycles among std modules; skip cycle detection
    // for std roots and use all_modules_topological (same as check-std).
    if !root_is_std && let Err(err) = self.module_graph.detect_cycles() {
      match err {
        ModuleError::CircularDependency { cycle } => {
          if !cycle_is_embedded_std_only(&cycle, Path::new(&config.std_path)) {
            let cycle_str: Vec<String> = cycle.iter().map(|p| p.display().to_string()).collect();
            eprintln!(
              "{} Circular dependency detected: {}",
              "Error:".red().bold(),
              cycle_str.join(" -> ")
            );
            return Err(());
          }
        },
        _ => {
          eprintln!("{} Module error: {:?}", "Error:".red().bold(), err);
          return Err(());
        },
      }
    }

    self.module_graph.root = Some(root_id);

    let order = if root_is_std {
      self.module_graph.all_modules_topological()
    } else {
      self.module_graph.topological_sort()
    };

    log_dbg!(config, "compile order has {} modules", order.len());

    let (mut output, has_errors, _) = self.analyze_modules_collect_all(&order, config, true);

    let entry_point = if order.contains(&root_id) {
      let symbols = output.symbols.borrow();
      output.defs.iter().find_map(|(def_id, def)| {
        if def.owner_module != root_id {
          return None;
        }

        if def.owner_namespace.is_some() {
          return None;
        }

        let DefinitionKind::Function(func_def) = &def.kind else {
          return None;
        };

        if func_def.is_extern {
          return None;
        }

        if symbols.get(&def.name) == "main" {
          Some(def_id)
        } else {
          None
        }
      })
    } else {
      None
    };
    output.hir.entry_point = entry_point;

    Ok((output, has_errors))
  }

  /// Compile all modules in the graph (used for std library build).
  pub fn compile_all(
    &mut self,
    config: &IgnisConfig,
  ) -> Result<ignis_analyzer::AnalyzerOutput, ()> {
    let order = self.module_graph.all_modules_topological();

    if order.is_empty() {
      eprintln!("{} No modules to compile", "Error:".red().bold());
      return Err(());
    }

    log_dbg!(config, "compiling {} std modules", order.len());

    let mut output = self.analyze_modules(&order, config)?;
    output.hir.entry_point = None;

    Ok(output)
  }

  fn analyze_modules(
    &self,
    order: &[ModuleId],
    config: &IgnisConfig,
  ) -> Result<ignis_analyzer::AnalyzerOutput, ()> {
    let (output, has_errors, _) = self.analyze_modules_collect_all(order, config, true);

    if has_errors {
      return Err(());
    }

    Ok(output)
  }

  /// Analyze all modules in topological order, collecting ALL diagnostics.
  ///
  /// Unlike `analyze_modules`, this method does NOT fail early on errors.
  /// It continues analyzing all modules and returns all diagnostics.
  /// This is useful for LSP where we want to show all errors at once.
  ///
  /// Returns (output, has_errors, per-module semantic maps).
  ///
  /// Node-indexed semantic maps are tracked per module and also exposed separately,
  /// because `NodeId` values are only meaningful within their module AST store.
  pub(crate) fn analyze_modules_collect_all(
    &self,
    order: &[ModuleId],
    config: &IgnisConfig,
    render_diagnostics: bool,
  ) -> (ignis_analyzer::AnalyzerOutput, bool, HashMap<ModuleId, ModuleSemanticData>) {
    let mut export_table: ExportTable = HashMap::new();
    let mut shared_types = TypeStore::new();
    let mut shared_defs = DefinitionStore::new();
    let mut shared_namespaces = ignis_type::namespace::NamespaceStore::new();
    let mut shared_extension_methods = std::collections::HashMap::new();
    let mut combined_hir = HIR::new();
    let mut all_diagnostics = Vec::new();
    let mut has_errors = false;

    // These will only be populated for the root module (entry file)
    // because NodeIds are not globally unique across modules
    let mut root_node_defs = HashMap::new();
    let mut root_node_types = HashMap::new();
    let mut root_node_spans = HashMap::new();
    let mut root_resolved_calls = HashMap::new();
    let mut root_import_item_defs = HashMap::new();
    let mut root_import_module_files = HashMap::new();
    let mut per_module_semantic = HashMap::new();

    // The root module is the last one in topological order
    let root_module_id = self.module_graph.root;

    for &module_id in order {
      let parsed = self
        .parsed_modules
        .get(&module_id)
        .unwrap_or_else(|| panic!("internal error: module {:?} not found in parsed_modules", module_id));

      {
        // Scope para el borrow temporal de module_path
        let module_path = self.module_graph.modules.get(&module_id).path.clone();
        log_dbg!(config, "analyzing module {:?}", module_path);
      }

      // Prelude modules whose exports have already been analyzed are available
      // as implicit imports. Skip the module itself and any prelude module not
      // yet in the export table (it sits later in topo order, meaning it depends
      // on the current module — injecting it would be premature).
      let implicit_imports: Vec<ModuleId> = self
        .prelude_module_ids
        .iter()
        .copied()
        .filter(|&pid| pid != module_id && export_table.contains_key(&pid))
        .collect();

      // Build a per-module import-path override map.
      //
      // `parsed.import_paths` and `module.imports` are parallel: discover_recursive
      // iterates import_paths sequentially and calls add_import for each one, so
      // their indices align. We zip them to recover the raw import string that the
      // analyzer will look up and map it to the exact ModuleId that discovery resolved
      // for this specific module — preventing collisions when two modules share the
      // same relative import string (e.g. both import "./layout").
      //
      // The per-module map is layered on top of the global aliases: per-module wins.
      let module_for_path_local: HashMap<String, ModuleId> = {
        let module_imports = &self.module_graph.modules.get(&module_id).imports;
        let mut local = self.module_for_path.clone();

        for ((_, import_path, _), import_info) in parsed.import_paths.iter().zip(module_imports.iter()) {
          local.insert(import_path.clone(), import_info.source_module());
        }

        local
      };

      // Build the per-module path_to_file map from the local import resolution.
      let path_to_file_local: HashMap<String, ignis_type::file::FileId> = module_for_path_local
        .iter()
        .map(|(path, mid)| {
          let file_id = self.module_graph.modules.get(mid).file_id;
          (path.clone(), file_id)
        })
        .collect();

      let output = ignis_analyzer::Analyzer::analyze_with_shared_stores(
        &parsed.nodes,
        &parsed.roots,
        self.symbol_table.clone(),
        &export_table,
        &module_for_path_local,
        &path_to_file_local,
        &mut shared_types,
        &mut shared_defs,
        &mut shared_namespaces,
        &mut shared_extension_methods,
        module_id,
        implicit_imports,
      );

      if render_diagnostics && !config.quiet {
        for diag in &output.diagnostics {
          ignis_diagnostics::render(diag, &self.source_map);
        }
      }

      trace_dbg!(
        config,
        DebugTrace::Analyzer,
        "module {:?} emitted {} diagnostics",
        module_id,
        output.diagnostics.len()
      );

      let module_has_errors = output
        .diagnostics
        .iter()
        .any(|d| matches!(d.severity, ignis_diagnostics::diagnostic_report::Severity::Error));

      if module_has_errors {
        has_errors = true;
      }

      let exports = output.collect_exports();

      let module_semantic = ModuleSemanticData {
        node_defs: output.node_defs,
        node_types: output.node_types,
        node_spans: output.node_spans,
        resolved_calls: output.resolved_calls,
        import_item_defs: output.import_item_defs,
        import_module_files: output.import_module_files,
      };

      if root_module_id == Some(module_id) {
        root_node_defs = module_semantic.node_defs.clone();
        root_node_types = module_semantic.node_types.clone();
        root_node_spans = module_semantic.node_spans.clone();
        root_resolved_calls = module_semantic.resolved_calls.clone();
        root_import_item_defs = module_semantic.import_item_defs.clone();
        root_import_module_files = module_semantic.import_module_files.clone();
      }

      per_module_semantic.insert(module_id, module_semantic);

      combined_hir.merge(output.hir);
      all_diagnostics.extend(output.diagnostics);

      export_table.insert(module_id, exports);
    }

    let output = ignis_analyzer::AnalyzerOutput {
      ast: ignis_type::Store::new(),
      types: shared_types,
      defs: shared_defs,
      namespaces: shared_namespaces,
      hir: combined_hir,
      diagnostics: all_diagnostics,
      symbols: self.symbol_table.clone(),
      node_defs: root_node_defs,
      node_types: root_node_types,
      node_spans: root_node_spans,
      resolved_calls: root_resolved_calls,
      import_item_defs: root_import_item_defs,
      import_module_files: root_import_module_files,
      extension_methods: shared_extension_methods,
      directive_registry: ignis_analyzer::directive_registry::DirectiveRegistry::default(),
      directive_execution_report: ignis_analyzer::directive_scheduler::DirectiveExecutionReport::default(),
      current_module: root_module_id.unwrap_or(ModuleId::new(0)),
      reexported_defs: HashMap::new(),
    };

    (output, has_errors, per_module_semantic)
  }
}

fn cycle_is_embedded_std_only(
  cycle: &[PathBuf],
  std_root: &Path,
) -> bool {
  if std_root.as_os_str().is_empty() || cycle.len() < 2 {
    return false;
  }

  let Some(repeated_module) = cycle.last() else {
    return false;
  };

  let repeated_index = cycle[..cycle.len() - 1].iter().position(|path| path == repeated_module);
  let Some(repeated_index) = repeated_index else {
    return false;
  };

  let canonical_std_root = std::fs::canonicalize(std_root).unwrap_or_else(|_| std_root.to_path_buf());
  cycle[repeated_index..].iter().all(|path| {
    let canonical_path = std::fs::canonicalize(path).unwrap_or_else(|_| path.to_path_buf());
    canonical_path.starts_with(&canonical_std_root)
  })
}

#[cfg(test)]
mod tests {
  use std::fs;

  use ignis_config::{IgnisSTDManifest, StdAutoLoad};

  use super::*;

  #[test]
  fn resolves_std_module_name_from_any_file_under_std_root() {
    let temp = tempfile::tempdir().expect("create temp std root");
    let hash_set_path = temp.path().join("collections/hash_set.ign");
    let string_mod_path = temp.path().join("string/mod.ign");

    fs::create_dir_all(hash_set_path.parent().expect("hash set parent")).expect("create hash set parent");
    fs::create_dir_all(string_mod_path.parent().expect("string parent")).expect("create string parent");
    fs::write(&hash_set_path, "").expect("write hash set module");
    fs::write(&string_mod_path, "").expect("write string module");

    let mut config = IgnisConfig::default();
    config.std_path = temp.path().to_string_lossy().into_owned();

    let ctx = CompilationContext::new(&config);

    assert_eq!(
      ctx.try_resolve_std_module_name(hash_set_path.to_string_lossy().as_ref()),
      Some("collections::hash_set".to_string())
    );
    assert_eq!(
      ctx.try_resolve_std_module_name(string_mod_path.to_string_lossy().as_ref()),
      Some("string".to_string())
    );
  }

  #[test]
  fn leaves_std_test_companions_as_project_modules() {
    let temp = tempfile::tempdir().expect("create temp std root");
    let test_path = temp.path().join("collections/tests.ign");

    fs::create_dir_all(test_path.parent().expect("tests parent")).expect("create tests parent");
    fs::write(&test_path, "").expect("write tests module");

    let mut config = IgnisConfig::default();
    config.std_path = temp.path().to_string_lossy().into_owned();

    let ctx = CompilationContext::new(&config);

    assert_eq!(ctx.try_resolve_std_module_name(test_path.to_string_lossy().as_ref()), None);
  }

  #[test]
  fn prelude_std_modules_comes_only_from_manifest_auto_load() {
    let mut config = IgnisConfig::default();
    config.manifest = IgnisSTDManifest {
      auto_load: Some(StdAutoLoad {
        modules: vec!["option".to_string(), "result".to_string()],
      }),
      ..IgnisSTDManifest::default()
    };

    assert_eq!(CompilationContext::prelude_std_modules(&config), vec!["option", "result"]);

    config.manifest.auto_load = None;
    assert!(
      CompilationContext::prelude_std_modules(&config).is_empty(),
      "std prelude modules must come from manifest auto_load.modules, not a hardcoded fallback"
    );
  }

  #[test]
  fn std_root_topological_order_keeps_explicit_std_imports_before_importers() {
    let temp = tempfile::tempdir().expect("create temp std root");
    let option_path = temp.path().join("option/mod.ign");
    let vector_path = temp.path().join("vector/mod.ign");

    fs::create_dir_all(option_path.parent().expect("option parent")).expect("create option parent");
    fs::create_dir_all(vector_path.parent().expect("vector parent")).expect("create vector parent");
    fs::write(&option_path, "export enum Option<T> { SOME(T), NONE }\n").expect("write option module");
    fs::write(&vector_path, "import Option from \"std::option\";\nexport record Vector {}\n")
      .expect("write vector module");
    fs::write(
      temp.path().join("manifest.toml"),
      r#"
[toolchain]

[modules]
option = "option/mod.ign"
vector = "vector/mod.ign"

[auto_load]
modules = ["vector", "option"]
"#,
    )
    .expect("write manifest");

    let mut config = IgnisConfig::default();
    config.std = true;
    config.auto_load_std = true;
    config.std_path = temp.path().to_string_lossy().into_owned();
    config.manifest = CompilationContext::load_std_manifest_if_needed(&config).expect("load std manifest");

    let mut ctx = CompilationContext::new(&config);
    ctx.discover_all_std_modules(&config);
    ctx.discover_prelude_modules_for_all(&config);

    let order = ctx.module_graph.all_modules_topological();
    let option_id = *ctx.module_for_path.get("std::option").expect("std::option alias");
    let vector_id = *ctx.module_for_path.get("std::vector").expect("std::vector alias");

    let option_index = order.iter().position(|id| *id == option_id).expect("option in order");
    let vector_index = order.iter().position(|id| *id == vector_id).expect("vector in order");

    assert!(
      option_index < vector_index,
      "std::option must be analyzed before std::vector because vector explicitly imports it"
    );
  }

  #[test]
  fn real_std_orders_option_before_vector_for_std_root_analysis() {
    let std_path = Path::new(env!("CARGO_MANIFEST_DIR")).join("../../std");

    let mut config = IgnisConfig::default();
    config.std = true;
    config.auto_load_std = true;
    config.std_path = std_path.to_string_lossy().into_owned();
    config.manifest = CompilationContext::load_std_manifest_if_needed(&config).expect("load real std manifest");

    let mut ctx = CompilationContext::new(&config);
    ctx.discover_all_std_modules(&config);
    ctx.discover_prelude_modules_for_all(&config);

    let order = ctx.module_graph.all_modules_topological();
    let option_id = *ctx.module_for_path.get("std::option").expect("std::option alias");
    let vector_id = *ctx.module_for_path.get("std::vector").expect("std::vector alias");

    let option_index = order.iter().position(|id| *id == option_id).expect("option in order");
    let vector_index = order.iter().position(|id| *id == vector_id).expect("vector in order");

    assert!(
      option_index < vector_index,
      "std::option must be analyzed before std::vector because vector explicitly imports it"
    );
  }

  #[test]
  fn loads_std_manifest_from_std_path_when_config_manifest_is_empty() {
    let temp = tempfile::tempdir().expect("create temp std root");
    fs::write(
      temp.path().join("manifest.toml"),
      r#"
[toolchain]

[modules]
option = "option/mod.ign"
string = "string/mod.ign"
"#,
    )
    .expect("write manifest");

    let mut config = IgnisConfig::default();
    config.std_path = temp.path().to_string_lossy().into_owned();

    let ctx = CompilationContext::new(&config);

    assert_eq!(
      ctx
        .module_graph
        .manifest
        .as_ref()
        .and_then(|manifest| manifest.get_module_path("option")),
      Some(&"option/mod.ign".to_string())
    );
  }

  /// Regression test for the relative-import collision bug (W2).
  ///
  /// When two modules in different directories both import a file named
  /// `./layout`, the global `module_for_path` map only keeps the first
  /// registration because the second insertion is skipped (the key is
  /// already mapped to a different ModuleId). Without the per-module
  /// override, both modules would resolve `./layout` to the same ModuleId,
  /// causing wrong symbol resolution.
  ///
  /// This test verifies that discovery assigns distinct ModuleIds to the
  /// two `layout.ign` files and that the per-module import override in
  /// `analyze_modules_collect_all` can correctly reconstruct which
  /// ModuleId each module's `./layout` import refers to.
  #[test]
  fn relative_imports_with_same_string_resolve_per_module() {
    let temp = tempfile::tempdir().expect("create temp project");

    // Directory layout:
    //   src/
    //     main.ign         -- imports ./mod_a and ./mod_b
    //     mod_a/
    //       mod.ign        -- exports nothing, imports ./layout
    //       layout.ign     -- exports LayoutA (distinct from LayoutB)
    //     mod_b/
    //       mod.ign        -- exports nothing, imports ./layout
    //       layout.ign     -- exports LayoutB (distinct from LayoutA)

    let src = temp.path().join("src");
    let mod_a = src.join("mod_a");
    let mod_b = src.join("mod_b");

    fs::create_dir_all(&mod_a).expect("create mod_a dir");
    fs::create_dir_all(&mod_b).expect("create mod_b dir");

    fs::write(
      src.join("main.ign"),
      "import _ from \"./mod_a\";\nimport _ from \"./mod_b\";\nfunction main(): i32 { return 0; }\n",
    )
    .expect("write main.ign");

    fs::write(mod_a.join("mod.ign"), "export LayoutA from \"./layout\";\n").expect("write mod_a/mod.ign");

    fs::write(mod_a.join("layout.ign"), "export record LayoutA {\n  x: i32;\n}\n").expect("write mod_a/layout.ign");

    fs::write(mod_b.join("mod.ign"), "export LayoutB from \"./layout\";\n").expect("write mod_b/mod.ign");

    fs::write(mod_b.join("layout.ign"), "export record LayoutB {\n  y: i32;\n}\n").expect("write mod_b/layout.ign");

    let mut config = IgnisConfig::default();
    config.std_path = String::new();
    config.quiet = true;

    let mut ctx = CompilationContext::new(&config);
    let entry = src.join("main.ign");
    let root_id = ctx
      .discover_modules(entry.to_str().expect("entry path"), &config)
      .expect("discovery must succeed");

    // After discovery, mod_a/layout.ign and mod_b/layout.ign must be separate modules.
    // Both are registered under the absolute path; collect their ModuleIds.
    let layout_a_path = mod_a.join("layout.ign");
    let layout_b_path = mod_b.join("layout.ign");

    // Find the ModuleId for each layout by looking at their file_ids in the source map.
    let layout_a_file_id = ctx.source_map.lookup_by_path(&layout_a_path);
    let layout_b_file_id = ctx.source_map.lookup_by_path(&layout_b_path);

    assert!(
      layout_a_file_id.is_some(),
      "mod_a/layout.ign must have been parsed and registered in the source map"
    );
    assert!(
      layout_b_file_id.is_some(),
      "mod_b/layout.ign must have been parsed and registered in the source map"
    );

    let layout_a_fid = layout_a_file_id.expect("layout_a file_id");
    let layout_b_fid = layout_b_file_id.expect("layout_b file_id");

    assert_ne!(
      layout_a_fid, layout_b_fid,
      "mod_a/layout.ign and mod_b/layout.ign must be distinct files"
    );

    // Resolve ModuleIds for each layout file via the parsed_modules table.
    let layout_a_mid = ctx
      .parsed_modules
      .iter()
      .find_map(|(mid, pm)| if pm.file_id == layout_a_fid { Some(*mid) } else { None })
      .expect("mod_a/layout.ign must have a ModuleId");

    let layout_b_mid = ctx
      .parsed_modules
      .iter()
      .find_map(|(mid, pm)| if pm.file_id == layout_b_fid { Some(*mid) } else { None })
      .expect("mod_b/layout.ign must have a ModuleId");

    assert_ne!(
      layout_a_mid, layout_b_mid,
      "mod_a/layout.ign and mod_b/layout.ign must have distinct ModuleIds"
    );

    // Verify that the per-module override logic can reconstruct the correct mapping.
    //
    // For mod_a/mod.ign: parsed.import_paths[0].1 == "./layout"
    //                    module.imports[0].source_module() == layout_a_mid
    //
    // For mod_b/mod.ign: parsed.import_paths[0].1 == "./layout"
    //                    module.imports[0].source_module() == layout_b_mid
    //
    // If they were the same ModuleId the bug would still be present.

    let mod_a_mid = ctx
      .parsed_modules
      .iter()
      .find_map(|(mid, pm)| {
        let mod_a_mod_fid = ctx.source_map.lookup_by_path(&mod_a.join("mod.ign"))?;
        if pm.file_id == mod_a_mod_fid { Some(*mid) } else { None }
      })
      .expect("mod_a/mod.ign must have a ModuleId");

    let mod_b_mid = ctx
      .parsed_modules
      .iter()
      .find_map(|(mid, pm)| {
        let mod_b_mod_fid = ctx.source_map.lookup_by_path(&mod_b.join("mod.ign"))?;
        if pm.file_id == mod_b_mod_fid { Some(*mid) } else { None }
      })
      .expect("mod_b/mod.ign must have a ModuleId");

    let mod_a_module = ctx.module_graph.modules.get(&mod_a_mid);
    let mod_b_module = ctx.module_graph.modules.get(&mod_b_mid);

    // Each mod.ign has exactly one import (./layout).
    let mod_a_layout_import = mod_a_module
      .imports
      .iter()
      .find(|imp| imp.source_module() == layout_a_mid);
    let mod_b_layout_import = mod_b_module
      .imports
      .iter()
      .find(|imp| imp.source_module() == layout_b_mid);

    assert!(
      mod_a_layout_import.is_some(),
      "mod_a/mod.ign must import layout_a_mid, not layout_b_mid — the per-module override must resolve correctly"
    );
    assert!(
      mod_b_layout_import.is_some(),
      "mod_b/mod.ign must import layout_b_mid, not layout_a_mid — the per-module override must resolve correctly"
    );

    // Finally, verify that the global module_for_path only has one entry for "./layout"
    // (the bug surface: the second insertion was silently skipped), while the per-module
    // override correctly provides both.
    let global_layout_id = ctx.module_for_path.get("./layout").copied();
    assert!(
      global_layout_id == Some(layout_a_mid) || global_layout_id == Some(layout_b_mid),
      "global module_for_path must have exactly one ./layout entry (whichever was registered first)"
    );

    // The per-module overrides for mod_a and mod_b each have the right mapping.
    let mod_a_parsed = ctx.parsed_modules.get(&mod_a_mid).expect("mod_a parsed");
    let mod_a_local: HashMap<String, ModuleId> = {
      let imports = &ctx.module_graph.modules.get(&mod_a_mid).imports;
      let mut local = ctx.module_for_path.clone();
      for ((_, import_path, _), import_info) in mod_a_parsed.import_paths.iter().zip(imports.iter()) {
        local.insert(import_path.clone(), import_info.source_module());
      }
      local
    };

    let mod_b_parsed = ctx.parsed_modules.get(&mod_b_mid).expect("mod_b parsed");
    let mod_b_local: HashMap<String, ModuleId> = {
      let imports = &ctx.module_graph.modules.get(&mod_b_mid).imports;
      let mut local = ctx.module_for_path.clone();
      for ((_, import_path, _), import_info) in mod_b_parsed.import_paths.iter().zip(imports.iter()) {
        local.insert(import_path.clone(), import_info.source_module());
      }
      local
    };

    assert_eq!(
      mod_a_local.get("./layout"),
      Some(&layout_a_mid),
      "per-module override for mod_a must resolve ./layout to layout_a_mid"
    );
    assert_eq!(
      mod_b_local.get("./layout"),
      Some(&layout_b_mid),
      "per-module override for mod_b must resolve ./layout to layout_b_mid"
    );

    // Sanity: if the global map resolved ./layout wrong for one of them,
    // the per-module maps must differ from each other.
    assert_ne!(
      mod_a_local.get("./layout"),
      mod_b_local.get("./layout"),
      "mod_a and mod_b must resolve ./layout to different ModuleIds via per-module overrides"
    );

    // Full compilation must succeed with no errors.
    let (_, has_errors) = ctx
      .compile_collect_all(root_id, &config)
      .expect("compile must not fail");
    assert!(
      !has_errors,
      "compilation must produce no errors after per-module import path fix"
    );
  }
}

fn build_type_compilation_context(config: &IgnisConfig) -> TypeCompilationContext {
  let mut ctx = if config.target_triple.is_empty() {
    TypeCompilationContext::default()
  } else {
    TypeCompilationContext::from_target_triple(&config.target_triple)
  };

  ctx.debug = config.debug;
  ctx.features = config.enabled_features.clone();
  ctx.known_features = config.known_features.clone();
  ctx
}
