use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::{cell::RefCell, rc::Rc};

use colored::*;
use ignis_analyzer::imports::ExportTable;
use ignis_analyzer::modules::{ModuleError, ModuleGraph};
use ignis_ast::{ASTNode, NodeId, statements::ASTStatement};
use ignis_config::{DebugTrace, IgnisConfig};
use ignis_diagnostics::diagnostic_report::Diagnostic;

use ignis_log::{log_dbg, log_trc, phase_log, trace_dbg};
use ignis_parser::{IgnisLexer, IgnisParser};
use ignis_hir::HIR;
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
  pub fn new(config: &IgnisConfig) -> Self {
    let project_root = config
      .project_config
      .as_ref()
      .map(|p| PathBuf::from(&p.build.source_dir));

    let std_path = PathBuf::from(&config.std_path);

    let module_graph = if config.manifest.modules.is_empty() {
      ModuleGraph::new(project_root, std_path)
    } else {
      ModuleGraph::with_manifest(project_root, std_path, config.manifest.clone())
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

  /// Fallback std modules implicitly available in every compilation when
  /// manifest auto-load configuration is missing.
  const DEFAULT_PRELUDE_STD_MODULES: &'static [&'static str] = &["string", "number", "vector", "types"];

  fn prelude_std_modules(config: &IgnisConfig) -> Vec<&str> {
    let mut modules = config.manifest.get_auto_load_modules();

    if modules.is_empty() {
      modules.extend(Self::DEFAULT_PRELUDE_STD_MODULES.iter().copied());
    }

    modules
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
  /// For each (module M, prelude P) pair, adds `M → P` only if P does
  /// not already transitively depend on M via existing explicit edges.
  /// Reachability is checked against a snapshot of the pre-prelude graph
  /// so that the result is independent of insertion order.
  fn add_safe_prelude_edges(&mut self) {
    use std::collections::HashSet;
    use ignis_type::BytePosition;

    // Snapshot: for each prelude module, compute which modules it can reach
    // via existing (explicit) edges. This snapshot is frozen before we
    // start adding prelude edges.
    let prelude_reachable: HashMap<ModuleId, HashSet<ModuleId>> = self
      .prelude_module_ids
      .iter()
      .map(|&pid| {
        let deps: HashSet<ModuleId> = self.module_graph.transitive_deps(pid).into_iter().collect();
        (pid, deps)
      })
      .collect();

    let all_module_ids: Vec<ModuleId> = self.module_graph.modules.iter().map(|(mid, _)| mid).collect();

    for module_id in all_module_ids {
      let file_id = self.module_graph.modules.get(&module_id).file_id;
      let dummy_span = Span::empty_at(file_id, BytePosition::default());

      for &prelude_id in &self.prelude_module_ids {
        if module_id == prelude_id {
          continue;
        }

        // Would adding module_id → prelude_id create a cycle?
        // That happens iff prelude_id already reaches module_id.
        if let Some(reachable) = prelude_reachable.get(&prelude_id)
          && reachable.contains(&module_id)
        {
          continue;
        }

        // Don't add duplicate edges.
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

  /// Discover a std module by name (e.g., "io", "string").
  /// Creates `ModulePath::Std` entries for proper std/project distinction.
  pub fn discover_std_module(
    &mut self,
    module_name: &str,
    config: &IgnisConfig,
  ) -> Result<ModuleId, ()> {
    let module_path = ModulePath::Std(module_name.to_string());

    if let Some(id) = self.module_graph.get_by_path(&module_path) {
      return Ok(id);
    }

    log_dbg!(config, "discovering std module {:?}", module_path);

    let fs_path = self.module_graph.to_fs_path(&module_path);
    let parsed = self.parse_file(&fs_path, config)?;
    let file_id = parsed.file_id;
    let import_paths = parsed.import_paths.clone();

    let module = Module::new(file_id, module_path.clone());
    let module_id = self.module_graph.register(module);
    self.module_for_path.insert(format!("std::{}", module_name), module_id);
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
    } else {
      ModulePath::Project(PathBuf::from(path))
    };

    if let Some(id) = self.module_graph.get_by_path(&module_path) {
      return Ok(id);
    }

    log_dbg!(config, "discovering module {:?}", module_path);

    // Placeholder to prevent infinite recursion on cyclic imports
    let placeholder_id = ModuleId::new(self.module_graph.modules.iter().count() as u32);
    self.module_for_path.insert(path.to_string(), placeholder_id);

    let fs_path = self.module_graph.to_fs_path(&module_path);
    let parsed = match self.parse_file(&fs_path, config) {
      Ok(p) => p,
      Err(()) => {
        self.module_for_path.remove(path);
        return Err(());
      },
    };
    let file_id = parsed.file_id;
    let import_paths = parsed.import_paths.clone();

    let module = Module::new(file_id, module_path.clone());
    let module_id = self.module_graph.register(module);
    self.module_for_path.insert(path.to_string(), module_id);
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

    let mut parser = IgnisParser::new(lexer.tokens, self.symbol_table.clone());
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
      if let ASTNode::Statement(ASTStatement::Import(import)) = nodes.get(root) {
        let symbol_ids: Vec<SymbolId> = import.items.iter().map(|item| item.name).collect();
        imports.push((symbol_ids, import.from.clone(), import.span.clone()));
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
    } else {
      ModulePath::Project(PathBuf::from(path))
    };

    if let Some(id) = self.module_graph.get_by_path(&module_path) {
      return Ok(id);
    }

    log_dbg!(config, "discovering module {:?} (LSP mode)", module_path);

    // Placeholder to prevent infinite recursion on cyclic imports
    let placeholder_id = ModuleId::new(self.module_graph.modules.iter().count() as u32);
    self.module_for_path.insert(path.to_string(), placeholder_id);

    let fs_path = self.module_graph.to_fs_path(&module_path);

    let parsed = match self.parse_file_lsp(&fs_path, config) {
      Ok(p) => p,
      Err(()) => {
        self.module_for_path.remove(path);
        return Err(());
      },
    };

    let file_id = parsed.file_id;
    let import_paths = parsed.import_paths.clone();

    let module = Module::new(file_id, module_path.clone());
    let module_id = self.module_graph.register(module);
    self.module_for_path.insert(path.to_string(), module_id);
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
    let mut parser = IgnisParser::new(lexer.tokens, self.symbol_table.clone());
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
    self.module_for_path.insert(format!("std::{}", module_name), module_id);
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
    if let Err(err) = self.module_graph.detect_cycles() {
      match err {
        ModuleError::CircularDependency { cycle } => {
          let cycle_str: Vec<String> = cycle.iter().map(|p| p.display().to_string()).collect();
          eprintln!(
            "{} Circular dependency detected: {}",
            "Error:".red().bold(),
            cycle_str.join(" -> ")
          );
        },
        _ => eprintln!("{} Module error: {:?}", "Error:".red().bold(), err),
      }
      return Err(());
    }

    self.module_graph.root = Some(root_id);
    let order = self.module_graph.topological_sort();

    log_dbg!(config, "compile order has {} modules", order.len());

    let mut output = self.analyze_modules(&order, config)?;

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

    Ok(output)
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

    // Build path_to_file mapping for import path resolution
    let path_to_file: HashMap<String, ignis_type::file::FileId> = self
      .module_for_path
      .iter()
      .map(|(path, module_id)| {
        let module = self.module_graph.modules.get(module_id);
        (path.clone(), module.file_id)
      })
      .collect();

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

      let output = ignis_analyzer::Analyzer::analyze_with_shared_stores(
        &parsed.nodes,
        &parsed.roots,
        self.symbol_table.clone(),
        &export_table,
        &self.module_for_path,
        &path_to_file,
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
    };

    (output, has_errors, per_module_semantic)
  }
}
