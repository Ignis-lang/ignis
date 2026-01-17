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

/// Discovery and compilation context
pub struct CompilationContext {
  pub source_map: SourceMap,
  pub symbol_table: Rc<RefCell<SymbolTable>>,
  pub module_graph: ModuleGraph,
  pub(crate) parsed_modules: HashMap<ModuleId, ParsedModule>,
  module_for_path: HashMap<String, ModuleId>,

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
    let module_path = if current_file.is_some() {
      match self.module_graph.resolve_import_path(path, current_file.unwrap()) {
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

    self
      .module_for_path
      .insert(path.to_string(), ModuleId::new(self.module_graph.modules.iter().count() as u32));

    let fs_path = self.module_graph.to_fs_path(&module_path);
    let parsed = self.parse_file(&fs_path, config)?;
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

    let mut lexer = IgnisLexer::new(file_id, &src);
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
    let module_path = if current_file.is_some() {
      match self.module_graph.resolve_import_path(path, current_file.unwrap()) {
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

    self
      .module_for_path
      .insert(path.to_string(), ModuleId::new(self.module_graph.modules.iter().count() as u32));

    let fs_path = self.module_graph.to_fs_path(&module_path);

    // Parse file, collecting diagnostics instead of rendering
    let parsed = match self.parse_file_lsp(&fs_path, config) {
      Ok(p) => p,
      Err(()) => {
        // File couldn't be read - already logged
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
    let (output, has_errors) = self.analyze_modules_collect_all(order, config, true);

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
  /// Returns (output, has_errors).
  ///
  /// Note: node_defs, node_types, node_spans, and resolved_calls are only kept for
  /// the ROOT module (last in topological order). This is because NodeIds are indices
  /// into per-module AST stores and would collide if merged naively.
  pub fn analyze_modules_collect_all(
    &self,
    order: &[ModuleId],
    config: &IgnisConfig,
    render_diagnostics: bool,
  ) -> (ignis_analyzer::AnalyzerOutput, bool) {
    let mut export_table: ExportTable = HashMap::new();
    let mut shared_types = TypeStore::new();
    let mut shared_defs = DefinitionStore::new();
    let mut shared_namespaces = ignis_type::namespace::NamespaceStore::new();
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

      let output = ignis_analyzer::Analyzer::analyze_with_shared_stores(
        &parsed.nodes,
        &parsed.roots,
        self.symbol_table.clone(),
        &export_table,
        &self.module_for_path,
        &mut shared_types,
        &mut shared_defs,
        &mut shared_namespaces,
        module_id,
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
      combined_hir.merge(output.hir);
      all_diagnostics.extend(output.diagnostics);

      // Only keep node_* data for the root module to avoid NodeId collisions
      if root_module_id == Some(module_id) {
        root_node_defs = output.node_defs;
        root_node_types = output.node_types;
        root_node_spans = output.node_spans;
        root_resolved_calls = output.resolved_calls;
        root_import_item_defs = output.import_item_defs;
      }

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
    };

    (output, has_errors)
  }
}
