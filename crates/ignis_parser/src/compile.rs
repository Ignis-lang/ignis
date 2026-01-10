use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::{cell::RefCell, rc::Rc};

use colored::*;
use ignis_analyzer::imports::ExportTable;
use ignis_analyzer::modules::{ModuleError, ModuleGraph};
use ignis_ast::{ASTNode, NodeId, statements::ASTStatement};
use ignis_config::IgnisConfig;
use ignis_type::file::{FileId, SourceMap};
use ignis_type::module::{Module, ModuleId, ModulePath};
use ignis_type::symbol::{SymbolId, SymbolTable};
use ignis_type::Store;

use crate::{IgnisLexer, parser::IgnisParser};

/// Parsed module data
struct ParsedModule {
  file_id: FileId,
  nodes: Store<ASTNode>,
  roots: Vec<NodeId>,
  import_paths: Vec<(Vec<SymbolId>, String, ignis_type::span::Span)>,
}

/// Discovery and compilation context
pub struct CompilationContext {
  pub source_map: SourceMap,
  pub symbol_table: Rc<RefCell<SymbolTable>>,
  pub module_graph: ModuleGraph,
  parsed_modules: HashMap<ModuleId, ParsedModule>,
  module_for_path: HashMap<String, ModuleId>,
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
    }
  }

  /// Discover and parse all modules starting from entry point
  pub fn discover_modules(
    &mut self,
    entry_path: &str,
    config: &IgnisConfig,
  ) -> Result<ModuleId, ()> {
    self.discover_recursive(entry_path, None, config)
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

    if !config.quiet {
      println!(
        "{:indent$}Scanning... {}",
        "-->".bright_green().bold(),
        path.display(),
        indent = 4
      );
    }

    let file_id = self.source_map.add_file(path, text);
    let src = &self.source_map.get(&file_id).text.clone();

    let mut lexer = IgnisLexer::new(file_id, &src);
    lexer.scan_tokens();

    if !lexer.diagnostics.is_empty() {
      for diag_msg in &lexer.diagnostics {
        ignis_diagnostics::render(&diag_msg.report(), &self.source_map);
      }
      return Err(());
    }

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

    let import_paths = self.extract_imports(&nodes, &roots);

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
        imports.push((import.items.clone(), import.from.clone(), import.span.clone()));
      }
    }

    imports
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

    if !config.quiet {
      println!("\n{}", "Analyzing modules...".bright_cyan().bold());
    }

    let mut export_table: ExportTable = HashMap::new();
    let mut last_output: Option<ignis_analyzer::AnalyzerOutput> = None;

    for module_id in order {
      let parsed = self.parsed_modules.get(&module_id).unwrap();

      let output = ignis_analyzer::Analyzer::analyze_with_imports(
        &parsed.nodes,
        &parsed.roots,
        self.symbol_table.clone(),
        &export_table,
        &self.module_for_path,
      );

      if !config.quiet {
        for diag in &output.diagnostics {
          ignis_diagnostics::render(diag, &self.source_map);
        }
      }

      let has_errors = output
        .diagnostics
        .iter()
        .any(|d| matches!(d.severity, ignis_diagnostics::diagnostic_report::Severity::Error));

      if has_errors {
        return Err(());
      }

      export_table.insert(module_id, output.collect_exports());
      last_output = Some(output);
    }

    last_output.ok_or(())
  }
}

pub fn compile_project(
  config: Arc<IgnisConfig>,
  entry_path: &str,
) -> Result<(), ()> {
  let mut ctx = CompilationContext::new(&config);
  let root_id = ctx.discover_modules(entry_path, &config)?;
  let output = ctx.compile(root_id, &config)?;

  let sym_table = output.symbols.borrow();
  if let Some(bc) = config.build_config.as_ref() {
    if bc.dump_types {
      println!("\n{}", ignis_analyzer::dump::dump_types(&output.types));
    }
    if bc.dump_defs {
      println!("\n{}", ignis_analyzer::dump::dump_defs(&output.defs, &output.types, &sym_table));
    }
    if bc.dump_hir_summary {
      println!(
        "\n{}",
        ignis_analyzer::dump::dump_hir_summary(&output.hir, &output.defs, &sym_table)
      );
    }
    if let Some(func_name) = &bc.dump_hir {
      match ignis_analyzer::dump::dump_hir_function(&output.hir, &output.defs, &sym_table, func_name) {
        Ok(out) => println!("\n{}", out),
        Err(err) => eprintln!("{} {}", "Error:".red().bold(), err),
      }
    }
  }

  if config.debug.contains(&ignis_config::DebugPrint::Analyzer) {
    println!("\n{}", "Type Store & Definitions:".bright_cyan().bold());
    println!("{}", ignis_analyzer::dump::dump_types(&output.types));
    println!("{}", ignis_analyzer::dump::dump_defs(&output.defs, &output.types, &sym_table));
  }

  if config.debug.contains(&ignis_config::DebugPrint::Hir) {
    println!("\n{}", "HIR:".bright_cyan().bold());
    println!(
      "{}",
      ignis_analyzer::dump::dump_hir_complete(&output.hir, &output.types, &output.defs, &sym_table)
    );
  }

  Ok(())
}
