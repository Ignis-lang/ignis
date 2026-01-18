//! Pure analysis API for reuse by CLI and LSP.
//!
//! This module provides side-effect-free functions that:
//! - Do NOT print to stdout/stderr
//! - Do NOT render diagnostics
//! - Do NOT write dump files
//! - Only produce data structures

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use ignis_ast::{ASTNode, NodeId};
use ignis_config::IgnisConfig;
use ignis_diagnostics::diagnostic_report::Diagnostic;
use ignis_parser::{IgnisLexer, IgnisParser};
use ignis_token::token::Token;
use ignis_type::definition::{DefinitionId, DefinitionStore};
use ignis_type::file::{FileId, SourceMap};
use ignis_type::span::Span;
use ignis_type::symbol::{SymbolId, SymbolTable};
use ignis_type::types::{TypeId, TypeStore};
use ignis_type::Store;

use crate::context::CompilationContext;

/// Output of `analyze_text`, containing all artifacts from lexing, parsing, and analysis.
///
/// This struct always contains results, even when errors occurred.
/// Check `has_errors` to determine if the analysis succeeded.
pub struct AnalyzeTextOutput {
  /// The source map containing the analyzed file.
  pub source_map: SourceMap,

  /// ID of the analyzed file within the source map.
  pub file_id: FileId,

  /// All tokens produced by the lexer.
  /// Useful for LSP semantic tokens.
  pub tokens: Vec<Token>,

  /// AST nodes produced by the parser.
  /// May be empty if lexer failed.
  pub nodes: Store<ASTNode>,

  /// Root node IDs in the AST.
  /// May be empty if lexer/parser failed.
  pub roots: Vec<NodeId>,

  /// Symbol table shared across parsing and analysis.
  pub symbol_table: Rc<RefCell<SymbolTable>>,

  /// Analyzer output (types, defs, HIR, etc.).
  /// Contains default/empty values if parsing failed.
  pub analyzer: ignis_analyzer::AnalyzerOutput,

  /// All accumulated diagnostics from lexer, parser, and analyzer.
  pub diagnostics: Vec<Diagnostic>,

  /// True if any diagnostic is an error.
  pub has_errors: bool,
}

/// Analyze source text without any side effects.
///
/// This function runs the full pipeline (lex -> parse -> analyze) and returns
/// all artifacts. It never prints, never writes files, and always returns `Ok`.
///
/// # Arguments
/// * `file_path` - Path used for error reporting (does not read from disk)
/// * `text` - Source code to analyze
///
/// # Returns
/// Always returns `Ok(AnalyzeTextOutput)`. Check `output.has_errors` to see if
/// errors occurred. All diagnostics are accumulated in `output.diagnostics`.
pub fn analyze_text(
  file_path: &str,
  text: String,
) -> AnalyzeTextOutput {
  let mut source_map = SourceMap::new();
  let mut diagnostics: Vec<Diagnostic> = Vec::new();

  let file_id = source_map.add_file(file_path, text);
  let src = source_map.get(&file_id).text.clone();

  // Lexer phase
  let mut lexer = IgnisLexer::new(file_id, &src);
  lexer.scan_tokens();

  let tokens = lexer.tokens.clone();

  for diag_msg in &lexer.diagnostics {
    diagnostics.push(diag_msg.report());
  }

  if !lexer.diagnostics.is_empty() {
    return AnalyzeTextOutput {
      source_map,
      file_id,
      tokens,
      nodes: Store::new(),
      roots: Vec::new(),
      symbol_table: Rc::new(RefCell::new(SymbolTable::new())),
      analyzer: empty_analyzer_output(),
      diagnostics,
      has_errors: true,
    };
  }

  // Parser phase
  let symbol_table = Rc::new(RefCell::new(SymbolTable::new()));
  let mut parser = IgnisParser::new(lexer.tokens, symbol_table.clone());

  let (nodes, roots) = match parser.parse() {
    Ok((nodes, roots)) => (nodes, roots),
    Err(parse_errors) => {
      for diag_msg in &parse_errors {
        diagnostics.push(diag_msg.report());
      }

      return AnalyzeTextOutput {
        source_map,
        file_id,
        tokens,
        nodes: Store::new(),
        roots: Vec::new(),
        symbol_table,
        analyzer: empty_analyzer_output(),
        diagnostics,
        has_errors: true,
      };
    },
  };

  // Analyzer phase
  let analyzer = ignis_analyzer::Analyzer::analyze(&nodes, &roots, symbol_table.clone());

  diagnostics.extend(analyzer.diagnostics.clone());

  let has_errors = diagnostics
    .iter()
    .any(|d| matches!(d.severity, ignis_diagnostics::diagnostic_report::Severity::Error));

  AnalyzeTextOutput {
    source_map,
    file_id,
    tokens,
    nodes,
    roots,
    symbol_table,
    analyzer,
    diagnostics,
    has_errors,
  }
}

/// Create an empty AnalyzerOutput for early-exit cases.
fn empty_analyzer_output() -> ignis_analyzer::AnalyzerOutput {
  ignis_analyzer::AnalyzerOutput {
    types: ignis_type::types::TypeStore::new(),
    defs: ignis_type::definition::DefinitionStore::new(),
    namespaces: ignis_type::namespace::NamespaceStore::new(),
    hir: ignis_hir::HIR::new(),
    diagnostics: Vec::new(),
    symbols: Rc::new(RefCell::new(SymbolTable::new())),
    node_defs: HashMap::new(),
    node_types: HashMap::new(),
    node_spans: HashMap::new(),
    resolved_calls: HashMap::new(),
    import_item_defs: HashMap::new(),
  }
}

/// Output of `analyze_project`, containing diagnostics from multi-file analysis.
///
/// This struct contains only `Send`-safe types, suitable for async contexts.
/// It includes the source map so diagnostics can be rendered with proper locations.
pub struct AnalyzeProjectOutput {
  /// The source map containing all analyzed files.
  pub source_map: SourceMap,

  /// All accumulated diagnostics from all modules.
  pub diagnostics: Vec<Diagnostic>,

  /// True if any diagnostic is an error.
  pub has_errors: bool,

  /// All definitions from the analysis.
  /// Used for document symbols, go-to-definition, etc.
  pub defs: DefinitionStore,

  /// All types from the analysis.
  /// Used for hover information.
  pub types: TypeStore,

  /// Maps AST nodes to their resolved definitions.
  /// Used for go-to-definition from usage sites.
  pub node_defs: HashMap<NodeId, DefinitionId>,

  /// Maps AST nodes to their inferred types.
  /// Used for hover information.
  pub node_types: HashMap<NodeId, TypeId>,

  /// Maps AST nodes to their source spans.
  /// Used for finding which node is at a given cursor position.
  pub node_spans: HashMap<NodeId, Span>,

  /// Symbol names (SymbolId -> String).
  /// Extracted from SymbolTable for Send safety.
  pub symbol_names: HashMap<SymbolId, String>,

  /// Maps Call nodes to their resolved overload.
  /// Used when hovering over an overloaded function call.
  pub resolved_calls: HashMap<NodeId, DefinitionId>,

  /// Maps import item spans to their resolved definitions.
  /// Used for hover on import statements.
  pub import_item_defs: HashMap<Span, DefinitionId>,

  /// AST nodes for the entry file (root module).
  /// Used for inlay hints which need to traverse the AST.
  pub nodes: Store<ASTNode>,

  /// Root node IDs in the AST for the entry file.
  pub roots: Vec<NodeId>,
}

/// Analyze a project starting from an entry file, resolving imports.
///
/// This function:
/// - Discovers all modules starting from `entry_path`
/// - Resolves imports (including std library)
/// - Analyzes all modules in topological order
/// - Collects ALL diagnostics (does not fail early on errors)
///
/// # Arguments
/// * `config` - Compiler configuration (must have valid `std_path`)
/// * `entry_path` - Absolute path to the entry file
///
/// # Returns
/// `AnalyzeProjectOutput` with all diagnostics. Check `has_errors` to see if
/// compilation would fail.
pub fn analyze_project(
  config: &IgnisConfig,
  entry_path: &str,
) -> AnalyzeProjectOutput {
  analyze_project_with_text(config, entry_path, None)
}

/// Analyze a project with optional in-memory text for the entry file.
///
/// Same as `analyze_project`, but allows passing the current file's text
/// instead of reading from disk. This is useful for LSP where the editor
/// has unsaved changes.
///
/// # Arguments
/// * `config` - Compiler configuration
/// * `entry_path` - Absolute path to the entry file
/// * `entry_text` - Optional in-memory text for the entry file
pub fn analyze_project_with_text(
  config: &IgnisConfig,
  entry_path: &str,
  entry_text: Option<String>,
) -> AnalyzeProjectOutput {
  let mut ctx = CompilationContext::new(config);
  let mut all_diagnostics: Vec<Diagnostic> = Vec::new();

  // If we have in-memory text, preload it so we don't read from disk
  if let Some(text) = entry_text {
    ctx.preload_file(std::path::PathBuf::from(entry_path), text);
  }

  // Discover all modules using LSP mode (collects diagnostics, doesn't fail early)
  let root_id = match ctx.discover_modules_lsp(entry_path, config) {
    Ok(id) => Some(id),
    Err(()) => {
      // Entry file couldn't be read at all
      None
    },
  };

  // Collect discovery diagnostics (lex/parse errors)
  all_diagnostics.extend(std::mem::take(&mut ctx.discovery_diagnostics));

  // If we have discovery errors, we may not have a valid module graph
  // but we still want to return the diagnostics we collected
  let has_discovery_errors = all_diagnostics
    .iter()
    .any(|d| matches!(d.severity, ignis_diagnostics::diagnostic_report::Severity::Error));

  // If we couldn't discover the root module, return what we have
  let Some(root_id) = root_id else {
    let symbol_names = extract_symbol_names(&ctx.symbol_table);
    return AnalyzeProjectOutput {
      source_map: ctx.source_map,
      diagnostics: all_diagnostics,
      has_errors: true,
      defs: DefinitionStore::new(),
      types: TypeStore::new(),
      node_defs: HashMap::new(),
      node_types: HashMap::new(),
      node_spans: HashMap::new(),
      symbol_names,
      resolved_calls: HashMap::new(),
      import_item_defs: HashMap::new(),
      nodes: Store::new(),
      roots: Vec::new(),
    };
  };

  // Check for cycles (but don't fail - just note the error)
  let has_cycle = ctx.module_graph.detect_cycles().is_err();

  ctx.module_graph.root = Some(root_id);
  let order = ctx.module_graph.topological_sort();

  // Only run analyzer if we have modules to analyze and no critical errors
  if !order.is_empty() && !has_cycle {
    let (output, analyzer_has_errors) = ctx.analyze_modules_collect_all(&order, config, false);
    all_diagnostics.extend(output.diagnostics);

    let has_errors = has_discovery_errors || analyzer_has_errors;
    let symbol_names = extract_symbol_names(&output.symbols);

    // Extract AST nodes and roots from the root module for inlay hints
    let (nodes, roots) = ctx
      .parsed_modules
      .remove(&root_id)
      .map(|pm| (pm.nodes, pm.roots))
      .unwrap_or_else(|| (Store::new(), Vec::new()));

    return AnalyzeProjectOutput {
      source_map: ctx.source_map,
      diagnostics: all_diagnostics,
      has_errors,
      defs: output.defs,
      types: output.types,
      node_defs: output.node_defs,
      node_types: output.node_types,
      node_spans: output.node_spans,
      symbol_names,
      resolved_calls: output.resolved_calls,
      import_item_defs: output.import_item_defs,
      nodes,
      roots,
    };
  }

  let symbol_names = extract_symbol_names(&ctx.symbol_table);

  // Extract AST nodes and roots from the root module if available
  let (nodes, roots) = ctx
    .parsed_modules
    .remove(&root_id)
    .map(|pm| (pm.nodes, pm.roots))
    .unwrap_or_else(|| (Store::new(), Vec::new()));

  AnalyzeProjectOutput {
    source_map: ctx.source_map,
    diagnostics: all_diagnostics,
    has_errors: has_discovery_errors || has_cycle,
    defs: DefinitionStore::new(),
    types: TypeStore::new(),
    node_defs: HashMap::new(),
    node_types: HashMap::new(),
    node_spans: HashMap::new(),
    symbol_names,
    resolved_calls: HashMap::new(),
    import_item_defs: HashMap::new(),
    nodes,
    roots,
  }
}

/// Extract symbol names from a SymbolTable into a Send-safe HashMap.
fn extract_symbol_names(symbols: &Rc<RefCell<SymbolTable>>) -> HashMap<SymbolId, String> {
  let table = symbols.borrow();
  let mut names = HashMap::new();

  for (name, id) in table.map.iter() {
    names.insert(*id, name.clone());
  }

  names
}
