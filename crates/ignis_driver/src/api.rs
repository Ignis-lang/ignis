//! Pure analysis API for reuse by CLI and LSP.
//!
//! This module provides side-effect-free functions that:
//! - Do NOT print to stdout/stderr
//! - Do NOT render diagnostics
//! - Do NOT write dump files
//! - Only produce data structures

use std::cell::RefCell;
use std::rc::Rc;

use ignis_ast::{ASTNode, NodeId};
use ignis_diagnostics::diagnostic_report::Diagnostic;
use ignis_parser::{IgnisLexer, IgnisParser};
use ignis_token::token::Token;
use ignis_type::file::{FileId, SourceMap};
use ignis_type::symbol::SymbolTable;
use ignis_type::Store;

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
  }
}
