mod lexer;
mod parser;

pub use lexer::IgnisLexer;
pub use parser::IgnisParser;

use ignis_config::IgnisConfig;
use ignis_type::{file::SourceMap, symbol::SymbolTable};
use std::{cell::RefCell, rc::Rc, sync::Arc};
use colored::*;

use ignis_ast::display::format_ast_nodes;

pub fn compile_file(
  config: Arc<IgnisConfig>,
  file_path: &str,
) -> Result<(), ()> {
  let mut sm = SourceMap::new();

  let text = match std::fs::read_to_string(file_path) {
    Ok(content) => content,
    Err(e) => {
      eprintln!("{} Failed to read file '{}': {}", "Error:".red().bold(), file_path, e);
      return Err(());
    },
  };

  if !config.quiet {
    println!("{:indent$}Scanning... {}", "-->".bright_green().bold(), file_path, indent = 4);
  }

  let file_id = sm.add_file(file_path, text);
  let src = &sm.get(&file_id).text;

  let mut lexer = IgnisLexer::new(file_id, src);
  lexer.scan_tokens();

  if !lexer.diagnostics.is_empty() {
    for diag_msg in &lexer.diagnostics {
      let diag = diag_msg.report();
      ignis_diagnostics::render(&diag, &sm);
    }
    return Err(());
  }

  if config.debug.contains(&ignis_config::DebugPrint::Lexer) {
    println!("\n{}", "Tokens:".bright_cyan().bold());
    for token in &lexer.tokens {
      println!("{}", token);
    }
  }

  let symbol_table = Rc::new(RefCell::new(SymbolTable::new()));

  let mut parser = parser::IgnisParser::new(lexer.tokens, symbol_table.clone());
  let parse_result = parser.parse();
  if parse_result.is_err() {
    let err = parse_result.unwrap_err();
    for diag_msg in &err {
      let diag = diag_msg.report();
      ignis_diagnostics::render(&diag, &sm);
    }
    return Err(());
  }

  let (nodes, roots) = parse_result.unwrap();

  if config.debug.contains(&ignis_config::DebugPrint::Ast) {
    println!("\n{}", "AST:".bright_cyan().bold());
    let ast_lisp = format_ast_nodes(nodes.clone(), symbol_table.clone(), &roots);
    println!("{}", ast_lisp);
  }

  // ALWAYS run the analyzer (unless --quiet is set)
  if !config.quiet {
    println!("\n{}", "Running analyzer...".bright_cyan().bold());
  }

  let analyzer_result = ignis_analyzer::Analyzer::analyze(&nodes, &roots, symbol_table);

  // ALWAYS show diagnostics (errors/warnings) unless --quiet
  if !config.quiet {
    for diag in &analyzer_result.diagnostics {
      ignis_diagnostics::render(diag, &sm);
    }
  }

  // Return error if there are any errors
  let has_errors = analyzer_result
    .diagnostics
    .iter()
    .any(|d| matches!(d.severity, ignis_diagnostics::diagnostic_report::Severity::Error));

  if has_errors {
    return Err(());
  }

  let build_config = config.build_config.as_ref();

  // Handle dump flags (optional debug output)
  let sym_table = analyzer_result.symbols.borrow();
  if let Some(bc) = build_config {
    if bc.dump_types {
      println!("\n{}", ignis_analyzer::dump::dump_types(&analyzer_result.types));
    }
    if bc.dump_defs {
      println!(
        "\n{}",
        ignis_analyzer::dump::dump_defs(&analyzer_result.defs, &analyzer_result.types, &sym_table)
      );
    }
    if bc.dump_hir_summary {
      println!(
        "\n{}",
        ignis_analyzer::dump::dump_hir_summary(&analyzer_result.hir, &analyzer_result.defs, &sym_table)
      );
    }
    if let Some(func_name) = &bc.dump_hir {
      match ignis_analyzer::dump::dump_hir_function(&analyzer_result.hir, &analyzer_result.defs, &sym_table, func_name)
      {
        Ok(output) => println!("\n{}", output),
        Err(err) => eprintln!("{} {}", "Error:".red().bold(), err),
      }
    }
  }

  // Debug flags for extra information
  if config.debug.contains(&ignis_config::DebugPrint::Analyzer) {
    println!("\n{}", "Type Store & Definitions:".bright_cyan().bold());
    println!("{}", ignis_analyzer::dump::dump_types(&analyzer_result.types));
    println!(
      "{}",
      ignis_analyzer::dump::dump_defs(&analyzer_result.defs, &analyzer_result.types, &sym_table)
    );
  }

  if config.debug.contains(&ignis_config::DebugPrint::Hir) {
    println!("\n{}", "HIR:".bright_cyan().bold());
    println!(
      "{}",
      ignis_analyzer::dump::dump_hir_complete(
        &analyzer_result.hir,
        &analyzer_result.types,
        &analyzer_result.defs,
        &sym_table
      )
    );
  }

  Ok(())
}
