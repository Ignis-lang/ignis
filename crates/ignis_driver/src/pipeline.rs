use std::sync::Arc;
use std::{cell::RefCell, rc::Rc};

use colored::*;
use ignis_ast::display::format_ast_nodes;
use ignis_config::IgnisConfig;
use ignis_parser::{IgnisLexer, IgnisParser};
use ignis_type::file::SourceMap;
use ignis_type::symbol::SymbolTable;

use crate::context::CompilationContext;

/// Compile a single file (used for simple single-file compilation without imports)
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

  let mut parser = IgnisParser::new(lexer.tokens, symbol_table.clone());
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

  if !config.quiet {
    println!("\n{}", "Running analyzer...".bright_cyan().bold());
  }

  let analyzer_result = ignis_analyzer::Analyzer::analyze(&nodes, &roots, symbol_table);

  if !config.quiet {
    for diag in &analyzer_result.diagnostics {
      ignis_diagnostics::render(diag, &sm);
    }
  }

  let has_errors = analyzer_result
    .diagnostics
    .iter()
    .any(|d| matches!(d.severity, ignis_diagnostics::diagnostic_report::Severity::Error));

  if has_errors {
    return Err(());
  }

  let build_config = config.build_config.as_ref();

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

/// Compile a project with multi-module support
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
    if bc.dump_lir {
      let mut types = output.types.clone();
      let (lir_program, verify_result) =
        ignis_lir::lowering::lower_and_verify(&output.hir, &mut types, &output.defs, &sym_table);

      let lir_output = ignis_lir::display::print_lir(&lir_program, &types, &output.defs, &sym_table);
      println!("\n{}", lir_output);

      if let Err(errors) = verify_result {
        eprintln!("{} LIR verification errors:", "Warning:".yellow().bold());
        for err in errors {
          eprintln!("  {:?}", err);
        }
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
