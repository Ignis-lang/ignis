mod lexer;
mod parser;

use ignis_config::IgnisConfig;
use ignis_type::{file::SourceMap, symbol::SymbolTable};
use std::{cell::RefCell, rc::Rc, sync::Arc};
use colored::*;

use crate::lexer::IgnisLexer;
use ignis_ast::display::format_ast_nodes;

pub fn compile_file(
  config: Arc<IgnisConfig>,
  file_path: &str,
) {
  let mut sm = SourceMap::new();

  let text = match std::fs::read_to_string(file_path) {
    Ok(content) => content,
    Err(e) => {
      eprintln!("{} Failed to read file '{}': {}", "Error:".red().bold(), file_path, e);
      std::process::exit(1);
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
    std::process::exit(1);
  }

  for token in &lexer.tokens {
    println!("{}", token);
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
    std::process::exit(1);
  }

  let (nodes, roots) = parse_result.unwrap();
  
  println!("\n{}", "AST:".bright_cyan().bold());
  let ast_lisp = format_ast_nodes(nodes, symbol_table, &roots);
  println!("{}", ast_lisp);
}
