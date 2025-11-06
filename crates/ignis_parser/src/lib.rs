mod lexer;
mod parser;

use ignis_config::IgnisConfig;
use ignis_type::file::SourceMap;
use std::sync::Arc;
use colored::*;

use crate::lexer::IgnisLexer;

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
    }
  };

  if !config.quiet {
    println!(
      "{:indent$}Scanning... {}",
      "-->".bright_green().bold(),
      file_path,
      indent = 4
    );
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
}
