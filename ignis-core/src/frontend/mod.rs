pub mod lexer;
mod meta;
pub mod parser;

use std::io::Read;

use ignis_config::{DebugPrint, IgnisConfig};
use ignis_token::token::Token;
use lexer::Lexer;

use crate::diagnostics::{diagnostic_report::DiagnosticReport, Diagnostic};

fn get_file_content(path: &String) -> String {
  let file = std::fs::File::open(path);

  if file.is_err() {
    return String::new();
  }

  let mut file = file.unwrap();
  let mut content = String::new();

  let _ = file.read_to_string(&mut content);

  return content;
}

pub struct IgnisFrontend {
  config: Box<IgnisConfig>,
  diagnostics: Vec<DiagnosticReport>,
}

impl IgnisFrontend {
  pub fn new(config: Box<IgnisConfig>) -> Self {
    Self {
      config,
      diagnostics: Vec::new(),
    }
  }

  pub fn process(&mut self) {
    let build_config = self.config.build_config.clone().unwrap();

    let file = build_config.file.unwrap();

    let source = get_file_content(&file);

    let mut lexer = Lexer::new(self.config.clone(), source.as_str(), file);
    lexer.scan_tokens(false);

    for diagnostic in lexer.diagnostics {
      self.diagnostics.push(diagnostic.report());
    }

    let tokens = lexer.tokens;

    if self.config.debug.contains(&DebugPrint::Lexer) && !self.config.quiet {
      if self.config.verbose == 0 {
        Token::print_ascii_table(&tokens);
      } else {
        println!("{:#?}", tokens);
      }
    }

    let mut parser = parser::IgnisParser::new(self.config.clone(), tokens);

    let (statements, diagnostics) = parser.parse(false);

    if self.config.debug.contains(&DebugPrint::Ast) && !self.config.quiet {
      println!("Pre-Meta AST: {:#?}", statements);
    }

    for diagnostic in diagnostics {
      self.diagnostics.push(diagnostic.report());
    }

    let mut meta_processor = meta::IgnisMetaProcessor::new(statements);
    meta_processor.process();

    if self.config.debug.contains(&DebugPrint::Ast) && !self.config.quiet {
      println!("Post-Meta AST: {:#?}", meta_processor.new_ast);
    }

    for diagnostic in meta_processor.diagnostics {
      self.diagnostics.push(diagnostic.report_diagnostic());
    }

    if !self.config.quiet {
      let diagnostics: Diagnostic = Diagnostic::default();

      diagnostics.report(self.diagnostics.clone());
      std::process::exit(1);
    }
  }
}
