pub mod analyzer;
pub mod lexer;
pub mod parser;

use std::io::Read;

use ignis_config::{DebugPrint, IgnisConfig};
use ignis_token::token::Token;
use lexer::IgnisLexer;

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

pub struct IgnisFrontend<'a> {
  config: &'a IgnisConfig,
  diagnostics: Vec<DiagnosticReport>,
}

impl<'a> IgnisFrontend<'a> {
  pub fn new(config: &'a IgnisConfig) -> Self {
    Self {
      config,
      diagnostics: Vec::new(),
    }
  }

  pub fn process(&mut self) {
    let build_config = self.config.build_config.clone().unwrap();

    let file = build_config.file.unwrap();

    let source = get_file_content(&file);

    let mut lexer = IgnisLexer::new(&self.config, source.as_str(), &file);
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

    let mut parser = parser::IgnisParser::new(&self.config, &tokens);

    let (statements, diagnostics) = parser.parse(false);

    self.diagnostics.append(&mut diagnostics.clone());

    let std = analyzer::IgnisAnalyzer::load_primitive_std(self.config.clone().into());

    let mut analyzer = analyzer::IgnisAnalyzer::new(
      self.config.clone().into(),
      file,
      statements,
      std.0.clone().into(),
      std.1.clone().into(),
    );
    let analyzer_result = analyzer.process(false);

    if analyzer_result.is_err() {
      self.diagnostics.append(&mut analyzer_result.unwrap_err());
    }

    if self.config.debug.contains(&DebugPrint::Hir) && !self.config.quiet {
      println!("HIR: {:#?}", analyzer.get_hir());
    }

    if !self.config.quiet {
      let diagnostics: Diagnostic = Diagnostic::default();

      diagnostics.report(self.diagnostics.clone());
      std::process::exit(1);
    }
  }
}
