use std::collections::HashMap;
use analyzer::Analyzer;
use diagnostic_report::DiagnosticReport;
use intermediate_representation::IRInstruction;
use lexer::Lexer;
use parser::Parser;
use ast::Ast;

#[derive(Debug, PartialEq)]
pub enum FrontendDebugPrint {
  Lexer,
  Ast,
  IR,
}

pub struct IgnisFrontend {
  source: String,
  path: String,
  debug: Vec<FrontendDebugPrint>,
}

impl IgnisFrontend {
  pub fn new(source: String, path: String, debug: Vec<FrontendDebugPrint>) -> Self {
    Self {
      source,
      path,
      debug,
    }
  }

  pub fn process(&mut self) -> Result<HashMap<String, Vec<IRInstruction>>, Vec<DiagnosticReport>> {
    let mut lexer = Lexer::new(&self.source, self.path.clone());
    lexer.scan_tokens();
    let tokens = lexer.tokens;

    let mut parser = Parser::new(tokens.clone());
    let statements = Ast::new(parser.parse()?);

    if self.debug.contains(&FrontendDebugPrint::Ast) {
      let pretty_string = serde_json::to_string_pretty(&statements.to_json()).unwrap();
      println!("{}", pretty_string);
    }

    let mut analyzer = Analyzer::new(self.path.clone(), tokens);
    analyzer.analyze(&statements.statements)?;

    if self.debug.contains(&FrontendDebugPrint::IR) {
      for (name, ir) in &analyzer.irs {
        let pretty_string = serde_json::to_string_pretty(
          &ir
            .iter()
            .map(|i| i.to_json())
            .collect::<serde_json::Value>(),
        )
        .unwrap();
        println!("{}:\n{}", name, pretty_string);
      }
    }

    Ok(analyzer.irs)
  }
}
