#![allow(dead_code)]

use std::cell::RefCell;
use std::rc::Rc;

use ignis_analyzer::{Analyzer, AnalyzerOutput};
use ignis_lir::{LirProgram, display::print_lir, lowering::lower_and_verify, verify::VerifyError};
use ignis_parser::{IgnisLexer, IgnisParser};
use ignis_type::{file::SourceMap, symbol::SymbolTable, types::TypeStore};

pub struct LirResult {
  pub program: LirProgram,
  pub verify_errors: Vec<VerifyError>,
  pub types: TypeStore,
  pub analyzer_output: AnalyzerOutput,
  pub source_map: SourceMap,
}

/// Run the full pipeline: lexer -> parser -> analyzer -> LIR lowering
pub fn lower_to_lir(src: &str) -> LirResult {
  let mut sm = SourceMap::new();
  let file_id = sm.add_file("test.ign", src.to_string());

  let mut lexer = IgnisLexer::new(file_id.clone(), sm.get(&file_id).text.as_str());
  lexer.scan_tokens();
  assert!(lexer.diagnostics.is_empty(), "Lexer errors: {:?}", lexer.diagnostics);

  let symbols = Rc::new(RefCell::new(SymbolTable::new()));
  let mut parser = IgnisParser::new(lexer.tokens, symbols.clone());
  let (nodes, roots) = parser.parse().expect("Parse failed");

  let output = Analyzer::analyze(&nodes, &roots, symbols);

  // Check for analyzer errors
  let has_errors = output
    .diagnostics
    .iter()
    .any(|d| matches!(d.severity, ignis_diagnostics::diagnostic_report::Severity::Error));
  assert!(!has_errors, "Analyzer errors: {:?}", output.diagnostics);

  let mut types = output.types.clone();

  // Run ownership analysis to produce drop schedules
  let (program, verify_result) = {
    let symbols = output.symbols.borrow();
    let ownership_checker =
      ignis_analyzer::HirOwnershipChecker::new(&output.hir, &output.types, &output.defs, &symbols);
    let (drop_schedules, _) = ownership_checker.check();
    lower_and_verify(&output.hir, &mut types, &output.defs, &symbols, &drop_schedules, None)
  };

  let verify_errors = match verify_result {
    Ok(()) => Vec::new(),
    Err(errs) => errs,
  };

  LirResult {
    program,
    verify_errors,
    types,
    analyzer_output: output,
    source_map: sm,
  }
}

/// Format LIR for stable snapshot comparison
pub fn format_lir(result: &LirResult) -> String {
  let symbols = result.analyzer_output.symbols.borrow();
  print_lir(&result.program, &result.types, &result.analyzer_output.defs, &symbols)
}

/// Assert LIR verifies without errors
#[allow(dead_code)]
pub fn assert_verifies(src: &str) {
  let result = lower_to_lir(src);
  assert!(
    result.verify_errors.is_empty(),
    "Expected no verification errors, got: {:?}",
    result.verify_errors
  );
}
