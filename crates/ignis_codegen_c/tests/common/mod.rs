use std::cell::RefCell;
use std::rc::Rc;

use ignis_analyzer::Analyzer;
use ignis_parser::{IgnisLexer, IgnisParser};
use ignis_type::file::SourceMap;
use ignis_type::symbol::SymbolTable;

/// Compile Ignis source code to C.
pub fn compile_to_c(source: &str) -> String {
  let mut sm = SourceMap::new();
  let file_id = sm.add_file("test.ign", source.to_string());
  let src = &sm.get(&file_id).text;

  let mut lexer = IgnisLexer::new(file_id, src);
  lexer.scan_tokens();
  assert!(lexer.diagnostics.is_empty(), "Lexer errors: {:?}", lexer.diagnostics);

  let symbol_table = Rc::new(RefCell::new(SymbolTable::new()));
  let mut parser = IgnisParser::new(lexer.tokens, symbol_table.clone());
  let (nodes, roots) = parser.parse().expect("Parse failed");

  let result = Analyzer::analyze(&nodes, &roots, symbol_table);
  let has_errors = result
    .diagnostics
    .iter()
    .any(|d| matches!(d.severity, ignis_diagnostics::diagnostic_report::Severity::Error));
  assert!(!has_errors, "Analyzer errors: {:?}", result.diagnostics);

  let mut types = result.types.clone();
  let sym_table = result.symbols.borrow();
  let (lir_program, verify_result) =
    ignis_lir::lowering::lower_and_verify(&result.hir, &mut types, &result.defs, &sym_table);

  if let Err(errors) = &verify_result {
    panic!("LIR verification errors: {:?}", errors);
  }

  ignis_codegen_c::emit_c(&lir_program, &types, &result.defs, &sym_table)
}
