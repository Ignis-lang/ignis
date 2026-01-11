use std::cell::RefCell;
use std::rc::Rc;

use ignis_analyzer::{Analyzer, AnalyzerOutput, HirOwnershipChecker};
use ignis_diagnostics::diagnostic_report::{Diagnostic, Severity};
use ignis_hir::display::print_hir;
use ignis_parser::{IgnisLexer, IgnisParser};
use ignis_type::{file::SourceMap, symbol::SymbolTable};

pub struct AnalysisResult {
  pub output: AnalyzerOutput,
  pub source_map: SourceMap,
}

/// Run the full pipeline: lexer -> parser -> analyzer -> ownership check
pub fn analyze(src: &str) -> AnalysisResult {
  let mut sm = SourceMap::new();
  let file_id = sm.add_file("test.ign", src.to_string());

  let mut lexer = IgnisLexer::new(file_id.clone(), sm.get(&file_id).text.as_str());
  lexer.scan_tokens();
  assert!(lexer.diagnostics.is_empty(), "Lexer errors: {:?}", lexer.diagnostics);

  let symbols = Rc::new(RefCell::new(SymbolTable::new()));
  let mut parser = IgnisParser::new(lexer.tokens, symbols.clone());
  let (nodes, roots) = parser.parse().expect("Parse failed");

  let mut output = Analyzer::analyze(&nodes, &roots, symbols.clone());

  let symbols_ref = symbols.borrow();
  let checker = HirOwnershipChecker::new(&output.hir, &output.types, &output.defs, &symbols_ref);
  let (_, ownership_diags) = checker.check();
  output.diagnostics.extend(ownership_diags);

  AnalysisResult { output, source_map: sm }
}

/// Run the full pipeline, allowing parse errors (for error case testing)
pub fn analyze_with_errors(src: &str) -> Option<AnalysisResult> {
  let mut sm = SourceMap::new();
  let file_id = sm.add_file("test.ign", src.to_string());

  let mut lexer = IgnisLexer::new(file_id.clone(), sm.get(&file_id).text.as_str());
  lexer.scan_tokens();

  if !lexer.diagnostics.is_empty() {
    return None;
  }

  let symbols = Rc::new(RefCell::new(SymbolTable::new()));
  let mut parser = IgnisParser::new(lexer.tokens, symbols.clone());

  match parser.parse() {
    Ok((nodes, roots)) => {
      let mut output = Analyzer::analyze(&nodes, &roots, symbols.clone());

      let symbols_ref = symbols.borrow();
      let checker = HirOwnershipChecker::new(&output.hir, &output.types, &output.defs, &symbols_ref);
      let (_, ownership_diags) = checker.check();
      output.diagnostics.extend(ownership_diags);

      Some(AnalysisResult { output, source_map: sm })
    },
    Err(_) => None,
  }
}

/// Assert source analyzes without errors
#[allow(dead_code)]
pub fn assert_ok(src: &str) {
  let result = analyze(src);
  let errors: Vec<_> = result
    .output
    .diagnostics
    .iter()
    .filter(|d| matches!(d.severity, Severity::Error))
    .collect();
  assert!(errors.is_empty(), "Expected no errors, got: {:?}", errors);
}

/// Assert source produces specific error codes
#[allow(dead_code)]
pub fn assert_err(
  src: &str,
  expected_codes: &[&str],
) {
  let result = analyze(src);
  let actual_codes: Vec<String> = result.output.diagnostics.iter().map(|d| d.error_code.clone()).collect();

  for code in expected_codes {
    assert!(
      actual_codes.contains(&code.to_string()),
      "Expected error code {} not found. Got: {:?}",
      code,
      actual_codes
    );
  }
}

/// Format diagnostics for stable snapshot comparison (sorted by span)
pub fn format_diagnostics(diags: &[Diagnostic]) -> String {
  let mut sorted = diags.to_vec();
  sorted.sort_by(|a, b| {
    a.primary_span
      .start
      .cmp(&b.primary_span.start)
      .then_with(|| a.error_code.cmp(&b.error_code))
  });

  if sorted.is_empty() {
    return "(no diagnostics)".to_string();
  }

  let mut output = String::new();
  for diag in &sorted {
    let severity = match diag.severity {
      Severity::Error => "ERROR",
      Severity::Warning => "WARN",
      Severity::Info => "INFO",
      Severity::Hint => "HINT",
    };
    output.push_str(&format!(
      "[{}] {}: {}\n  at {:?}:{}-{}\n",
      severity,
      diag.error_code,
      diag.message,
      diag.primary_span.file,
      diag.primary_span.start.0,
      diag.primary_span.end.0,
    ));
    for label in &diag.labels {
      output.push_str(&format!(
        "  label: {} at {:?}:{}-{}\n",
        label.message, label.span.file, label.span.start.0, label.span.end.0,
      ));
    }
    for note in &diag.notes {
      output.push_str(&format!("  note: {}\n", note));
    }
  }
  output
}

/// Format HIR for stable snapshot comparison (uses sorted iteration)
pub fn format_hir(result: &AnalysisResult) -> String {
  let symbols = result.output.symbols.borrow();
  print_hir(&result.output.hir, &result.output.types, &result.output.defs, &symbols)
}

/// Get line number for a diagnostic (1-indexed)
#[allow(dead_code)]
pub fn diagnostic_line(
  result: &AnalysisResult,
  diag: &Diagnostic,
) -> u32 {
  let (line, _col) = result
    .source_map
    .line_col(&diag.primary_span.file, diag.primary_span.start);
  line
}

/// Assert a specific error code appears at a specific line
#[allow(dead_code)]
pub fn assert_diagnostic_at_line(
  src: &str,
  code: &str,
  expected_line: u32,
) {
  let result = analyze(src);

  let matching = result.output.diagnostics.iter().find(|d| d.error_code == code);

  assert!(matching.is_some(), "Expected error {} not found", code);

  let diag = matching.unwrap();
  let actual_line = diagnostic_line(&result, diag);

  assert_eq!(
    actual_line, expected_line,
    "Error {} at wrong line: expected {}, got {}",
    code, expected_line, actual_line
  );
}
