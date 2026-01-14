use std::cell::RefCell;
use std::path::PathBuf;
use std::process::Command;
use std::rc::Rc;

use ignis_analyzer::Analyzer;
use ignis_config::CHeader;
use ignis_parser::{IgnisLexer, IgnisParser};
use ignis_type::definition::{DefinitionId, DefinitionKind, DefinitionStore, Visibility};
use ignis_type::file::SourceMap;
use ignis_type::symbol::SymbolTable;
use tempfile::TempDir;

pub struct E2EResult {
  pub exit_code: i32,
  pub stdout: String,
  pub stderr: String,
}

fn compile_to_c(source: &str) -> Result<String, String> {
  let mut sm = SourceMap::new();
  let file_id = sm.add_file("test.ign", source.to_string());
  let src = &sm.get(&file_id).text;

  let mut lexer = IgnisLexer::new(file_id, src);
  lexer.scan_tokens();
  if !lexer.diagnostics.is_empty() {
    return Err(format!("Lexer errors: {:?}", lexer.diagnostics));
  }

  let symbol_table = Rc::new(RefCell::new(SymbolTable::new()));
  let mut parser = IgnisParser::new(lexer.tokens, symbol_table.clone());
  let (nodes, roots) = parser.parse().map_err(|e| format!("Parse errors: {:?}", e))?;

  let result = Analyzer::analyze(&nodes, &roots, symbol_table);
  let has_errors = result
    .diagnostics
    .iter()
    .any(|d| matches!(d.severity, ignis_diagnostics::diagnostic_report::Severity::Error));
  if has_errors {
    return Err(format!("Analyzer errors: {:?}", result.diagnostics));
  }

  let mut types = result.types.clone();

  // Run monomorphization: transform generic HIR into concrete HIR
  // Note: We need to drop the sym_table borrow before running monomorphization
  // because the monomorphizer needs to borrow_mut() to intern new names.
  let mono_roots = {
    let sym_table = result.symbols.borrow();
    collect_mono_roots(&result.defs, &sym_table)
  };
  let mono_output =
    ignis_analyzer::mono::Monomorphizer::new(&result.hir, &result.defs, &mut types, result.symbols.clone())
      .run(&mono_roots);

  // Re-borrow for downstream passes
  let sym_table = result.symbols.borrow();

  // Run ownership analysis to produce drop schedules
  let ownership_checker =
    ignis_analyzer::HirOwnershipChecker::new(&mono_output.hir, &types, &mono_output.defs, &sym_table);
  let (drop_schedules, _) = ownership_checker.check();

  let (lir, verify) = ignis_lir::lowering::lower_and_verify(
    &mono_output.hir,
    &mut types,
    &mono_output.defs,
    &sym_table,
    &drop_schedules,
    None,
  );
  if let Err(e) = verify {
    return Err(format!("LIR verification errors: {:?}", e));
  }

  let headers = vec![CHeader {
    path: "types.h".to_string(),
    quoted: true,
  }];

  Ok(ignis_codegen_c::emit_c(
    &lir,
    &types,
    &mono_output.defs,
    &result.namespaces,
    &sym_table,
    &headers,
  ))
}

pub fn compile_and_run(source: &str) -> Result<E2EResult, String> {
  let c_code = compile_to_c(source)?;

  let temp_dir = TempDir::new().map_err(|e| format!("Failed to create temp dir: {}", e))?;
  let c_path = temp_dir.path().join("test.c");
  let bin_path = temp_dir.path().join("test");
  let fixtures_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures");

  std::fs::write(&c_path, &c_code).map_err(|e| format!("Failed to write C file: {}", e))?;

  let compile = Command::new("gcc")
    .arg("-I")
    .arg(&fixtures_dir)
    .arg(&c_path)
    .arg("-o")
    .arg(&bin_path)
    .output()
    .map_err(|e| format!("Failed to run gcc: {}", e))?;

  if !compile.status.success() {
    return Err(format!(
      "gcc compilation failed:\nC code:\n{}\n\nstderr:\n{}",
      c_code,
      String::from_utf8_lossy(&compile.stderr)
    ));
  }

  let run = Command::new(&bin_path)
    .output()
    .map_err(|e| format!("Failed to run binary: {}", e))?;

  Ok(E2EResult {
    exit_code: run.status.code().unwrap_or(-1),
    stdout: String::from_utf8_lossy(&run.stdout).to_string(),
    stderr: String::from_utf8_lossy(&run.stderr).to_string(),
  })
}

pub fn format_e2e_result(result: &E2EResult) -> String {
  format!(
    "exit_code: {}\nstdout: {}\nstderr: {}",
    result.exit_code,
    if result.stdout.is_empty() {
      "(empty)"
    } else {
      result.stdout.trim_end()
    },
    if result.stderr.is_empty() {
      "(empty)"
    } else {
      result.stderr.trim_end()
    }
  )
}

/// Returns analyzer error messages, or Err if lex/parse fails.
pub fn compile_diagnostics(source: &str) -> Result<Vec<String>, String> {
  let mut sm = SourceMap::new();
  let file_id = sm.add_file("test.ign", source.to_string());
  let src = &sm.get(&file_id).text;

  let mut lexer = IgnisLexer::new(file_id, src);
  lexer.scan_tokens();
  if !lexer.diagnostics.is_empty() {
    return Err(format!("Lexer errors: {:?}", lexer.diagnostics));
  }

  let symbol_table = Rc::new(RefCell::new(SymbolTable::new()));
  let mut parser = IgnisParser::new(lexer.tokens, symbol_table.clone());
  let (nodes, roots) = parser.parse().map_err(|e| format!("Parse errors: {:?}", e))?;

  let result = Analyzer::analyze(&nodes, &roots, symbol_table);

  let messages: Vec<String> = result
    .diagnostics
    .iter()
    .filter(|d| matches!(d.severity, ignis_diagnostics::diagnostic_report::Severity::Error))
    .map(|d| d.message.clone())
    .collect();

  Ok(messages)
}

/// Collect root definitions for monomorphization.
fn collect_mono_roots(
  defs: &DefinitionStore,
  symbols: &SymbolTable,
) -> Vec<DefinitionId> {
  let mut roots = Vec::new();

  for (def_id, def) in defs.iter() {
    match &def.kind {
      // Include main function
      DefinitionKind::Function(fd) if !fd.is_extern => {
        let name = symbols.get(&def.name);
        if name == "main" {
          roots.push(def_id);
        }
      },

      // Include all public functions
      DefinitionKind::Function(fd) if def.visibility == Visibility::Public && !fd.is_extern => {
        roots.push(def_id);
      },

      // Include non-generic records and their methods
      DefinitionKind::Record(rd) if rd.type_params.is_empty() => {
        for method_id in rd.instance_methods.values() {
          roots.push(*method_id);
        }
        for method_id in rd.static_methods.values() {
          roots.push(*method_id);
        }
      },

      // Include non-generic enums and their methods
      DefinitionKind::Enum(ed) if ed.type_params.is_empty() => {
        for method_id in ed.static_methods.values() {
          roots.push(*method_id);
        }
      },

      _ => {},
    }
  }

  roots
}
