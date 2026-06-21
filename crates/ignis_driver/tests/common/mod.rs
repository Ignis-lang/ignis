#![allow(dead_code)]

use std::cell::RefCell;
use std::path::Path;
use std::path::PathBuf;
use std::process::Command;
use std::rc::Rc;
use std::sync::Arc;

use ignis_analyzer::Analyzer;
use ignis_config::{CHeader, IgnisBuildConfig, IgnisConfig, IgnisSTDManifest, StdToolchainConfig, TargetBackend};
use ignis_driver::{AnalyzeProjectOutput, analyze_project_with_text, build_std, check_std, compile_project};
use ignis_parser::{IgnisLexer, IgnisParser};
use ignis_type::compilation_context::CompilationContext;
use ignis_type::definition::{DefinitionId, DefinitionKind, DefinitionStore, Visibility, SymbolEntry};
use ignis_type::file::SourceMap;
use ignis_type::symbol::SymbolTable;
use tempfile::TempDir;

/// LSan uses exit code 23 to signal detected memory leaks.
const LSAN_EXIT_CODE: i32 = 23;

pub struct E2EResult {
  pub exit_code: i32,
  pub stdout: String,
  pub stderr: String,
  pub leaked: bool,
  pub leak_report: String,
}

pub struct DriverBuildAttempt {
  pub _temp_dir: TempDir,
  pub bin_path: PathBuf,
  pub result: Result<(), ()>,
}

pub struct StdCommandAttempt {
  pub _temp_dir: TempDir,
  pub output_dir: PathBuf,
  pub result: Result<(), ()>,
}

fn build_driver_test_config(
  file_path: &std::path::Path,
  output_dir: &std::path::Path,
  target: TargetBackend,
) -> Arc<IgnisConfig> {
  let mut config = IgnisConfig::new_basic(false, Vec::new(), true, 0);
  let bin_path = output_dir.join(file_path.file_stem().and_then(|s| s.to_str()).unwrap_or("out"));
  let fixtures_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures");

  config.std = false;
  config.auto_load_std = false;
  config.std_path = fixtures_dir.to_string_lossy().to_string();
  config.manifest = IgnisSTDManifest {
    toolchain: StdToolchainConfig {
      base_header: Some("types.h".to_string()),
      base_header_quoted: Some(true),
      include_dirs: vec![".".to_string()],
    },
    modules: std::collections::HashMap::from([("__test_base".to_string(), "types.h".to_string())]),
    ..Default::default()
  };
  config.build = true;
  config.build_config = Some(IgnisBuildConfig::new(
    Some(file_path.to_string_lossy().to_string()),
    target,
    false,
    false,
    output_dir.to_string_lossy().to_string(),
    Vec::new(),
    None,
    None,
    None,
    None,
    Some(bin_path.to_string_lossy().to_string()),
    false,
    true,
    false,
    false,
    false,
  ));

  Arc::new(config)
}

fn build_workspace_std_driver_test_config(
  file_path: &std::path::Path,
  output_dir: &std::path::Path,
  target: TargetBackend,
) -> Result<Arc<IgnisConfig>, String> {
  let std_path = workspace_std_path();
  let manifest = load_std_manifest(&std_path)?;
  let mut config = IgnisConfig::new_basic(false, Vec::new(), true, 0);
  let bin_path = output_dir.join(file_path.file_stem().and_then(|s| s.to_str()).unwrap_or("out"));

  config.std_path = std_path.to_string_lossy().to_string();
  config.std = true;
  config.auto_load_std = true;
  config.manifest = manifest;
  config.build = true;
  config.build_config = Some(IgnisBuildConfig::new(
    Some(file_path.to_string_lossy().to_string()),
    target,
    false,
    false,
    output_dir.to_string_lossy().to_string(),
    Vec::new(),
    None,
    None,
    None,
    None,
    Some(bin_path.to_string_lossy().to_string()),
    false,
    true,
    false,
    false,
    false,
  ));

  Ok(Arc::new(config))
}

fn workspace_std_path() -> PathBuf {
  PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../std")
}

fn load_std_manifest(std_path: &Path) -> Result<IgnisSTDManifest, String> {
  let manifest_path = std_path.join("manifest.toml");
  let content = std::fs::read_to_string(&manifest_path)
    .map_err(|error| format!("Failed to read std manifest '{}': {}", manifest_path.display(), error))?;

  toml::from_str(&content)
    .map_err(|error| format!("Failed to parse std manifest '{}': {}", manifest_path.display(), error))
}

fn build_std_test_config(
  output_dir: &Path,
  target: TargetBackend,
) -> Result<Arc<IgnisConfig>, String> {
  let std_path = workspace_std_path();
  let manifest = load_std_manifest(&std_path)?;
  let mut config = IgnisConfig::new_basic(false, Vec::new(), true, 0);

  config.std_path = std_path.to_string_lossy().to_string();
  config.std = true;
  config.auto_load_std = true;
  config.manifest = manifest;
  config.build_std = true;
  config.check_std = true;
  config.build_config = Some(IgnisBuildConfig::new(
    None,
    target,
    false,
    false,
    output_dir.to_string_lossy().to_string(),
    Vec::new(),
    None,
    None,
    None,
    None,
    None,
    false,
    false,
    false,
    false,
    false,
  ));

  Ok(Arc::new(config))
}

pub fn compile_project_single_file(
  source: &str,
  target: TargetBackend,
) -> Result<DriverBuildAttempt, String> {
  let temp_dir = TempDir::new().map_err(|e| format!("Failed to create temp dir: {}", e))?;
  let source_path = temp_dir.path().join("main.ign");
  let output_dir = temp_dir.path().join("build");
  std::fs::create_dir_all(&output_dir).map_err(|e| format!("Failed to create build dir: {}", e))?;
  std::fs::write(&source_path, source).map_err(|e| format!("Failed to write source file: {}", e))?;

  let config = build_driver_test_config(&source_path, &output_dir, target);
  let bin_path = output_dir.join("main");
  let result = compile_project(config, source_path.to_string_lossy().as_ref());

  Ok(DriverBuildAttempt {
    _temp_dir: temp_dir,
    bin_path,
    result,
  })
}

pub fn build_std_with_target(target: TargetBackend) -> Result<StdCommandAttempt, String> {
  let temp_dir = TempDir::new().map_err(|e| format!("Failed to create temp dir: {}", e))?;
  let output_dir = temp_dir.path().join("build-std");
  std::fs::create_dir_all(&output_dir).map_err(|e| format!("Failed to create build dir: {}", e))?;

  let config = build_std_test_config(&output_dir, target)?;
  let result = build_std(config, output_dir.to_string_lossy().as_ref());

  Ok(StdCommandAttempt {
    _temp_dir: temp_dir,
    output_dir,
    result,
  })
}

pub fn compile_project_single_file_with_workspace_std(
  source: &str,
  target: TargetBackend,
) -> Result<DriverBuildAttempt, String> {
  let temp_dir = TempDir::new().map_err(|e| format!("Failed to create temp dir: {}", e))?;
  let source_path = temp_dir.path().join("main.ign");
  let output_dir = temp_dir.path().join("build");
  std::fs::create_dir_all(&output_dir).map_err(|e| format!("Failed to create build dir: {}", e))?;
  std::fs::write(&source_path, source).map_err(|e| format!("Failed to write source file: {}", e))?;

  let config = build_workspace_std_driver_test_config(&source_path, &output_dir, target)?;
  let bin_path = output_dir.join("main");
  let result = compile_project(config, source_path.to_string_lossy().as_ref());

  Ok(DriverBuildAttempt {
    _temp_dir: temp_dir,
    bin_path,
    result,
  })
}

pub fn analyze_project_single_file_with_workspace_std(source: &str) -> Result<AnalyzeProjectOutput, String> {
  let temp_dir = TempDir::new().map_err(|e| format!("Failed to create temp dir: {}", e))?;
  let source_path = temp_dir.path().join("main.ign");
  let output_dir = temp_dir.path().join("build");
  std::fs::create_dir_all(&output_dir).map_err(|e| format!("Failed to create build dir: {}", e))?;
  std::fs::write(&source_path, source).map_err(|e| format!("Failed to write source file: {}", e))?;

  let config = build_workspace_std_driver_test_config(&source_path, &output_dir, TargetBackend::C)?;
  let analysis = analyze_project_with_text(&config, source_path.to_string_lossy().as_ref(), Some(source.to_string()));

  Ok(analysis)
}

pub fn check_std_with_target(target: TargetBackend) -> Result<StdCommandAttempt, String> {
  let temp_dir = TempDir::new().map_err(|e| format!("Failed to create temp dir: {}", e))?;
  let output_dir = temp_dir.path().join("check-std");
  std::fs::create_dir_all(&output_dir).map_err(|e| format!("Failed to create build dir: {}", e))?;

  let config = build_std_test_config(&output_dir, target)?;
  let result = check_std(config, output_dir.to_string_lossy().as_ref());

  Ok(StdCommandAttempt {
    _temp_dir: temp_dir,
    output_dir,
    result,
  })
}

pub fn compile_project_and_run(source: &str) -> Result<E2EResult, String> {
  let attempt = compile_project_single_file(source, TargetBackend::C)?;
  attempt
    .result
    .map_err(|_| "compile_project failed for C backend".to_string())?;

  let run = Command::new(&attempt.bin_path)
    .output()
    .map_err(|e| format!("Failed to run compiled binary: {}", e))?;

  Ok(E2EResult {
    exit_code: run.status.code().unwrap_or(-1),
    stdout: String::from_utf8_lossy(&run.stdout).to_string(),
    stderr: String::from_utf8_lossy(&run.stderr).to_string(),
    leaked: false,
    leak_report: String::new(),
  })
}

pub fn compile_project_and_run_with_workspace_std(source: &str) -> Result<E2EResult, String> {
  let attempt = compile_project_single_file_with_workspace_std(source, TargetBackend::C)?;
  attempt
    .result
    .map_err(|_| "compile_project failed for C backend".to_string())?;

  let run = Command::new(&attempt.bin_path)
    .output()
    .map_err(|e| format!("Failed to run compiled binary: {}", e))?;

  Ok(E2EResult {
    exit_code: run.status.code().unwrap_or(-1),
    stdout: String::from_utf8_lossy(&run.stdout).to_string(),
    stderr: String::from_utf8_lossy(&run.stderr).to_string(),
    leaked: false,
    leak_report: String::new(),
  })
}

fn compile_to_c(source: &str) -> Result<String, String> {
  let source = source.to_string();

  let mut sm = SourceMap::new();
  let file_id = sm.add_file("test.ign", source);
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
  let mono_output = ignis_analyzer::mono::Monomorphizer::new(
    &result.hir,
    &result.defs,
    &result.namespaces,
    &mut types,
    result.symbols.clone(),
  )
  .run(&mono_roots);

  // Re-borrow for downstream passes
  let sym_table = result.symbols.borrow();

  // Run post-mono checks: ownership analysis + borrow checking
  let ownership_checker =
    ignis_analyzer::HirOwnershipChecker::new(&mono_output.hir, &types, &mono_output.defs, &sym_table);
  let (drop_schedules, _) = ownership_checker.check();

  let borrow_checker = ignis_analyzer::HirBorrowChecker::new(&mono_output.hir, &types, &mono_output.defs, &sym_table);
  let borrow_diagnostics = borrow_checker.check();
  if borrow_diagnostics
    .iter()
    .any(|d| matches!(d.severity, ignis_diagnostics::diagnostic_report::Severity::Error))
  {
    return Err(format!("Borrow check errors: {:?}", borrow_diagnostics));
  }

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

  Ok(ignis_codegen_c::emit_c_from_input(
    ignis_codegen_c::EmitInput::new(&lir, &types, &mono_output.defs).with_source_map(&sm),
    &result.namespaces,
    &sym_table,
    &headers,
  ))
}

/// Splits stderr into (user output, LSan report). LSan output starts with
/// a line matching `==<pid>==ERROR: LeakSanitizer:` and runs to the end.
fn split_lsan_output(stderr: &str) -> (String, String) {
  const LSAN_HEADER: &str = "ERROR: LeakSanitizer:";

  if !stderr.contains(LSAN_HEADER) {
    return (stderr.to_string(), String::new());
  }

  let mut user_lines = Vec::new();
  let mut leak_lines = Vec::new();
  let mut in_lsan = false;

  for line in stderr.lines() {
    if !in_lsan && line.starts_with("==") && line.contains(LSAN_HEADER) {
      in_lsan = true;
    }

    if in_lsan {
      leak_lines.push(line);
    } else {
      user_lines.push(line);
    }
  }

  (user_lines.join("\n"), leak_lines.join("\n"))
}

pub fn compile_and_run(source: &str) -> Result<E2EResult, String> {
  compile_and_run_inner(source, true)
}

pub fn compile_and_run_no_lsan(source: &str) -> Result<E2EResult, String> {
  compile_and_run_inner(source, false)
}

fn compile_and_run_inner(
  source: &str,
  check_leaks: bool,
) -> Result<E2EResult, String> {
  let c_code = compile_to_c(source)?;

  let temp_dir = TempDir::new().map_err(|e| format!("Failed to create temp dir: {}", e))?;
  let c_path = temp_dir.path().join("test.c");
  let bin_path = temp_dir.path().join("test");
  let fixtures_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures");

  std::fs::write(&c_path, &c_code).map_err(|e| format!("Failed to write C file: {}", e))?;

  let mut gcc = Command::new("gcc");
  gcc.arg("-I").arg(&fixtures_dir);

  if check_leaks {
    gcc.arg("-fsanitize=leak").arg("-g").arg("-fno-omit-frame-pointer");
  }

  gcc.arg(&c_path).arg("-o").arg(&bin_path);

  let compile = gcc.output().map_err(|e| format!("Failed to run gcc: {}", e))?;

  if !compile.status.success() {
    return Err(format!(
      "gcc compilation failed:\nC code:\n{}\n\nstderr:\n{}",
      c_code,
      String::from_utf8_lossy(&compile.stderr)
    ));
  }

  let mut run_cmd = Command::new(&bin_path);

  if check_leaks {
    run_cmd.env("LSAN_OPTIONS", "detect_leaks=1:leak_check_at_exit=1");
  } else {
    run_cmd.env("LSAN_OPTIONS", "detect_leaks=0");
  }

  let run = run_cmd.output().map_err(|e| format!("Failed to run binary: {}", e))?;

  let raw_exit = run.status.code().unwrap_or(-1);
  let raw_stderr = String::from_utf8_lossy(&run.stderr).to_string();

  let leaked = check_leaks && raw_exit == LSAN_EXIT_CODE;
  let (user_stderr, leak_report) = split_lsan_output(&raw_stderr);

  let exit_code = if leaked {
    // LSan overwrites the exit code; report 0 so snapshots stay stable.
    0
  } else {
    raw_exit
  };

  Ok(E2EResult {
    exit_code,
    stdout: String::from_utf8_lossy(&run.stdout).to_string(),
    stderr: user_stderr,
    leaked,
    leak_report,
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

/// Returns analyzer warning messages, or Err if lex/parse fails.
pub fn compile_warnings(source: &str) -> Result<Vec<String>, String> {
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
    .filter(|d| matches!(d.severity, ignis_diagnostics::diagnostic_report::Severity::Warning))
    .map(|d| d.message.clone())
    .collect();

  Ok(messages)
}

/// Returns ownership checker error messages (from post-mono analysis), or Err if earlier phases fail.
pub fn compile_ownership_diagnostics(source: &str) -> Result<Vec<String>, String> {
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

  let mono_roots = {
    let sym_table = result.symbols.borrow();
    collect_mono_roots(&result.defs, &sym_table)
  };
  let mono_output = ignis_analyzer::mono::Monomorphizer::new(
    &result.hir,
    &result.defs,
    &result.namespaces,
    &mut types,
    result.symbols.clone(),
  )
  .run(&mono_roots);

  let sym_table = result.symbols.borrow();

  let ownership_checker =
    ignis_analyzer::HirOwnershipChecker::new(&mono_output.hir, &types, &mono_output.defs, &sym_table);
  let (_, ownership_diagnostics) = ownership_checker.check();

  let borrow_checker = ignis_analyzer::HirBorrowChecker::new(&mono_output.hir, &types, &mono_output.defs, &sym_table);
  let borrow_diagnostics = borrow_checker.check();

  let all_diagnostics: Vec<_> = ownership_diagnostics.into_iter().chain(borrow_diagnostics).collect();

  let messages: Vec<String> = all_diagnostics
    .iter()
    .filter(|d| matches!(d.severity, ignis_diagnostics::diagnostic_report::Severity::Error))
    .map(|d| {
      let mut parts = vec![d.message.clone()];
      for note in &d.notes {
        parts.push(format!("  note: {}", note));
      }
      parts.join("\n")
    })
    .collect();

  Ok(messages)
}

fn compile_to_c_with_ctx(
  source: &str,
  ctx: CompilationContext,
) -> Result<String, String> {
  let source = source.to_string();

  let mut sm = SourceMap::new();
  let file_id = sm.add_file("test.ign", source);
  let src = &sm.get(&file_id).text;

  let mut lexer = IgnisLexer::new(file_id, src);
  lexer.scan_tokens();
  if !lexer.diagnostics.is_empty() {
    return Err(format!("Lexer errors: {:?}", lexer.diagnostics));
  }

  let symbol_table = Rc::new(RefCell::new(SymbolTable::new()));
  let mut parser = IgnisParser::new_with_compilation_ctx(lexer.tokens, symbol_table.clone(), ctx);
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

  let mono_roots = {
    let sym_table = result.symbols.borrow();
    collect_mono_roots(&result.defs, &sym_table)
  };
  let mono_output = ignis_analyzer::mono::Monomorphizer::new(
    &result.hir,
    &result.defs,
    &result.namespaces,
    &mut types,
    result.symbols.clone(),
  )
  .run(&mono_roots);

  let sym_table = result.symbols.borrow();

  let ownership_checker =
    ignis_analyzer::HirOwnershipChecker::new(&mono_output.hir, &types, &mono_output.defs, &sym_table);
  let (drop_schedules, _) = ownership_checker.check();

  let borrow_checker = ignis_analyzer::HirBorrowChecker::new(&mono_output.hir, &types, &mono_output.defs, &sym_table);
  let borrow_diagnostics = borrow_checker.check();
  if borrow_diagnostics
    .iter()
    .any(|d| matches!(d.severity, ignis_diagnostics::diagnostic_report::Severity::Error))
  {
    return Err(format!("Borrow check errors: {:?}", borrow_diagnostics));
  }

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

  Ok(ignis_codegen_c::emit_c_from_input(
    ignis_codegen_c::EmitInput::new(&lir, &types, &mono_output.defs).with_source_map(&sm),
    &result.namespaces,
    &sym_table,
    &headers,
  ))
}

pub fn compile_and_run_with_ctx(
  source: &str,
  ctx: CompilationContext,
) -> Result<E2EResult, String> {
  let c_code = compile_to_c_with_ctx(source, ctx)?;

  let temp_dir = TempDir::new().map_err(|e| format!("Failed to create temp dir: {}", e))?;
  let c_path = temp_dir.path().join("test.c");
  let bin_path = temp_dir.path().join("test");
  let fixtures_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures");

  std::fs::write(&c_path, &c_code).map_err(|e| format!("Failed to write C file: {}", e))?;

  let mut gcc = Command::new("gcc");
  gcc
    .arg("-I")
    .arg(&fixtures_dir)
    .arg("-fsanitize=leak")
    .arg("-g")
    .arg("-fno-omit-frame-pointer")
    .arg(&c_path)
    .arg("-o")
    .arg(&bin_path);

  let compile = gcc.output().map_err(|e| format!("Failed to run gcc: {}", e))?;
  if !compile.status.success() {
    return Err(format!(
      "gcc compilation failed:\nC code:\n{}\n\nstderr:\n{}",
      c_code,
      String::from_utf8_lossy(&compile.stderr)
    ));
  }

  let run = Command::new(&bin_path)
    .env("LSAN_OPTIONS", "detect_leaks=1:leak_check_at_exit=1")
    .output()
    .map_err(|e| format!("Failed to run binary: {}", e))?;

  let raw_exit = run.status.code().unwrap_or(-1);
  let raw_stderr = String::from_utf8_lossy(&run.stderr).to_string();
  let leaked = raw_exit == LSAN_EXIT_CODE;
  let (user_stderr, leak_report) = split_lsan_output(&raw_stderr);

  let exit_code = if leaked { 0 } else { raw_exit };

  Ok(E2EResult {
    exit_code,
    stdout: String::from_utf8_lossy(&run.stdout).to_string(),
    stderr: user_stderr,
    leaked,
    leak_report,
  })
}

pub fn parse_errors_with_ctx(
  source: &str,
  ctx: CompilationContext,
) -> Vec<String> {
  let source = source.to_string();

  let mut sm = SourceMap::new();
  let file_id = sm.add_file("test.ign", source);
  let src = &sm.get(&file_id).text;

  let mut lexer = IgnisLexer::new(file_id, src);
  lexer.scan_tokens();
  if !lexer.diagnostics.is_empty() {
    return lexer.diagnostics.iter().map(|d| format!("{}", d)).collect();
  }

  let symbol_table = Rc::new(RefCell::new(SymbolTable::new()));
  let mut parser = IgnisParser::new_with_compilation_ctx(lexer.tokens, symbol_table.clone(), ctx);

  match parser.parse() {
    Ok(_) => Vec::new(),
    Err(errs) => errs.iter().map(|d| format!("{}", d)).collect(),
  }
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
        for entry in rd.instance_methods.values() {
          match entry {
            SymbolEntry::Single(id) => roots.push(*id),
            SymbolEntry::Overload(ids) => roots.extend(ids),
          }
        }
        for entry in rd.static_methods.values() {
          match entry {
            SymbolEntry::Single(id) => roots.push(*id),
            SymbolEntry::Overload(ids) => roots.extend(ids),
          }
        }
      },

      // Include non-generic enums and their methods
      DefinitionKind::Enum(ed) if ed.type_params.is_empty() => {
        for entry in ed.instance_methods.values() {
          match entry {
            SymbolEntry::Single(id) => roots.push(*id),
            SymbolEntry::Overload(ids) => roots.extend(ids),
          }
        }
        for entry in ed.static_methods.values() {
          match entry {
            SymbolEntry::Single(id) => roots.push(*id),
            SymbolEntry::Overload(ids) => roots.extend(ids),
          }
        }
      },

      _ => {},
    }
  }

  roots
}
