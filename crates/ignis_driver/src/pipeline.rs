use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::Arc;
use std::time::Instant;

use colored::*;
use ignis_ast::display::format_ast_nodes;
use ignis_config::{
  DebugTrace, DumpKind, IgnisBuildConfig, IgnisConfig, IgnisProjectConfig, IgnisSTDManifest, TargetBackend,
};

use ignis_log::{
  cmd_artifact, cmd_fail, cmd_header, cmd_ok, cmd_stats, log_dbg, log_phase, log_trc, phase_log, phase_ok, phase_warn,
  section, section_item, trace_dbg,
};
use ignis_type::definition::{DefinitionId, DefinitionKind, DefinitionStore, SymbolEntry, Visibility};
use ignis_type::file::SourceMap;
use ignis_type::module::{ModuleId, ModulePath};
use ignis_type::symbol::SymbolTable;
use ignis_type::types::{Type, TypeId, TypeStore};
use ignis_diagnostics::message::DiagnosticMessage;

use crate::api::analyze_text;
use crate::backend::{
  emit_text, select_backend, BackendRequest, HeaderBackendRequest, LoweredBackendRequest, SelectedBackend,
};
use crate::build_layout::{
  hash_file, is_module_stamp_valid, is_std_stamp_valid, write_if_changed, write_module_stamp, write_std_stamp,
  BuildFingerprint, BuildLayout, FileEntry, ModuleStamp, StdStamp,
};
use crate::context::CompilationContext;
use crate::link::{
  compile_to_object, format_tool_error, link_executable, link_executable_multi, rebuild_std_runtime, LinkPlan,
};
use crate::project::{CliOverrides, load_project_toml, resolve_project};
use crate::stages::{AnalyzedStage, BackendInput, CheckedStage, LirStage, ParsedStage, StageError};

#[derive(Debug, Clone)]
struct TestDriverInput {
  config: Arc<IgnisConfig>,
  entry_path: PathBuf,
  source_dir: PathBuf,
  out_dir: PathBuf,
  layout: BuildLayout,
  harness_c_path: PathBuf,
  bin_path: PathBuf,
}

/// Compiler version for stamp file invalidation.
const COMPILER_VERSION: &str = env!("CARGO_PKG_VERSION");

fn dump_requested(
  config: &IgnisConfig,
  kind: DumpKind,
) -> bool {
  config
    .build_config
    .as_ref()
    .map(|bc| bc.dump.contains(&kind))
    .unwrap_or(false)
}

fn warn_unsupported_dumps(config: &IgnisConfig) {
  if dump_requested(config, DumpKind::Ir) {
    eprintln!("{} Dump kind 'ir' is not supported yet.", "Warning:".yellow().bold());
  }
}

fn configured_backend_target(config: &IgnisConfig) -> TargetBackend {
  config
    .build_config
    .as_ref()
    .map(|build_config| build_config.target)
    .unwrap_or(TargetBackend::C)
}

fn select_backend_or_report(
  config: &IgnisConfig,
  failure_label: &str,
  start: Instant,
) -> Result<SelectedBackend, ()> {
  select_backend(configured_backend_target(config)).map_err(|stage_error| {
    cmd_fail!(config, failure_label, start.elapsed());
    eprintln!("{} {}", "Error:".red().bold(), stage_error);
  })
}

fn is_supported_entry_main_args(
  params: &[DefinitionId],
  defs: &DefinitionStore,
  types: &TypeStore,
) -> bool {
  match params {
    [] => true,
    [argc_param, argv_param] => {
      let argc_type = *defs.type_of(argc_param);
      if argc_type != types.i32() {
        return false;
      }

      let argv_type = *defs.type_of(argv_param);
      matches!(types.get(&argv_type), Type::Pointer { inner, .. } if matches!(types.get(inner), Type::Str))
    },
    _ => false,
  }
}

fn is_supported_entry_main_return(
  return_type: TypeId,
  types: &TypeStore,
  defs: &DefinitionStore,
) -> bool {
  match types.get(&return_type) {
    Type::I32 | Type::Void => true,

    // Post-monomorphization: concrete enum type.
    Type::Enum(enum_def_id) => is_try_capable_with_i32_ok(*enum_def_id, types, defs, &[]),

    // Pre-monomorphization: generic enum instance (e.g. Result<i32, IoError>).
    Type::Instance { generic, args } => is_try_capable_with_i32_ok(*generic, types, defs, args),

    _ => false,
  }
}

fn is_try_capable_with_i32_ok(
  enum_def_id: DefinitionId,
  types: &TypeStore,
  defs: &DefinitionStore,
  type_args: &[TypeId],
) -> bool {
  let DefinitionKind::Enum(enum_def) = &defs.get(&enum_def_id).kind else {
    return false;
  };

  let Some(try_capability) = enum_def.try_capable else {
    return false;
  };

  let Some(ok_variant) = enum_def
    .variants
    .iter()
    .find(|variant| variant.tag_value == try_capability.ok_variant)
  else {
    return false;
  };

  let has_error_variant = enum_def
    .variants
    .iter()
    .any(|variant| variant.tag_value == try_capability.err_variant);

  if !has_error_variant || ok_variant.payload.len() != 1 {
    return false;
  }

  let ok_type = resolve_type_param(types, ok_variant.payload[0], enum_def_id, type_args);
  ok_type == types.i32()
}

fn resolve_type_param(
  types: &TypeStore,
  ty: TypeId,
  owner: DefinitionId,
  args: &[TypeId],
) -> TypeId {
  match types.get(&ty) {
    Type::Param {
      owner: param_owner,
      index,
    } if *param_owner == owner => args.get(*index as usize).copied().unwrap_or(ty),
    _ => ty,
  }
}

fn validate_entry_main_signature(
  defs: &DefinitionStore,
  types: &TypeStore,
  entry_def_id: DefinitionId,
) -> Result<(), DiagnosticMessage> {
  let entry_def = defs.get(&entry_def_id);

  let DefinitionKind::Function(entry_function) = &entry_def.kind else {
    return Err(DiagnosticMessage::CompileError {
      message: "Internal error: entry point must be a function".to_string(),
      span: entry_def.span.clone(),
    });
  };

  if !is_supported_entry_main_args(&entry_function.params, defs, types) {
    return Err(DiagnosticMessage::CompileError {
      message: "Function 'main' parameters must be empty or (i32, *str)".to_string(),
      span: entry_def.span.clone(),
    });
  }

  if !is_supported_entry_main_return(entry_function.return_type, types, defs) {
    return Err(DiagnosticMessage::CompileError {
      message: "Function 'main' must return i32, void, or a try-capable enum with i32 ok variant".to_string(),
      span: entry_def.span.clone(),
    });
  }

  Ok(())
}

fn sanitize_dump_name(name: &str) -> String {
  name
    .chars()
    .map(|ch| {
      if ch.is_ascii_alphanumeric() || ch == '-' || ch == '_' {
        ch
      } else {
        '_'
      }
    })
    .collect()
}

fn write_dump_output(
  config: &IgnisConfig,
  file_name: &str,
  content: &str,
) -> Result<(), ()> {
  if let Some(build_config) = config.build_config.as_ref()
    && let Some(dump_dir) = &build_config.dump_dir
  {
    let output_dir = Path::new(dump_dir);
    if let Err(e) = std::fs::create_dir_all(output_dir) {
      eprintln!(
        "{} Failed to create dump directory '{}': {}",
        "Error:".red().bold(),
        output_dir.display(),
        e
      );
      return Err(());
    }

    let output_path = output_dir.join(file_name);
    if let Err(e) = std::fs::write(&output_path, content) {
      eprintln!(
        "{} Failed to write dump file '{}': {}",
        "Error:".red().bold(),
        output_path.display(),
        e
      );
      return Err(());
    }

    return Ok(());
  }

  println!("\n{}", content);
  Ok(())
}

/// Read a file from disk, reporting errors to stderr.
fn read_file_or_report(file_path: &str) -> Result<String, ()> {
  match std::fs::read_to_string(file_path) {
    Ok(content) => Ok(content),
    Err(e) => {
      eprintln!("{} Failed to read file '{}': {}", "Error:".red().bold(), file_path, e);
      Err(())
    },
  }
}

/// Render all diagnostics to stderr.
fn render_diagnostics(
  diagnostics: &[ignis_diagnostics::diagnostic_report::Diagnostic],
  source_map: &SourceMap,
  quiet: bool,
) {
  if quiet {
    return;
  }

  for diag in diagnostics {
    ignis_diagnostics::render(diag, source_map);
  }
}

/// Write all requested dumps for compile_file.
fn write_compile_file_dumps(
  config: &IgnisConfig,
  output: &crate::api::AnalyzeTextOutput,
) -> Result<(), ()> {
  if dump_requested(config, DumpKind::Lexer) {
    let mut dump = String::new();
    for token in &output.tokens {
      dump.push_str(&format!("{}\n", token));
    }
    write_dump_output(config, "dump-lexer.txt", &dump)?;
  }

  if dump_requested(config, DumpKind::Ast) {
    let ast_lisp = format_ast_nodes(output.nodes.clone(), output.symbol_table.clone(), &output.roots);
    write_dump_output(config, "dump-ast.txt", &ast_lisp)?;
  }

  let sym_table = output.analyzer.symbols.borrow();

  if dump_requested(config, DumpKind::Types) {
    let types_dump = ignis_analyzer::dump::dump_types(&output.analyzer.types);
    write_dump_output(config, "dump-types.txt", &types_dump)?;
  }

  if dump_requested(config, DumpKind::Defs) {
    let defs_dump = ignis_analyzer::dump::dump_defs(&output.analyzer.defs, &output.analyzer.types, &sym_table);
    write_dump_output(config, "dump-defs.txt", &defs_dump)?;
  }

  if dump_requested(config, DumpKind::HirSummary) {
    let summary_dump = ignis_analyzer::dump::dump_hir_summary(&output.analyzer.hir, &output.analyzer.defs, &sym_table);
    write_dump_output(config, "dump-hir-summary.txt", &summary_dump)?;
  }

  if let Some(build_config) = config.build_config.as_ref()
    && let Some(func_name) = &build_config.dump_hir
  {
    match ignis_analyzer::dump::dump_hir_function(&output.analyzer.hir, &output.analyzer.defs, &sym_table, func_name) {
      Ok(dump) => {
        let file_name = format!("dump-hir-{}.txt", sanitize_dump_name(func_name));
        write_dump_output(config, &file_name, &dump)?;
      },
      Err(err) => eprintln!("{} {}", "Error:".red().bold(), err),
    }
  }

  if dump_requested(config, DumpKind::Hir) {
    let hir_dump = ignis_analyzer::dump::dump_hir_complete(
      &output.analyzer.hir,
      &output.analyzer.types,
      &output.analyzer.defs,
      &sym_table,
    );
    write_dump_output(config, "dump-hir.txt", &hir_dump)?;
  }

  Ok(())
}

/// Compile a single file (used for simple single-file compilation without imports)
pub fn compile_file(
  config: Arc<IgnisConfig>,
  file_path: &str,
) -> Result<(), ()> {
  warn_unsupported_dumps(&config);

  let text = read_file_or_report(file_path)?;

  phase_log!(&config, "Scanning... {}", file_path);

  let output = analyze_text(file_path, text);

  trace_dbg!(&config, DebugTrace::Lexer, "produced {} tokens", output.tokens.len());

  phase_log!(&config, "Parsing... {}", file_path);
  log_dbg!(&config, "parsing {}", file_path);

  trace_dbg!(&config, DebugTrace::Parser, "produced {} nodes", output.nodes.len());

  phase_ok!(&config, "{}", "Running analyzer...".bright_cyan().bold());

  trace_dbg!(
    &config,
    DebugTrace::Analyzer,
    "emitted {} diagnostics",
    output.diagnostics.len()
  );

  render_diagnostics(&output.diagnostics, &output.source_map, config.quiet);

  if output.has_errors {
    return Err(());
  }

  write_compile_file_dumps(&config, &output)?;

  Ok(())
}

/// Compile a project with multi-module support
pub fn compile_project(
  config: Arc<IgnisConfig>,
  entry_path: &str,
) -> Result<(), ()> {
  let start = Instant::now();
  let is_check_mode = config.build_config.as_ref().map(|bc| bc.check_mode).unwrap_or(false);
  let cmd_label = if is_check_mode { "Checking" } else { "Building" };

  cmd_header!(&config, cmd_label, entry_path);

  warn_unsupported_dumps(&config);

  section!(&config, "Scanning & parsing");

  let mut ctx = CompilationContext::new(&config);

  // For std entries, discover all manifest modules up front so the
  // insertion order matches check-std (avoids prelude-cycle ordering issues).
  let entry_is_std = ctx.try_resolve_std_module_name(entry_path).is_some();
  if entry_is_std && config.std {
    ctx.discover_all_std_modules(&config);
  }

  let root_id = match ctx.discover_modules(entry_path, &config) {
    Ok(id) => id,
    Err(()) => {
      cmd_fail!(
        &config,
        if is_check_mode { "Check failed" } else { "Build failed" },
        start.elapsed()
      );
      return Err(());
    },
  };

  let should_load_prelude = config.std && config.auto_load_std;

  if should_load_prelude {
    ctx.discover_prelude_modules_for_all(&config);
  }

  ctx.module_graph.root = Some(root_id);

  let parsed_stage = ParsedStage::new(ctx, root_id);

  if let Err(stage_error) = parsed_stage.verify() {
    cmd_fail!(
      &config,
      if is_check_mode { "Check failed" } else { "Build failed" },
      start.elapsed()
    );
    eprintln!("{} {}", "Error:".red().bold(), stage_error);
    return Err(());
  }

  let std_module_names: Vec<String> = parsed_stage
    .ctx
    .module_graph
    .modules
    .iter()
    .filter(|(_, m)| m.path.is_std())
    .map(|(_, m)| m.path.module_name())
    .collect();
  let user_module_names: Vec<String> = parsed_stage
    .ctx
    .module_graph
    .modules
    .iter()
    .filter(|(_, m)| !m.path.is_std())
    .map(|(_, m)| m.path.module_name())
    .collect();

  let std_count = std_module_names.len();
  let user_count = user_module_names.len();

  if user_count > 1 {
    section_item!(&config, "{} (+{})", entry_path, user_count - 1);
  } else {
    section_item!(&config, "{}", entry_path);
  }

  if std_count > 0 {
    section!(&config, "Loading standard library");
    if ignis_log::show_verbose(&config) {
      for name in &std_module_names {
        phase_log!(&config, "{}", name);
      }
    } else {
      section_item!(&config, "{} modules", std_count);
    }
  }

  log_dbg!(&config, "compiling project entry {}", entry_path);

  section!(&config, "Analyzing");

  let analyzed_stage = match parsed_stage.analyze(&config) {
    Ok(stage) => stage,
    Err(StageError::AnalysisFailed) => {
      cmd_fail!(
        &config,
        if is_check_mode { "Check failed" } else { "Build failed" },
        start.elapsed()
      );
      return Err(());
    },
    Err(stage_error) => {
      cmd_fail!(
        &config,
        if is_check_mode { "Check failed" } else { "Build failed" },
        start.elapsed()
      );
      eprintln!("{} {}", "Error:".red().bold(), stage_error);
      return Err(());
    },
  };

  trace_dbg!(
    &config,
    DebugTrace::Analyzer,
    "compilation produced {} diagnostics",
    analyzed_stage.semantic.diagnostics.len()
  );

  // Dump phase: use a scoped borrow that ends before monomorphization
  {
    let sym_table = analyzed_stage.semantic.symbols.borrow();

    if dump_requested(&config, DumpKind::Types) {
      let types_dump = ignis_analyzer::dump::dump_types(&analyzed_stage.semantic.types);
      write_dump_output(&config, "dump-types.txt", &types_dump)?;
    }

    if dump_requested(&config, DumpKind::Defs) {
      let defs_dump =
        ignis_analyzer::dump::dump_defs(&analyzed_stage.semantic.defs, &analyzed_stage.semantic.types, &sym_table);
      write_dump_output(&config, "dump-defs.txt", &defs_dump)?;
    }

    if dump_requested(&config, DumpKind::HirSummary) {
      let summary_dump =
        ignis_analyzer::dump::dump_hir_summary(&analyzed_stage.hir, &analyzed_stage.semantic.defs, &sym_table);
      write_dump_output(&config, "dump-hir-summary.txt", &summary_dump)?;
    }

    if let Some(build_config) = config.build_config.as_ref()
      && let Some(func_name) = &build_config.dump_hir
    {
      match ignis_analyzer::dump::dump_hir_function(
        &analyzed_stage.hir,
        &analyzed_stage.semantic.defs,
        &sym_table,
        func_name,
      ) {
        Ok(out) => {
          let file_name = format!("dump-hir-{}.txt", sanitize_dump_name(func_name));
          write_dump_output(&config, &file_name, &out)?;
        },
        Err(err) => eprintln!("{} {}", "Error:".red().bold(), err),
      }
    }

    if dump_requested(&config, DumpKind::Hir) {
      let hir_dump = ignis_analyzer::dump::dump_hir_complete(
        &analyzed_stage.hir,
        &analyzed_stage.semantic.types,
        &analyzed_stage.semantic.defs,
        &sym_table,
      );
      write_dump_output(&config, "dump-hir.txt", &hir_dump)?;
    }
  }

  let AnalyzedStage {
    ctx,
    root_id,
    semantic,
    hir,
  } = analyzed_stage;

  if let Some(bc) = config.build_config.as_ref() {
    let has_entry_point = hir.entry_point.is_some();
    let is_lib = bc.lib;
    let is_bin = !is_lib;
    let check_mode = bc.check_mode;
    let analyze_only = bc.analyze_only;
    let dump_c_requested = dump_requested(&config, DumpKind::C);

    if analyze_only {
      cmd_ok!(&config, "No errors found", start.elapsed());
      return Ok(());
    }

    if is_bin && !has_entry_point {
      let root_module = ctx.module_graph.modules.get(&root_id);
      let file_id = root_module.file_id;
      let span = ignis_type::span::Span::empty_at(file_id, ignis_type::BytePosition::default());
      cmd_fail!(&config, "Build failed", start.elapsed());
      ignis_diagnostics::render(
        &DiagnosticMessage::ExecutableMustHaveMainFunction { span }.report(),
        &ctx.source_map,
      );
      return Err(());
    }

    if is_lib && has_entry_point {
      let entry_def_id = hir.entry_point.unwrap();
      let span = semantic.defs.get(&entry_def_id).span.clone();
      cmd_fail!(&config, "Build failed", start.elapsed());
      ignis_diagnostics::render(
        &DiagnosticMessage::LibraryCannotHaveMainFunction { span }.report(),
        &ctx.source_map,
      );
      return Err(());
    }

    if is_bin
      && let Some(entry_def_id) = hir.entry_point
      && let Err(diagnostic_message) = validate_entry_main_signature(&semantic.defs, &semantic.types, entry_def_id)
    {
      cmd_fail!(&config, "Build failed", start.elapsed());
      ignis_diagnostics::render(&diagnostic_message.report(), &ctx.source_map);
      return Err(());
    }

    let needs_codegen = check_mode
      || dump_requested(&config, DumpKind::Lir)
      || dump_c_requested
      || bc.emit_c.is_some()
      || bc.emit_obj.is_some()
      || bc.emit_bin.is_some()
      || bc.lib;

    if needs_codegen {
      let used_modules = ctx.module_graph.topological_sort();

      section!(&config, "Codegen & linking");

      if ignis_log::show_verbose(&config) {
        for module_id in &used_modules {
          let module = ctx.module_graph.modules.get(module_id);
          if !module.path.is_std() {
            phase_log!(&config, "codegen {}", module.path.module_name());
          }
        }
      }

      log_dbg!(&config, "preparing codegen for {} modules", used_modules.len());
      log_trc!(&config, "codegen module order {:?}", used_modules);

      trace_dbg!(&config, DebugTrace::Std, "ensuring standard library is built");

      if ensure_std_built(&used_modules, &ctx.module_graph, &config).is_err() {
        cmd_fail!(&config, "Build failed", start.elapsed());
        return Err(());
      }

      let manifest = if config.manifest.modules.is_empty() {
        None
      } else {
        Some(&config.manifest)
      };
      let build_dir = config
        .build_config
        .as_ref()
        .map(|bc| bc.output_dir.clone())
        .unwrap_or_else(|| "build".to_string());
      let mut link_plan = LinkPlan::from_modules(
        &used_modules,
        &ctx.module_graph,
        Path::new(&config.std_path),
        Path::new(&build_dir),
        manifest,
      );
      link_plan.cc = config.c_compiler.clone();
      link_plan.cflags = config.cflags.clone();

      if bc.rebuild_std {
        trace_dbg!(&config, DebugTrace::Std, "rebuilding standard library runtime");

        if let Err(e) = rebuild_std_runtime(Path::new(&config.std_path), config.quiet) {
          cmd_fail!(&config, "Build failed", start.elapsed());
          eprintln!("{} {}", "Error:".red().bold(), e);
          return Err(());
        }
      }

      let mut types = semantic.types.clone();

      // Monomorphization: transform generic HIR into concrete HIR
      // Use a temporary borrow for collect_mono_roots that drops before Monomorphizer::run()
      let mono_roots = collect_mono_roots(&semantic.defs, &semantic.symbols.borrow());
      let mono_output = ignis_analyzer::mono::Monomorphizer::new(
        &hir,
        &semantic.defs,
        &semantic.namespaces,
        &mut types,
        semantic.symbols.clone(),
      )
      .run(&mono_roots);

      trace_dbg!(&config, DebugTrace::Mono, "monomorphization completed");

      // In debug builds, verify no generic types remain
      #[cfg(debug_assertions)]
      mono_output.verify_no_generics(&types);

      let (drop_schedules, ownership_diagnostics, borrow_diagnostics) = {
        let sym_table = semantic.symbols.borrow();

        let ownership_checker =
          ignis_analyzer::HirOwnershipChecker::new(&mono_output.hir, &types, &mono_output.defs, &sym_table)
            .with_source_map(&ctx.source_map);
        let (drop_schedules, ownership_diagnostics) = ownership_checker.check();

        let borrow_checker =
          ignis_analyzer::HirBorrowChecker::new(&mono_output.hir, &types, &mono_output.defs, &sym_table)
            .with_source_map(&ctx.source_map);
        let borrow_diagnostics = borrow_checker.check();

        (drop_schedules, ownership_diagnostics, borrow_diagnostics)
      };

      let mut post_mono_diagnostics = ownership_diagnostics;
      post_mono_diagnostics.extend(borrow_diagnostics);

      trace_dbg!(
        &config,
        DebugTrace::Ownership,
        "post-mono check produced {} diagnostics",
        post_mono_diagnostics.len()
      );

      if !config.quiet {
        for diag in &post_mono_diagnostics {
          ignis_diagnostics::render(diag, &ctx.source_map);
        }
      }
      let mut checked_stage = CheckedStage::new(
        AnalyzedStage {
          ctx,
          root_id,
          semantic,
          hir,
        },
        types,
        mono_output,
        drop_schedules,
        post_mono_diagnostics,
      );

      let error_count = checked_stage
        .diagnostics
        .iter()
        .filter(|d| matches!(d.severity, ignis_diagnostics::diagnostic_report::Severity::Error))
        .count();
      let warning_count = checked_stage
        .diagnostics
        .iter()
        .filter(|d| matches!(d.severity, ignis_diagnostics::diagnostic_report::Severity::Warning))
        .count();

      if let Err(stage_error) = checked_stage.verify() {
        cmd_fail!(
          &config,
          if check_mode { "Check failed" } else { "Build failed" },
          start.elapsed()
        );
        cmd_stats!(&config, error_count, warning_count);
        eprintln!("{} {}", "Error:".red().bold(), stage_error);
        return Err(());
      }

      // TODO: Filter to project_modules only when std is pre-compiled
      let used_module_set: std::collections::HashSet<ignis_type::module::ModuleId> =
        used_modules.iter().copied().collect();

      let symbols = checked_stage.semantic.symbols.clone();
      let sym_table = symbols.borrow();
      let (lir_program, verify_result) = ignis_lir::lowering::lower_and_verify(
        &checked_stage.mono_output.hir,
        &mut checked_stage.types,
        &checked_stage.mono_output.defs,
        &sym_table,
        &checked_stage.drop_schedules,
        Some(&used_module_set),
      );

      drop(sym_table);

      let lir_stage = LirStage::new(checked_stage, lir_program, verify_result);

      let LirStage {
        ctx,
        root_id: _,
        semantic,
        hir: _,
        types,
        mono_output,
        drop_schedules: _,
        diagnostics: _,
        program: lir_program,
        verification: verify_result,
      } = lir_stage;

      let sym_table = semantic.symbols.borrow();

      trace_dbg!(&config, DebugTrace::Lir, "lowering completed");

      if dump_requested(&config, DumpKind::Lir) {
        let lir_output = ignis_lir::display::print_lir(&lir_program, &types, &mono_output.defs, &sym_table);
        write_dump_output(&config, "dump-lir.txt", &lir_output)?;
      }

      if let Err(errors) = &verify_result {
        eprintln!("{} LIR verification errors:", "Warning:".yellow().bold());
        for err in errors {
          eprintln!("  {:?}", err);
        }

        if config.debug {
          cmd_fail!(
            &config,
            if check_mode { "Check failed" } else { "Build failed" },
            start.elapsed()
          );
          eprintln!("{} LIR verification failed in debug mode", "Error:".red().bold());
          return Err(());
        }
      }

      let needs_emit = check_mode
        || dump_c_requested
        || bc.emit_c.is_some()
        || bc.emit_obj.is_some()
        || bc.emit_bin.is_some()
        || bc.lib;
      if needs_emit {
        trace_dbg!(&config, DebugTrace::Codegen, "emitting C code");
        let selected_backend =
          select_backend_or_report(&config, if check_mode { "Check failed" } else { "Build failed" }, start)?;

        if verify_result.is_err() {
          cmd_fail!(
            &config,
            if check_mode { "Check failed" } else { "Build failed" },
            start.elapsed()
          );
          eprintln!("{} Cannot emit C: LIR verification failed", "Error:".red().bold());
          return Err(());
        }

        let base_name = bc
          .file
          .as_ref()
          .and_then(|f| Path::new(f).file_stem())
          .and_then(|s| s.to_str())
          .unwrap_or("out");

        // Per-module compilation when precompiled std is available
        if link_plan.std_archive.is_some() && bc.emit_bin.is_some() {
          let module_paths = build_module_paths_from_graph(&ctx.module_graph);

          let out_dir_path = Path::new(&bc.output_dir);
          let project_root = if out_dir_path.is_absolute() {
            out_dir_path.parent().map(|p| p.to_path_buf())
          } else {
            std::env::current_dir().ok()
          };

          let layout = match &project_root {
            Some(root) => BuildLayout::with_project_root(base_name, out_dir_path, root),
            None => BuildLayout::new(base_name, out_dir_path),
          };

          // Create user directories
          if let Err(e) = layout.create_user_dirs() {
            cmd_fail!(&config, "Build failed", start.elapsed());
            eprintln!("{} Failed to create user directories: {}", "Error:".red().bold(), e);
            return Err(());
          }

          let std_defined_symbols = match &link_plan.std_archive {
            Some(archive_path) => match collect_archive_defined_symbols(archive_path) {
              Ok(symbols) => Some(symbols),
              Err(error) => {
                cmd_fail!(&config, "Build failed", start.elapsed());
                eprintln!("{} {}", "Error:".red().bold(), error);
                return Err(());
              },
            },
            None => None,
          };

          // Collect user modules (exclude std and std-internal files)
          let std_dir = ctx.module_graph.std_path();
          let user_modules: Vec<_> = used_modules
            .iter()
            .filter(|id| {
              let module = ctx.module_graph.modules.get(id);
              module.path.is_project() && !module.path.is_inside_dir(std_dir)
            })
            .collect();

          if user_modules.is_empty() {
            cmd_fail!(&config, "Build failed", start.elapsed());
            eprintln!("{} No user modules found", "Error:".red().bold());
            return Err(());
          }

          // Collect source paths for all user modules
          let source_paths: Vec<PathBuf> = user_modules
            .iter()
            .filter_map(|module_id| {
              let module = ctx.module_graph.modules.get(module_id);
              match &module.path {
                ModulePath::Project(p) => Some(p.clone()),
                _ => None,
              }
            })
            .collect();

          // Create build fingerprint for stamp validation
          let fingerprint = BuildFingerprint {
            compiler_version: COMPILER_VERSION.to_string(),
            codegen_abi_version: ignis_codegen_c::CODEGEN_ABI_VERSION,
            target: String::new(), // TODO: add target triple when cross-compilation is supported
          };

          // Pre-compute hashes for all user modules
          let mut module_hashes: std::collections::HashMap<PathBuf, String> = std::collections::HashMap::new();
          for source_path in &source_paths {
            match hash_file(source_path) {
              Ok(hash) => {
                module_hashes.insert(source_path.clone(), hash);
              },
              Err(e) => {
                cmd_fail!(&config, "Build failed", start.elapsed());
                eprintln!("{} Failed to hash '{}': {}", "Error:".red().bold(), source_path.display(), e);
                return Err(());
              },
            }
          }

          // Phase 1: Generate headers for all user modules
          for module_id in &user_modules {
            let module = ctx.module_graph.modules.get(module_id);
            let source_path = match &module.path {
              ModulePath::Project(p) => p.clone(),
              _ => continue,
            };

            // Create directories for this module
            if let Err(e) = layout.ensure_user_module_dirs(&source_path) {
              cmd_fail!(&config, "Build failed", start.elapsed());
              eprintln!("{} Failed to create module directories: {}", "Error:".red().bold(), e);
              return Err(());
            }

            // Generate header
            let header_content = match emit_text(
              &selected_backend,
              BackendInput::Lowered {
                root_id,
                types: &types,
                defs: &mono_output.defs,
                program: &lir_program,
              },
              BackendRequest::Header(HeaderBackendRequest::EmitUserModuleHeader {
                module_id: **module_id,
                source_path: &source_path,
                namespaces: &semantic.namespaces,
                symbols: &sym_table,
              }),
            ) {
              Ok(contents) => contents,
              Err(stage_error) => {
                cmd_fail!(&config, "Build failed", start.elapsed());
                eprintln!("{} {}", "Error:".red().bold(), stage_error);
                return Err(());
              },
            };

            let header_path = layout.user_module_header(&source_path);
            if let Err(e) = std::fs::write(&header_path, &header_content) {
              cmd_fail!(&config, "Build failed", start.elapsed());
              eprintln!(
                "{} Failed to write header '{}': {}",
                "Error:".red().bold(),
                header_path.display(),
                e
              );
              return Err(());
            }
          }

          // Phase 2: Generate umbrella header
          let relative_source_paths: Vec<PathBuf> = source_paths.iter().map(|p| layout.relativize(p)).collect();
          let umbrella_content = generate_user_umbrella_header(&relative_source_paths);
          let umbrella_path = layout.user_umbrella_header();
          if let Err(e) = std::fs::write(&umbrella_path, &umbrella_content) {
            cmd_fail!(&config, "Build failed", start.elapsed());
            eprintln!("{} Failed to write umbrella header: {}", "Error:".red().bold(), e);
            return Err(());
          }

          // Phase 3: Generate C files and compile to objects (with per-module caching)
          let mut object_files: Vec<PathBuf> = Vec::new();
          let mut any_module_recompiled = false;
          let mut link_plan_with_user_includes = link_plan.clone();
          link_plan_with_user_includes
            .include_dirs
            .push(layout.user_include_dir());

          for module_id in &user_modules {
            let module = ctx.module_graph.modules.get(module_id);
            let source_path = match &module.path {
              ModulePath::Project(p) => p.clone(),
              _ => continue,
            };

            let obj_path = layout.user_module_obj(&source_path);
            object_files.push(obj_path.clone());

            // Compute current module hash and dependency hashes
            let self_hash = module_hashes.get(&source_path).cloned().unwrap_or_default();

            // Get transitive dependencies (only user modules)
            let dep_ids = ctx.module_graph.transitive_deps(**module_id);
            let current_deps: Vec<FileEntry> = dep_ids
              .iter()
              .filter_map(|dep_id| {
                let dep_module = ctx.module_graph.modules.get(dep_id);
                match &dep_module.path {
                  ModulePath::Project(dep_path) => {
                    let hash = module_hashes.get(dep_path).cloned().unwrap_or_default();
                    Some(FileEntry {
                      path: dep_path.clone(),
                      hash,
                    })
                  },
                  _ => None, // Skip std modules for now (they have their own stamp)
                }
              })
              .collect();

            // Check if this module's stamp is still valid
            let stamp_path = layout.user_module_stamp_path(&source_path);
            let stamp_valid = is_module_stamp_valid(&stamp_path, &fingerprint, &self_hash, &current_deps);
            let obj_exists = obj_path.exists();

            if stamp_valid && obj_exists {
              // Module is up-to-date, skip recompilation
              continue;
            }

            any_module_recompiled = true;

            // Build include list: own header + transitive user dependencies
            let mut user_module_headers: Vec<ignis_config::CHeader> = Vec::new();

            let self_header_rel = layout.relativize(&source_path).with_extension("h");
            user_module_headers.push(ignis_config::CHeader {
              path: normalize_path_for_include(&self_header_rel),
              quoted: true,
            });

            for dep_id in &dep_ids {
              let dep_module = ctx.module_graph.modules.get(dep_id);
              if let ModulePath::Project(dep_path) = &dep_module.path {
                if dep_path.starts_with(std_dir) {
                  continue;
                }
                let dep_header_rel = layout.relativize(dep_path).with_extension("h");
                user_module_headers.push(ignis_config::CHeader {
                  path: normalize_path_for_include(&dep_header_rel),
                  quoted: true,
                });
              }
            }

            // Generate C code for this module
            let c_code = match emit_text(
              &selected_backend,
              BackendInput::Lowered {
                root_id,
                types: &types,
                defs: &mono_output.defs,
                program: &lir_program,
              },
              BackendRequest::Lowered(LoweredBackendRequest::EmitUserModule {
                module_id: **module_id,
                namespaces: &semantic.namespaces,
                symbols: &sym_table,
                headers: &link_plan_with_user_includes.headers,
                module_paths: &module_paths,
                user_module_headers: &user_module_headers,
                std_path: std_dir,
                std_defined_symbols: std_defined_symbols.as_ref(),
              }),
            ) {
              Ok(contents) => contents,
              Err(stage_error) => {
                cmd_fail!(&config, "Build failed", start.elapsed());
                eprintln!("{} {}", "Error:".red().bold(), stage_error);
                return Err(());
              },
            };

            let c_path = layout.user_module_src(&source_path);
            if let Err(e) = write_if_changed(&c_path, &c_code) {
              cmd_fail!(&config, "Build failed", start.elapsed());
              eprintln!("{} Failed to write C file '{}': {}", "Error:".red().bold(), c_path.display(), e);
              return Err(());
            }

            // Compile to object
            let suppress_link_logs = !ignis_log::show_verbose(&config);
            if let Err(e) = compile_to_object(&c_path, &obj_path, &link_plan_with_user_includes, suppress_link_logs) {
              cmd_fail!(&config, "Build failed", start.elapsed());
              eprintln!("{} {}", "Error:".red().bold(), e);
              return Err(());
            }

            // Write per-module stamp after successful compilation
            let stamp = ModuleStamp::new(fingerprint.clone(), source_path.clone(), self_hash, current_deps);
            if let Err(e) = write_module_stamp(&stamp_path, &stamp) {
              // Non-fatal: warn but don't fail the build
              eprintln!("{} Failed to write module stamp: {}", "Warning:".yellow().bold(), e);
            }
          }

          // Phase 4: Create user archive and link (only if any module changed or binary doesn't exist)
          if let Some(bin_path) = &bc.emit_bin {
            let bin_exists = Path::new(bin_path).exists();
            let user_archive_path = layout.user_lib_path();

            if !any_module_recompiled && bin_exists && user_archive_path.exists() {
              // Everything is up-to-date
              cmd_ok!(&config, "Compiled (cached)", start.elapsed());
              cmd_artifact!(&config, "Binary", bin_path);
              return Ok(());
            }

            // Create user archive from all object files
            trace_dbg!(&config, DebugTrace::Link, "creating user archive");
            let log_archive = ignis_log::show_verbose(&config);
            if let Err(e) = create_static_archive_multi(&object_files, &user_archive_path, log_archive) {
              cmd_fail!(&config, "Build failed", start.elapsed());
              eprintln!("{} {}", "Error:".red().bold(), e);
              return Err(());
            }

            // Link using the archive instead of individual objects
            trace_dbg!(&config, DebugTrace::Link, "linking executable");
            link_plan_with_user_includes.user_archive = Some(user_archive_path);

            let suppress_link_logs = !ignis_log::show_verbose(&config);
            if let Err(e) = link_executable_multi(
              &[], // No direct objects, using archive instead
              Path::new(bin_path),
              &link_plan_with_user_includes,
              suppress_link_logs,
            ) {
              cmd_fail!(&config, "Build failed", start.elapsed());
              eprintln!("{} {}", "Error:".red().bold(), e);
              return Err(());
            }

            cmd_ok!(&config, "Compiled", start.elapsed());
            cmd_artifact!(&config, "Binary", bin_path);
            return Ok(());
          }
        } else {
          // Legacy single-file compilation (no precompiled std or no binary output)
          let c_code = if link_plan.std_archive.is_some() {
            let module_paths = build_module_paths_from_graph(&ctx.module_graph);
            match emit_text(
              &selected_backend,
              BackendInput::Lowered {
                root_id,
                types: &types,
                defs: &mono_output.defs,
                program: &lir_program,
              },
              BackendRequest::Lowered(LoweredBackendRequest::EmitUserCombined {
                namespaces: &semantic.namespaces,
                symbols: &sym_table,
                headers: &link_plan.headers,
                module_paths: &module_paths,
              }),
            ) {
              Ok(contents) => contents,
              Err(stage_error) => {
                cmd_fail!(&config, "Build failed", start.elapsed());
                eprintln!("{} {}", "Error:".red().bold(), stage_error);
                return Err(());
              },
            }
          } else {
            match emit_text(
              &selected_backend,
              BackendInput::Lowered {
                root_id,
                types: &types,
                defs: &mono_output.defs,
                program: &lir_program,
              },
              BackendRequest::Lowered(LoweredBackendRequest::EmitCombined {
                namespaces: &semantic.namespaces,
                symbols: &sym_table,
                headers: &link_plan.headers,
              }),
            ) {
              Ok(contents) => contents,
              Err(stage_error) => {
                cmd_fail!(&config, "Build failed", start.elapsed());
                eprintln!("{} {}", "Error:".red().bold(), stage_error);
                return Err(());
              },
            }
          };

          if dump_c_requested {
            write_dump_output(&config, "dump-c.c", &c_code)?;
          }

          let c_path = if let Some(path) = &bc.emit_c {
            PathBuf::from(path)
          } else {
            let output_dir = Path::new(&bc.output_dir);
            if !output_dir.exists()
              && let Err(e) = std::fs::create_dir_all(output_dir)
            {
              cmd_fail!(&config, "Build failed", start.elapsed());
              eprintln!(
                "{} Failed to create output directory '{}': {}",
                "Error:".red().bold(),
                bc.output_dir,
                e
              );
              return Err(());
            }
            output_dir.join(format!("{}.c", base_name))
          };

          let should_write_c =
            bc.emit_c.is_some() || !check_mode || bc.emit_obj.is_some() || bc.emit_bin.is_some() || bc.lib;
          if should_write_c {
            if let Err(e) = std::fs::write(&c_path, &c_code) {
              cmd_fail!(&config, "Build failed", start.elapsed());
              eprintln!("{} Failed to write C file '{}': {}", "Error:".red().bold(), c_path.display(), e);
              return Err(());
            }

            if (bc.emit_c.is_some() || !check_mode) && ignis_log::show_verbose(&config) {
              eprintln!("    {} Emitted C code to {}", "-->".bright_green().bold(), c_path.display());
            }
          }

          if (bc.emit_obj.is_some() || bc.emit_bin.is_some() || bc.lib) && !check_mode {
            let obj_path = if let Some(path) = &bc.emit_obj {
              PathBuf::from(path)
            } else {
              PathBuf::from(format!("{}/{}.o", bc.output_dir, base_name))
            };

            trace_dbg!(&config, DebugTrace::Link, "compiling object file");

            let suppress_link_logs = !ignis_log::show_verbose(&config);
            if let Err(e) = compile_to_object(&c_path, &obj_path, &link_plan, suppress_link_logs) {
              cmd_fail!(&config, "Build failed", start.elapsed());
              eprintln!("{} {}", "Error:".red().bold(), e);
              return Err(());
            }

            if let Some(bin_path) = &bc.emit_bin {
              trace_dbg!(&config, DebugTrace::Link, "linking executable");

              if let Err(e) = link_executable(&obj_path, Path::new(bin_path), &link_plan, suppress_link_logs) {
                cmd_fail!(&config, "Build failed", start.elapsed());
                eprintln!("{} {}", "Error:".red().bold(), e);
                return Err(());
              }

              cmd_ok!(&config, "Compiled", start.elapsed());
              cmd_artifact!(&config, "Binary", bin_path);
              return Ok(());
            }
          }
        }
      }
    }
  }

  if is_check_mode {
    cmd_ok!(&config, "No errors found", start.elapsed());
  } else {
    cmd_ok!(&config, "Compiled", start.elapsed());
  }

  Ok(())
}

fn load_manifest(std_path: &Path) -> IgnisSTDManifest {
  let manifest_path = std_path.join("manifest.toml");

  std::fs::read_to_string(&manifest_path)
    .ok()
    .and_then(|content| toml::from_str(&content).ok())
    .unwrap_or_default()
}

fn build_test_driver_config(project_root: &Path) -> Result<(Arc<IgnisConfig>, crate::project::Project), ()> {
  let toml_path = project_root.join("ignis.toml");
  let project_toml = load_project_toml(&toml_path).map_err(|error| {
    eprintln!("{} {}", "Error:".red().bold(), error);
  })?;

  let project =
    resolve_project(project_root.to_path_buf(), project_toml, &CliOverrides::default()).map_err(|error| {
      eprintln!("{} {}", "Error:".red().bold(), error);
    })?;

  let mut config = IgnisConfig::new_basic(false, Vec::new(), false, 0);

  if let Some(std_path) = &project.std_path {
    config.std_path = std_path.to_string_lossy().to_string();
    config.manifest = load_manifest(std_path);
  }

  config.project_config = Some(IgnisProjectConfig::new(
    project.name.clone(),
    project.version.clone(),
    Vec::new(),
    String::new(),
    Vec::new(),
    String::new(),
    String::new(),
    project.target,
    project.entry.to_string_lossy().to_string(),
    project.out_dir.to_string_lossy().to_string(),
    project.source_dir.to_string_lossy().to_string(),
    project.opt_level > 0,
    config.std_path.clone(),
    project.std_path.is_some(),
  ));
  config.std = project.std_path.is_some();
  config.auto_load_std = project.std_path.is_some();
  config.test = true;
  config.c_compiler = project.cc.clone();
  config.cflags = project.cflags.clone();
  config.aliases = project.aliases.clone();
  config.build_config = Some(IgnisBuildConfig::new(
    Some(project.entry.to_string_lossy().to_string()),
    project.target,
    true,
    project.opt_level > 0,
    project.out_dir.to_string_lossy().to_string(),
    Vec::new(),
    None,
    None,
    None,
    None,
    None,
    false,
    project.bin,
    false,
    true,
    true,
  ));

  Ok((Arc::new(config), project))
}

fn resolve_test_std_path(std_path_override: Option<&Path>) -> String {
  std_path_override
    .map(|path| path.to_string_lossy().into_owned())
    .or_else(|| std::env::var("IGNIS_STD_PATH").ok())
    .unwrap_or_default()
}

fn is_project_test_file_name(file_name: &str) -> bool {
  file_name == "tests.ign"
    || file_name.ends_with(".test.ign")
    || file_name.ends_with(".tests.ign")
    || file_name.ends_with("-test.ign")
    || file_name.ends_with("-tests.ign")
    || file_name.ends_with("_test.ign")
    || file_name.ends_with("_tests.ign")
}

fn collect_project_test_sources(
  current_dir: &Path,
  discovered: &mut Vec<PathBuf>,
) {
  let Ok(entries) = std::fs::read_dir(current_dir) else {
    return;
  };

  let mut entry_paths: Vec<PathBuf> = entries
    .filter_map(|entry| entry.ok().map(|entry| entry.path()))
    .collect();
  entry_paths.sort();

  for entry_path in entry_paths {
    if entry_path.is_dir() {
      collect_project_test_sources(&entry_path, discovered);
      continue;
    }

    let Some(file_name) = entry_path.file_name().and_then(|file_name| file_name.to_str()) else {
      continue;
    };

    if is_project_test_file_name(file_name) {
      discovered.push(entry_path);
    }
  }
}

fn discover_project_test_sources(
  source_dir: &Path,
  entry_path: &Path,
) -> Vec<PathBuf> {
  let mut discovered = Vec::new();
  collect_project_test_sources(source_dir, &mut discovered);
  discovered.retain(|path| path != entry_path);
  discovered.sort();
  discovered.dedup();
  discovered
}

fn discover_project_test_modules(
  ctx: &mut CompilationContext,
  entry_path: &Path,
  source_dir: &Path,
  config: &IgnisConfig,
) -> Result<ModuleId, ()> {
  let root_id = ctx.discover_modules(entry_path.to_string_lossy().as_ref(), config)?;

  for test_source in discover_project_test_sources(source_dir, entry_path) {
    ctx.discover_modules(test_source.to_string_lossy().as_ref(), config)?;
  }

  Ok(root_id)
}

fn build_single_file_test_driver_input(
  file_path: &Path,
  std_path_override: Option<&Path>,
) -> Result<TestDriverInput, ()> {
  if !file_path.exists() {
    eprintln!("{} File not found: '{}'", "Error:".red().bold(), file_path.display());
    return Err(());
  }

  let source_dir = file_path
    .parent()
    .map(Path::to_path_buf)
    .unwrap_or_else(|| PathBuf::from("."));
  let out_dir = source_dir.join("build");
  let stem = file_path.file_stem().and_then(|stem| stem.to_str()).unwrap_or("out");
  let raw_std_path = resolve_test_std_path(std_path_override);
  let std_path = if raw_std_path.is_empty() {
    raw_std_path
  } else {
    let candidate = PathBuf::from(&raw_std_path);
    candidate
      .canonicalize()
      .unwrap_or(candidate)
      .to_string_lossy()
      .into_owned()
  };
  let std_enabled = !std_path.is_empty();

  let mut config = IgnisConfig::new_basic(false, Vec::new(), false, 0);
  config.std_path = std_path.clone();
  config.std = std_enabled;
  config.auto_load_std = std_enabled;
  config.manifest = load_manifest(Path::new(&std_path));
  config.test = true;
  config.c_compiler = "cc".to_string();
  config.cflags = Vec::new();
  config.project_config = Some(IgnisProjectConfig::new(
    stem.to_string(),
    COMPILER_VERSION.to_string(),
    Vec::new(),
    String::new(),
    Vec::new(),
    String::new(),
    String::new(),
    TargetBackend::C,
    file_path.to_string_lossy().to_string(),
    out_dir.to_string_lossy().to_string(),
    source_dir.to_string_lossy().to_string(),
    false,
    std_path,
    std_enabled,
  ));
  config.build_config = Some(IgnisBuildConfig::new(
    Some(file_path.to_string_lossy().to_string()),
    TargetBackend::C,
    false,
    false,
    out_dir.to_string_lossy().to_string(),
    Vec::new(),
    None,
    None,
    None,
    None,
    Some(
      out_dir
        .join("bin")
        .join(format!("{}-tests", stem))
        .to_string_lossy()
        .to_string(),
    ),
    false,
    true,
    false,
    true,
    true,
  ));

  let layout = BuildLayout::with_project_root(stem, &out_dir, &source_dir);

  Ok(TestDriverInput {
    config: Arc::new(config),
    entry_path: file_path.to_path_buf(),
    source_dir,
    out_dir: out_dir.clone(),
    layout,
    harness_c_path: out_dir.join(format!("{}-tests-harness.c", stem)),
    bin_path: out_dir.join("bin").join(format!("{}-tests", stem)),
  })
}

fn build_std_test_driver_input(
  std_root: &Path,
  output_dir: Option<&Path>,
) -> Result<TestDriverInput, ()> {
  if !std_root.exists() {
    eprintln!("{} std root '{}' does not exist", "Error:".red().bold(), std_root.display());
    return Err(());
  }

  let std_root = std_root.canonicalize().unwrap_or_else(|_| std_root.to_path_buf());
  let out_dir = output_dir
    .map(Path::to_path_buf)
    .unwrap_or_else(|| PathBuf::from("build"));

  let mut config = IgnisConfig::new_basic(false, Vec::new(), false, 0);
  config.std_path = std_root.to_string_lossy().into_owned();
  config.std = true;
  config.auto_load_std = true;
  config.manifest = load_manifest(&std_root);
  config.test = true;
  config.c_compiler = "cc".to_string();
  config.cflags = Vec::new();
  config.project_config = Some(IgnisProjectConfig::new(
    "std".to_string(),
    COMPILER_VERSION.to_string(),
    Vec::new(),
    String::new(),
    Vec::new(),
    String::new(),
    String::new(),
    TargetBackend::C,
    std_root.to_string_lossy().to_string(),
    out_dir.to_string_lossy().to_string(),
    std_root.to_string_lossy().to_string(),
    false,
    std_root.to_string_lossy().to_string(),
    true,
  ));
  config.build_config = Some(IgnisBuildConfig::new(
    Some(std_root.to_string_lossy().to_string()),
    TargetBackend::C,
    true,
    false,
    out_dir.to_string_lossy().to_string(),
    Vec::new(),
    None,
    None,
    None,
    None,
    Some(out_dir.join("bin/std-tests").to_string_lossy().to_string()),
    false,
    true,
    false,
    true,
    true,
  ));

  let layout = BuildLayout::with_project_root("std", &out_dir, &std_root);

  Ok(TestDriverInput {
    config: Arc::new(config),
    entry_path: std_root.clone(),
    source_dir: std_root,
    out_dir: out_dir.clone(),
    layout,
    harness_c_path: out_dir.join("std-tests-harness.c"),
    bin_path: out_dir.join("bin/std-tests"),
  })
}

fn module_segments(
  module_path: &ModulePath,
  source_dir: &Path,
) -> Vec<String> {
  match module_path {
    ModulePath::Std(name) => name.split("::").map(str::to_string).collect(),
    ModulePath::Project(path) => {
      let relative = path.strip_prefix(source_dir).unwrap_or(path.as_path());
      let mut segments: Vec<String> = relative
        .iter()
        .map(|segment| segment.to_string_lossy().to_string())
        .collect();

      if let Some(last) = segments.last_mut()
        && let Some(file_stem) = last.strip_suffix(".ign")
      {
        *last = file_stem.to_string();
      }

      if matches!(segments.last(), Some(last) if last == "mod") {
        segments.pop();
      }

      segments
    },
  }
}

fn format_test_name(
  definition_id: DefinitionId,
  defs: &DefinitionStore,
  namespaces: &ignis_type::namespace::NamespaceStore,
  symbols: &SymbolTable,
  module_paths: &HashMap<ModuleId, ModulePath>,
  source_dir: &Path,
) -> Option<String> {
  let definition = defs.get(&definition_id);
  let mut segments = module_paths
    .get(&definition.owner_module)
    .map(|module_path| module_segments(module_path, source_dir))?;

  if let Some(namespace_id) = definition.owner_namespace {
    segments.extend(
      namespaces
        .full_path(namespace_id)
        .into_iter()
        .map(|symbol_id| symbols.get(&symbol_id).to_string()),
    );
  }

  segments.push(symbols.get(&definition.name).to_string());

  Some(segments.join("::"))
}

fn discover_test_cases(
  defs: &DefinitionStore,
  namespaces: &ignis_type::namespace::NamespaceStore,
  symbols: &SymbolTable,
  module_paths: &HashMap<ModuleId, ModulePath>,
  source_dir: &Path,
) -> Vec<crate::backend::TestCase> {
  defs
    .iter()
    .filter_map(|(definition_id, definition)| {
      let DefinitionKind::Function(function) = &definition.kind else {
        return None;
      };

      if !function.has_test_attr() {
        return None;
      }

      let ModulePath::Project(source_path) = module_paths.get(&definition.owner_module)? else {
        return None;
      };

      format_test_name(definition_id, defs, namespaces, symbols, module_paths, source_dir).map(|fq_name| {
        crate::backend::TestCase {
          def_id: definition_id,
          fq_name,
          source_path: source_path.clone(),
        }
      })
    })
    .collect()
}

fn discover_std_test_companions(
  std_root: &Path,
  manifest: &IgnisSTDManifest,
) -> Vec<PathBuf> {
  let mut module_names: Vec<&str> = manifest.modules.keys().map(|name| name.as_str()).collect();
  module_names.sort();

  let mut discovered = Vec::new();

  for module_name in module_names {
    let Some(module_path) = manifest.modules.get(module_name) else {
      continue;
    };

    let companion_path = std_root.join(module_path).with_file_name("tests.ign");
    if companion_path.exists() {
      discovered.push(companion_path);
    }
  }

  discovered
}

fn validate_std_test_layout(
  defs: &DefinitionStore,
  module_paths: &HashMap<ModuleId, ModulePath>,
) -> Result<(), String> {
  for (_, definition) in defs.iter() {
    let DefinitionKind::Function(function) = &definition.kind else {
      continue;
    };

    if !function.has_test_attr() {
      continue;
    }

    if let Some(ModulePath::Std(module_name)) = module_paths.get(&definition.owner_module) {
      return Err(format!(
        "std::{} contains @test in a shipping source module; move std tests into the sibling tests.ign file",
        module_name
      ));
    }
  }

  Ok(())
}

fn discover_std_test_cases(
  defs: &DefinitionStore,
  namespaces: &ignis_type::namespace::NamespaceStore,
  symbols: &SymbolTable,
  module_paths: &HashMap<ModuleId, ModulePath>,
  source_dir: &Path,
  allowed_sources: &HashSet<PathBuf>,
) -> Vec<crate::backend::TestCase> {
  defs
    .iter()
    .filter_map(|(definition_id, definition)| {
      let DefinitionKind::Function(function) = &definition.kind else {
        return None;
      };

      if !function.has_test_attr() {
        return None;
      }

      let ModulePath::Project(source_path) = module_paths.get(&definition.owner_module)? else {
        return None;
      };

      if !allowed_sources.contains(source_path) {
        return None;
      }

      format_test_name(definition_id, defs, namespaces, symbols, module_paths, source_dir).map(|fq_name| {
        crate::backend::TestCase {
          def_id: definition_id,
          fq_name,
          source_path: source_path.clone(),
        }
      })
    })
    .collect()
}

fn build_test_harness_plan(
  mut discovered_tests: Vec<crate::backend::TestCase>,
  filter: Option<&str>,
) -> crate::backend::TestHarnessPlan {
  discovered_tests.sort_by(|left, right| left.fq_name.cmp(&right.fq_name));

  if let Some(filter) = filter {
    discovered_tests.retain(|test| test.fq_name.contains(filter));
  }

  crate::backend::TestHarnessPlan {
    tests: discovered_tests,
  }
}

#[cfg(test)]
fn plan_project_tests_for_snapshot(
  project_root: &Path,
  filter: Option<&str>,
) -> Result<crate::backend::TestHarnessPlan, ()> {
  let (config, project) = build_test_driver_config(project_root)?;
  let mut ctx = CompilationContext::new(&config);
  let root_id = discover_project_test_modules(&mut ctx, &project.entry, &project.source_dir, &config)?;

  if config.std && config.auto_load_std {
    ctx.discover_prelude_modules_for_all(&config);
  }

  let (output, has_errors) = ctx.compile_collect_all(root_id, &config)?;

  if has_errors {
    return Err(());
  }

  let module_paths = build_module_paths_from_graph(&ctx.module_graph);
  let discovered_tests = {
    let symbols = output.symbols.borrow();
    discover_test_cases(&output.defs, &output.namespaces, &symbols, &module_paths, &project.source_dir)
  };

  Ok(build_test_harness_plan(discovered_tests, filter))
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct TestExecutionResult {
  fq_name: String,
  success: bool,
  exit_code: i32,
  stdout: String,
  stderr: String,
}

fn collect_test_mono_roots(plan: &crate::backend::TestHarnessPlan) -> Vec<DefinitionId> {
  plan.tests.iter().map(|test| test.def_id).collect()
}

fn test_harness_paths(project: &crate::project::Project) -> (PathBuf, PathBuf) {
  let layout = BuildLayout::with_project_root(&project.name, &project.out_dir, &project.root);
  let bin_path = layout.bin_dir().join(format!("{}-tests", project.name));
  let harness_c_path = project.out_dir.join(format!("{}-tests-harness.c", project.name));
  (harness_c_path, bin_path)
}

fn execute_test_harness_binary(
  binary_path: &Path,
  plan: &crate::backend::TestHarnessPlan,
  update_snapshots: bool,
) -> Result<Vec<TestExecutionResult>, String> {
  let mut results = Vec::with_capacity(plan.tests.len());

  for test in &plan.tests {
    let snapshot_dir = test
      .source_path
      .parent()
      .map(|parent| parent.join("__snapshots__"))
      .ok_or_else(|| format!("Test '{}' has no parent module directory", test.fq_name))?;

    let output = Command::new(binary_path)
      .arg("--ignis-test")
      .arg(&test.fq_name)
      .env("IGNIS_TEST_NAME", &test.fq_name)
      .env("IGNIS_TEST_SNAPSHOT_DIR", &snapshot_dir)
      .env("IGNIS_TEST_UPDATE_SNAPSHOTS", if update_snapshots { "1" } else { "0" })
      .output()
      .map_err(|error| format!("Failed to run test '{}': {}", test.fq_name, error))?;

    results.push(TestExecutionResult {
      fq_name: test.fq_name.clone(),
      success: output.status.success(),
      exit_code: output.status.code().unwrap_or(1),
      stdout: String::from_utf8_lossy(&output.stdout).to_string(),
      stderr: String::from_utf8_lossy(&output.stderr).to_string(),
    });
  }

  Ok(results)
}

#[cfg(test)]
fn format_test_plan_snapshot(plan: &crate::backend::TestHarnessPlan) -> String {
  let mut lines = vec![format!("selected: {}", plan.tests.len())];

  for test in &plan.tests {
    lines.push(format!("- {}", test.fq_name));
  }

  lines.join("\n")
}

const FAILED_TEST_OUTPUT_LIMIT: usize = 240;

fn truncate_failed_test_output(output: &str) -> (String, bool) {
  let trimmed = output.trim_end();

  if trimmed.chars().count() <= FAILED_TEST_OUTPUT_LIMIT {
    return (trimmed.to_string(), false);
  }

  let truncated: String = trimmed.chars().take(FAILED_TEST_OUTPUT_LIMIT).collect();
  (truncated, true)
}

fn push_failed_test_stream(
  lines: &mut Vec<String>,
  label: &str,
  output: &str,
) {
  if output.trim().is_empty() {
    return;
  }

  let (display, was_truncated) = truncate_failed_test_output(output);

  lines.push(format!("  {}:", label));
  lines.extend(display.lines().map(|line| format!("    {}", line)));

  if was_truncated {
    lines.push("    [truncated]".to_string());
  }
}

fn format_failed_test_details(result: &TestExecutionResult) -> String {
  let mut lines = vec![format!("- {} (exit code {})", result.fq_name, result.exit_code)];

  push_failed_test_stream(&mut lines, "stderr", &result.stderr);
  push_failed_test_stream(&mut lines, "stdout", &result.stdout);

  lines.join("\n")
}

#[cfg(test)]
fn format_test_summary_snapshot(results: &[TestExecutionResult]) -> String {
  let total = results.len();
  let passed = results.iter().filter(|result| result.success).count();
  let failed = total - passed;

  let mut lines = Vec::with_capacity(results.len() + 4);

  for result in results {
    let status = if result.success { "ok" } else { "FAILED" };
    lines.push(format!("- {} ... {} ({})", result.fq_name, status, result.exit_code));
  }

  lines.push(String::new());

  let failed_results: Vec<_> = results.iter().filter(|result| !result.success).collect();
  if !failed_results.is_empty() {
    lines.push("failures:".to_string());

    for result in failed_results {
      lines.push(format_failed_test_details(result));
    }

    lines.push(String::new());
  }

  lines.push(format!("total: {}", total));
  lines.push(format!("passed: {}", passed));
  lines.push(format!("failed: {}", failed));

  lines.join("\n")
}

pub fn run_project_tests(
  project_root: &Path,
  filter: Option<&str>,
  update_snapshots: bool,
) -> Result<(), ()> {
  let start = Instant::now();
  let (config, project) = build_test_driver_config(project_root)?;

  cmd_header!(&config, "Testing", project.entry.display());
  section!(&config, "Scanning & parsing");

  let mut ctx = CompilationContext::new(&config);
  let root_id =
    discover_project_test_modules(&mut ctx, &project.entry, &project.source_dir, &config).map_err(|()| {
      cmd_fail!(&config, "Test setup failed", start.elapsed());
    })?;

  if config.std && config.auto_load_std {
    ctx.discover_prelude_modules_for_all(&config);
  }

  section!(&config, "Analyzing");

  let (output, has_errors) = ctx.compile_collect_all(root_id, &config)?;

  if has_errors {
    cmd_fail!(&config, "Test setup failed", start.elapsed());
    return Err(());
  }

  let module_paths = build_module_paths_from_graph(&ctx.module_graph);
  let discovered_tests = {
    let symbols = output.symbols.borrow();
    discover_test_cases(&output.defs, &output.namespaces, &symbols, &module_paths, &project.source_dir)
  };
  let plan = build_test_harness_plan(discovered_tests, filter);

  section!(&config, "Planning");
  if plan.tests.is_empty() {
    section_item!(&config, "No tests selected");
    cmd_ok!(&config, "No tests selected", start.elapsed());
    return Ok(());
  }

  for test in &plan.tests {
    section_item!(&config, "{}", test.fq_name);
  }

  section!(&config, "Codegen & linking");

  let used_modules = ctx.module_graph.all_modules_topological();
  let mut test_module_ids = HashSet::new();
  for test in &plan.tests {
    let owner_module = output.defs.get(&test.def_id).owner_module;
    test_module_ids.insert(owner_module);

    for dep_id in ctx.module_graph.transitive_deps(owner_module) {
      test_module_ids.insert(dep_id);
    }
  }

  if ensure_std_built(&used_modules, &ctx.module_graph, &config).is_err() {
    cmd_fail!(&config, "Test setup failed", start.elapsed());
    return Err(());
  }

  let manifest = if config.manifest.modules.is_empty() {
    None
  } else {
    Some(&config.manifest)
  };
  let build_dir = config
    .build_config
    .as_ref()
    .map(|build_config| build_config.output_dir.clone())
    .unwrap_or_else(|| "build".to_string());

  // `test-std` exercises the shipped std modules themselves. Reusing a cached
  // std archive compiled by an earlier compiler build can leave this command
  // running stale code even after lowering/codegen fixes land in the workspace.
  // Rebuild the std archive for each std-test invocation so the direct CLI path
  // and the temp-output test path observe the same fresh compiler output.
  if build_std(config.clone(), &build_dir).is_err() {
    cmd_fail!(&config, "Test setup failed", start.elapsed());
    return Err(());
  }

  let mut link_plan = LinkPlan::from_modules(
    &used_modules,
    &ctx.module_graph,
    Path::new(&config.std_path),
    Path::new(&build_dir),
    manifest,
  );
  link_plan.cc = config.c_compiler.clone();
  link_plan.cflags = config.cflags.clone();
  let std_defined_symbols = match &link_plan.std_archive {
    Some(archive_path) => match collect_archive_defined_symbols(archive_path) {
      Ok(symbols) => Some(symbols),
      Err(error) => {
        cmd_fail!(&config, "Test setup failed", start.elapsed());
        eprintln!("{} {}", "Error:".red().bold(), error);
        return Err(());
      },
    },
    None => None,
  };

  let mut types = output.types.clone();
  let mono_output = ignis_analyzer::mono::Monomorphizer::new(
    &output.hir,
    &output.defs,
    &output.namespaces,
    &mut types,
    output.symbols.clone(),
  )
  .run(&collect_mono_roots_for_std(&output.defs));

  #[cfg(debug_assertions)]
  mono_output.verify_no_generics(&types);

  let sym_table = output.symbols.borrow();

  let ownership_checker =
    ignis_analyzer::HirOwnershipChecker::new(&mono_output.hir, &types, &mono_output.defs, &sym_table)
      .with_source_map(&ctx.source_map);
  let (drop_schedules, ownership_diagnostics) = ownership_checker.check();

  let borrow_checker = ignis_analyzer::HirBorrowChecker::new(&mono_output.hir, &types, &mono_output.defs, &sym_table)
    .with_source_map(&ctx.source_map);
  let borrow_diagnostics = borrow_checker.check();

  for diagnostic in ownership_diagnostics.iter().chain(borrow_diagnostics.iter()) {
    ignis_diagnostics::render(diagnostic, &ctx.source_map);
  }

  if ownership_diagnostics
    .iter()
    .chain(borrow_diagnostics.iter())
    .any(|diagnostic| matches!(diagnostic.severity, ignis_diagnostics::diagnostic_report::Severity::Error))
  {
    cmd_fail!(&config, "Test setup failed", start.elapsed());
    return Err(());
  }

  let used_module_set: std::collections::HashSet<ModuleId> = used_modules.iter().copied().collect();
  let (lir_program, verify_result) = ignis_lir::lowering::lower_and_verify(
    &mono_output.hir,
    &mut types,
    &mono_output.defs,
    &sym_table,
    &drop_schedules,
    Some(&used_module_set),
  );

  if let Err(errors) = &verify_result {
    eprintln!("{} Cannot execute tests: LIR verification failed", "Error:".red().bold());
    for error in errors {
      eprintln!("  {:?}", error);
    }
    cmd_fail!(&config, "Test setup failed", start.elapsed());
    return Err(());
  }

  let selected_backend = select_backend_or_report(&config, "Test setup failed", start)?;
  let module_paths = build_module_paths_from_graph(&ctx.module_graph);

  let layout = BuildLayout::with_project_root(&project.name, &project.out_dir, &project.root);

  if let Err(error) = layout.create_user_dirs() {
    cmd_fail!(&config, "Test setup failed", start.elapsed());
    eprintln!("{} Failed to create user build directories: {}", "Error:".red().bold(), error);
    return Err(());
  }

  let std_dir = ctx.module_graph.std_path();
  let user_modules: Vec<_> = used_modules
    .iter()
    .filter(|module_id| {
      let module = ctx.module_graph.modules.get(module_id);
      test_module_ids.contains(module_id) && module.path.is_project() && !module.path.is_inside_dir(std_dir)
    })
    .collect();

  if user_modules.is_empty() {
    cmd_fail!(&config, "Test setup failed", start.elapsed());
    eprintln!("{} No user modules found", "Error:".red().bold());
    return Err(());
  }

  for module_id in &user_modules {
    let module = ctx.module_graph.modules.get(module_id);
    let source_path = match &module.path {
      ModulePath::Project(path) => path.clone(),
      _ => continue,
    };

    if let Err(error) = layout.ensure_user_module_dirs(&source_path) {
      cmd_fail!(&config, "Test setup failed", start.elapsed());
      eprintln!("{} Failed to create module directories: {}", "Error:".red().bold(), error);
      return Err(());
    }

    let header_content = match emit_text(
      &selected_backend,
      BackendInput::Lowered {
        root_id: **module_id,
        types: &types,
        defs: &mono_output.defs,
        program: &lir_program,
      },
      BackendRequest::Header(HeaderBackendRequest::EmitUserModuleHeader {
        module_id: **module_id,
        source_path: &source_path,
        namespaces: &output.namespaces,
        symbols: &sym_table,
      }),
    ) {
      Ok(contents) => contents,
      Err(stage_error) => {
        cmd_fail!(&config, "Test setup failed", start.elapsed());
        eprintln!("{} {}", "Error:".red().bold(), stage_error);
        return Err(());
      },
    };

    let header_path = layout.user_module_header(&source_path);
    if let Err(error) = std::fs::write(&header_path, &header_content) {
      cmd_fail!(&config, "Test setup failed", start.elapsed());
      eprintln!(
        "{} Failed to write header '{}': {}",
        "Error:".red().bold(),
        header_path.display(),
        error
      );
      return Err(());
    }
  }

  let mut link_plan_with_user_includes = link_plan.clone();
  link_plan_with_user_includes
    .include_dirs
    .push(layout.user_include_dir());

  let mut user_object_paths = Vec::with_capacity(user_modules.len());

  for module_id in &user_modules {
    let module = ctx.module_graph.modules.get(module_id);
    let source_path = match &module.path {
      ModulePath::Project(path) => path.clone(),
      _ => continue,
    };

    let dep_ids = ctx.module_graph.transitive_deps(**module_id);
    let mut user_module_headers = Vec::new();
    let self_header_rel = layout.relativize(&source_path).with_extension("h");
    user_module_headers.push(ignis_config::CHeader {
      path: normalize_path_for_include(&self_header_rel),
      quoted: true,
    });

    for dep_id in &dep_ids {
      let dep_module = ctx.module_graph.modules.get(dep_id);
      if let ModulePath::Project(dep_path) = &dep_module.path {
        if dep_path.starts_with(std_dir) {
          continue;
        }

        let dep_header_rel = layout.relativize(dep_path).with_extension("h");
        user_module_headers.push(ignis_config::CHeader {
          path: normalize_path_for_include(&dep_header_rel),
          quoted: true,
        });
      }
    }

    let module_c_code = match emit_text(
      &selected_backend,
      BackendInput::Lowered {
        root_id,
        types: &types,
        defs: &mono_output.defs,
        program: &lir_program,
      },
      BackendRequest::Lowered(LoweredBackendRequest::EmitUserModule {
        module_id: **module_id,
        namespaces: &output.namespaces,
        symbols: &sym_table,
        headers: &link_plan_with_user_includes.headers,
        module_paths: &module_paths,
        user_module_headers: &user_module_headers,
        std_path: std_dir,
        std_defined_symbols: std_defined_symbols.as_ref(),
      }),
    ) {
      Ok(contents) => contents,
      Err(stage_error) => {
        cmd_fail!(&config, "Test setup failed", start.elapsed());
        eprintln!("{} {}", "Error:".red().bold(), stage_error);
        return Err(());
      },
    };

    let module_c_path = layout.user_module_src(&source_path);
    if let Err(error) = std::fs::write(&module_c_path, &module_c_code) {
      cmd_fail!(&config, "Test setup failed", start.elapsed());
      eprintln!(
        "{} Failed to write module source '{}': {}",
        "Error:".red().bold(),
        module_c_path.display(),
        error
      );
      return Err(());
    }

    let module_object_path = layout.user_module_obj(&source_path);
    if let Err(error) = compile_to_object(
      &module_c_path,
      &module_object_path,
      &link_plan_with_user_includes,
      !ignis_log::show_verbose(&config),
    ) {
      cmd_fail!(&config, "Test setup failed", start.elapsed());
      eprintln!("{} {}", "Error:".red().bold(), error);
      return Err(());
    }

    user_object_paths.push(module_object_path);
  }

  let harness_c_code = match emit_text(
    &selected_backend,
    BackendInput::Lowered {
      root_id,
      types: &types,
      defs: &mono_output.defs,
      program: &lir_program,
    },
    BackendRequest::Lowered(LoweredBackendRequest::EmitUserTestHarness {
      namespaces: &output.namespaces,
      symbols: &sym_table,
      headers: &link_plan.headers,
      module_paths: &module_paths,
      plan: &plan,
    }),
  ) {
    Ok(contents) => contents,
    Err(stage_error) => {
      cmd_fail!(&config, "Test setup failed", start.elapsed());
      eprintln!("{} {}", "Error:".red().bold(), stage_error);
      return Err(());
    },
  };

  let (harness_c_path, bin_path) = test_harness_paths(&project);

  if let Err(error) = std::fs::create_dir_all(&project.out_dir) {
    cmd_fail!(&config, "Test setup failed", start.elapsed());
    eprintln!(
      "{} Failed to create build directory '{}': {}",
      "Error:".red().bold(),
      project.out_dir.display(),
      error
    );
    return Err(());
  }

  if let Some(parent) = bin_path.parent()
    && let Err(error) = std::fs::create_dir_all(parent)
  {
    cmd_fail!(&config, "Test setup failed", start.elapsed());
    eprintln!(
      "{} Failed to create binary directory '{}': {}",
      "Error:".red().bold(),
      parent.display(),
      error
    );
    return Err(());
  }

  if let Err(error) = std::fs::write(&harness_c_path, &harness_c_code) {
    cmd_fail!(&config, "Test setup failed", start.elapsed());
    eprintln!(
      "{} Failed to write harness source '{}': {}",
      "Error:".red().bold(),
      harness_c_path.display(),
      error
    );
    return Err(());
  }

  let harness_object_path = harness_c_path.with_extension("o");
  let suppress_link_logs = !ignis_log::show_verbose(&config);

  if let Err(error) = compile_to_object(&harness_c_path, &harness_object_path, &link_plan, suppress_link_logs) {
    cmd_fail!(&config, "Test setup failed", start.elapsed());
    eprintln!("{} {}", "Error:".red().bold(), error);
    return Err(());
  }

  let mut executable_objects = user_object_paths;
  executable_objects.push(harness_object_path);

  if let Err(error) = link_executable_multi(&executable_objects, &bin_path, &link_plan, suppress_link_logs) {
    cmd_fail!(&config, "Test setup failed", start.elapsed());
    eprintln!("{} {}", "Error:".red().bold(), error);
    return Err(());
  }

  section!(&config, "Running");

  let results = match execute_test_harness_binary(&bin_path, &plan, update_snapshots) {
    Ok(results) => results,
    Err(error) => {
      cmd_fail!(&config, "Test setup failed", start.elapsed());
      eprintln!("{} {}", "Error:".red().bold(), error);
      return Err(());
    },
  };

  let passed = results.iter().filter(|result| result.success).count();
  let total = results.len();
  let failed = total - passed;

  for result in &results {
    let status = if result.success {
      "ok".green()
    } else {
      "FAILED".red().bold()
    };
    section_item!(&config, "{} ... {}", result.fq_name, status);
  }

  let failed_results: Vec<_> = results.iter().filter(|result| !result.success).collect();
  if !failed_results.is_empty() {
    section!(&config, "Failures");

    for result in failed_results {
      for line in format_failed_test_details(result).lines() {
        println!("  {}", line);
      }

      println!();
    }
  }

  section!(&config, "Summary");
  section_item!(&config, "{} total", total);
  section_item!(&config, "{} passed", passed);
  section_item!(&config, "{} failed", failed);
  cmd_artifact!(&config, "Test binary", bin_path.display());

  if failed == 0 {
    cmd_ok!(&config, "Tests passed", start.elapsed());
    Ok(())
  } else {
    cmd_fail!(&config, "Tests failed", start.elapsed());
    Err(())
  }
}

pub fn run_single_file_tests(
  file_path: &Path,
  filter: Option<&str>,
  update_snapshots: bool,
  std_path_override: Option<&Path>,
) -> Result<(), ()> {
  let start = Instant::now();
  let input = build_single_file_test_driver_input(file_path, std_path_override)?;
  let config = input.config.clone();

  cmd_header!(&config, "Testing", input.entry_path.display());
  section!(&config, "Scanning & parsing");

  let mut ctx = CompilationContext::new(&config);
  let root_id = ctx
    .discover_modules(input.entry_path.to_string_lossy().as_ref(), &config)
    .map_err(|()| {
      cmd_fail!(&config, "Test setup failed", start.elapsed());
    })?;

  if config.std && config.auto_load_std {
    ctx.discover_prelude_modules_for_all(&config);
  }

  section!(&config, "Analyzing");

  let (output, has_errors) = ctx.compile_collect_all(root_id, &config)?;

  if has_errors {
    cmd_fail!(&config, "Test setup failed", start.elapsed());
    return Err(());
  }

  let module_paths = build_module_paths_from_graph(&ctx.module_graph);
  let discovered_tests = {
    let symbols = output.symbols.borrow();
    discover_test_cases(&output.defs, &output.namespaces, &symbols, &module_paths, &input.source_dir)
  };
  let plan = build_test_harness_plan(discovered_tests, filter);

  section!(&config, "Planning");
  if plan.tests.is_empty() {
    section_item!(&config, "No tests selected");
    cmd_ok!(&config, "No tests selected", start.elapsed());
    return Ok(());
  }

  for test in &plan.tests {
    section_item!(&config, "{}", test.fq_name);
  }

  section!(&config, "Codegen & linking");

  let used_modules = ctx.module_graph.topological_sort();

  if ensure_std_built(&used_modules, &ctx.module_graph, &config).is_err() {
    cmd_fail!(&config, "Test setup failed", start.elapsed());
    return Err(());
  }

  let manifest = if config.manifest.modules.is_empty() {
    None
  } else {
    Some(&config.manifest)
  };
  let build_dir = config
    .build_config
    .as_ref()
    .map(|build_config| build_config.output_dir.clone())
    .unwrap_or_else(|| "build".to_string());
  let mut link_plan = LinkPlan::from_modules(
    &used_modules,
    &ctx.module_graph,
    Path::new(&config.std_path),
    Path::new(&build_dir),
    manifest,
  );
  link_plan.cc = config.c_compiler.clone();
  link_plan.cflags = config.cflags.clone();
  let std_defined_symbols = match &link_plan.std_archive {
    Some(archive_path) => match collect_archive_defined_symbols(archive_path) {
      Ok(symbols) => Some(symbols),
      Err(error) => {
        cmd_fail!(&config, "Test setup failed", start.elapsed());
        eprintln!("{} {}", "Error:".red().bold(), error);
        return Err(());
      },
    },
    None => None,
  };

  let mut types = output.types.clone();
  let mono_output = ignis_analyzer::mono::Monomorphizer::new(
    &output.hir,
    &output.defs,
    &output.namespaces,
    &mut types,
    output.symbols.clone(),
  )
  .run(&collect_test_mono_roots(&plan));

  #[cfg(debug_assertions)]
  mono_output.verify_no_generics(&types);

  let sym_table = output.symbols.borrow();

  let ownership_checker =
    ignis_analyzer::HirOwnershipChecker::new(&mono_output.hir, &types, &mono_output.defs, &sym_table)
      .with_source_map(&ctx.source_map);
  let (drop_schedules, ownership_diagnostics) = ownership_checker.check();

  let borrow_checker = ignis_analyzer::HirBorrowChecker::new(&mono_output.hir, &types, &mono_output.defs, &sym_table)
    .with_source_map(&ctx.source_map);
  let borrow_diagnostics = borrow_checker.check();

  for diagnostic in ownership_diagnostics.iter().chain(borrow_diagnostics.iter()) {
    ignis_diagnostics::render(diagnostic, &ctx.source_map);
  }

  if ownership_diagnostics
    .iter()
    .chain(borrow_diagnostics.iter())
    .any(|diagnostic| matches!(diagnostic.severity, ignis_diagnostics::diagnostic_report::Severity::Error))
  {
    cmd_fail!(&config, "Test setup failed", start.elapsed());
    return Err(());
  }

  let used_module_set: std::collections::HashSet<ModuleId> = used_modules.iter().copied().collect();
  let (lir_program, verify_result) = ignis_lir::lowering::lower_and_verify(
    &mono_output.hir,
    &mut types,
    &mono_output.defs,
    &sym_table,
    &drop_schedules,
    Some(&used_module_set),
  );

  if let Err(errors) = &verify_result {
    eprintln!("{} Cannot execute tests: LIR verification failed", "Error:".red().bold());
    for error in errors {
      eprintln!("  {:?}", error);
    }
    cmd_fail!(&config, "Test setup failed", start.elapsed());
    return Err(());
  }

  let selected_backend = select_backend_or_report(&config, "Test setup failed", start)?;
  let module_paths = build_module_paths_from_graph(&ctx.module_graph);

  if let Err(error) = input.layout.create_user_dirs() {
    cmd_fail!(&config, "Test setup failed", start.elapsed());
    eprintln!("{} Failed to create user build directories: {}", "Error:".red().bold(), error);
    return Err(());
  }

  let std_dir = ctx.module_graph.std_path();
  let user_modules: Vec<_> = used_modules
    .iter()
    .filter(|module_id| {
      let module = ctx.module_graph.modules.get(module_id);
      module.path.is_project() && !module.path.is_inside_dir(std_dir)
    })
    .collect();

  if user_modules.is_empty() {
    cmd_fail!(&config, "Test setup failed", start.elapsed());
    eprintln!("{} No user modules found", "Error:".red().bold());
    return Err(());
  }

  for module_id in &user_modules {
    let module = ctx.module_graph.modules.get(module_id);
    let source_path = match &module.path {
      ModulePath::Project(path) => path.clone(),
      _ => continue,
    };

    if let Err(error) = input.layout.ensure_user_module_dirs(&source_path) {
      cmd_fail!(&config, "Test setup failed", start.elapsed());
      eprintln!("{} Failed to create module directories: {}", "Error:".red().bold(), error);
      return Err(());
    }

    let header_content = match emit_text(
      &selected_backend,
      BackendInput::Lowered {
        root_id,
        types: &types,
        defs: &mono_output.defs,
        program: &lir_program,
      },
      BackendRequest::Header(HeaderBackendRequest::EmitUserModuleHeader {
        module_id: **module_id,
        source_path: &source_path,
        namespaces: &output.namespaces,
        symbols: &sym_table,
      }),
    ) {
      Ok(contents) => contents,
      Err(stage_error) => {
        cmd_fail!(&config, "Test setup failed", start.elapsed());
        eprintln!("{} {}", "Error:".red().bold(), stage_error);
        return Err(());
      },
    };

    let header_path = input.layout.user_module_header(&source_path);
    if let Err(error) = std::fs::write(&header_path, &header_content) {
      cmd_fail!(&config, "Test setup failed", start.elapsed());
      eprintln!(
        "{} Failed to write header '{}': {}",
        "Error:".red().bold(),
        header_path.display(),
        error
      );
      return Err(());
    }
  }

  let mut link_plan_with_user_includes = link_plan.clone();
  link_plan_with_user_includes
    .include_dirs
    .push(input.layout.user_include_dir());

  let mut user_object_paths = Vec::with_capacity(user_modules.len());

  for module_id in &user_modules {
    let module = ctx.module_graph.modules.get(module_id);
    let source_path = match &module.path {
      ModulePath::Project(path) => path.clone(),
      _ => continue,
    };

    let dep_ids = ctx.module_graph.transitive_deps(**module_id);
    let mut user_module_headers = Vec::new();
    let self_header_rel = input.layout.relativize(&source_path).with_extension("h");
    user_module_headers.push(ignis_config::CHeader {
      path: normalize_path_for_include(&self_header_rel),
      quoted: true,
    });

    for dep_id in &dep_ids {
      let dep_module = ctx.module_graph.modules.get(dep_id);
      if let ModulePath::Project(dep_path) = &dep_module.path {
        if dep_path.starts_with(std_dir) {
          continue;
        }

        let dep_header_rel = input.layout.relativize(dep_path).with_extension("h");
        user_module_headers.push(ignis_config::CHeader {
          path: normalize_path_for_include(&dep_header_rel),
          quoted: true,
        });
      }
    }

    let module_c_code = match emit_text(
      &selected_backend,
      BackendInput::Lowered {
        root_id,
        types: &types,
        defs: &mono_output.defs,
        program: &lir_program,
      },
      BackendRequest::Lowered(LoweredBackendRequest::EmitUserModule {
        module_id: **module_id,
        namespaces: &output.namespaces,
        symbols: &sym_table,
        headers: &link_plan_with_user_includes.headers,
        module_paths: &module_paths,
        user_module_headers: &user_module_headers,
        std_path: std_dir,
        std_defined_symbols: std_defined_symbols.as_ref(),
      }),
    ) {
      Ok(contents) => contents,
      Err(stage_error) => {
        cmd_fail!(&config, "Test setup failed", start.elapsed());
        eprintln!("{} {}", "Error:".red().bold(), stage_error);
        return Err(());
      },
    };

    let module_c_path = input.layout.user_module_src(&source_path);
    if let Err(error) = std::fs::write(&module_c_path, &module_c_code) {
      cmd_fail!(&config, "Test setup failed", start.elapsed());
      eprintln!(
        "{} Failed to write module source '{}': {}",
        "Error:".red().bold(),
        module_c_path.display(),
        error
      );
      return Err(());
    }

    let module_object_path = input.layout.user_module_obj(&source_path);
    if let Err(error) = compile_to_object(
      &module_c_path,
      &module_object_path,
      &link_plan_with_user_includes,
      !ignis_log::show_verbose(&config),
    ) {
      cmd_fail!(&config, "Test setup failed", start.elapsed());
      eprintln!("{} {}", "Error:".red().bold(), error);
      return Err(());
    }

    user_object_paths.push(module_object_path);
  }

  let harness_c_code = match emit_text(
    &selected_backend,
    BackendInput::Lowered {
      root_id,
      types: &types,
      defs: &mono_output.defs,
      program: &lir_program,
    },
    BackendRequest::Lowered(LoweredBackendRequest::EmitUserTestHarness {
      namespaces: &output.namespaces,
      symbols: &sym_table,
      headers: &link_plan.headers,
      module_paths: &module_paths,
      plan: &plan,
    }),
  ) {
    Ok(contents) => contents,
    Err(stage_error) => {
      cmd_fail!(&config, "Test setup failed", start.elapsed());
      eprintln!("{} {}", "Error:".red().bold(), stage_error);
      return Err(());
    },
  };

  if let Err(error) = std::fs::create_dir_all(&input.out_dir) {
    cmd_fail!(&config, "Test setup failed", start.elapsed());
    eprintln!(
      "{} Failed to create build directory '{}': {}",
      "Error:".red().bold(),
      input.out_dir.display(),
      error
    );
    return Err(());
  }

  if let Some(parent) = input.bin_path.parent()
    && let Err(error) = std::fs::create_dir_all(parent)
  {
    cmd_fail!(&config, "Test setup failed", start.elapsed());
    eprintln!(
      "{} Failed to create binary directory '{}': {}",
      "Error:".red().bold(),
      parent.display(),
      error
    );
    return Err(());
  }

  if let Err(error) = std::fs::write(&input.harness_c_path, &harness_c_code) {
    cmd_fail!(&config, "Test setup failed", start.elapsed());
    eprintln!(
      "{} Failed to write harness source '{}': {}",
      "Error:".red().bold(),
      input.harness_c_path.display(),
      error
    );
    return Err(());
  }

  let harness_object_path = input.harness_c_path.with_extension("o");
  let suppress_link_logs = !ignis_log::show_verbose(&config);

  if let Err(error) = compile_to_object(&input.harness_c_path, &harness_object_path, &link_plan, suppress_link_logs) {
    cmd_fail!(&config, "Test setup failed", start.elapsed());
    eprintln!("{} {}", "Error:".red().bold(), error);
    return Err(());
  }

  let mut executable_objects = user_object_paths;
  executable_objects.push(harness_object_path);

  if let Err(error) = link_executable_multi(&executable_objects, &input.bin_path, &link_plan, suppress_link_logs) {
    cmd_fail!(&config, "Test setup failed", start.elapsed());
    eprintln!("{} {}", "Error:".red().bold(), error);
    return Err(());
  }

  section!(&config, "Running");

  let results = match execute_test_harness_binary(&input.bin_path, &plan, update_snapshots) {
    Ok(results) => results,
    Err(error) => {
      cmd_fail!(&config, "Test setup failed", start.elapsed());
      eprintln!("{} {}", "Error:".red().bold(), error);
      return Err(());
    },
  };

  let passed = results.iter().filter(|result| result.success).count();
  let total = results.len();
  let failed = total - passed;

  for result in &results {
    let status = if result.success {
      "ok".green()
    } else {
      "FAILED".red().bold()
    };
    section_item!(&config, "{} ... {}", result.fq_name, status);
  }

  let failed_results: Vec<_> = results.iter().filter(|result| !result.success).collect();
  if !failed_results.is_empty() {
    section!(&config, "Failures");

    for result in failed_results {
      for line in format_failed_test_details(result).lines() {
        println!("  {}", line);
      }

      println!();
    }
  }

  section!(&config, "Summary");
  section_item!(&config, "{} total", total);
  section_item!(&config, "{} passed", passed);
  section_item!(&config, "{} failed", failed);
  cmd_artifact!(&config, "Test binary", input.bin_path.display());

  if failed == 0 {
    cmd_ok!(&config, "Tests passed", start.elapsed());
    Ok(())
  } else {
    cmd_fail!(&config, "Tests failed", start.elapsed());
    Err(())
  }
}

pub fn run_std_tests(
  std_root: &Path,
  filter: Option<&str>,
  update_snapshots: bool,
  output_dir: Option<&Path>,
) -> Result<(), ()> {
  let start = Instant::now();
  let input = build_std_test_driver_input(std_root, output_dir)?;
  let config = input.config.clone();

  cmd_header!(&config, "Testing std", input.entry_path.display());

  if config.manifest.modules.is_empty() {
    cmd_fail!(&config, "Test setup failed", start.elapsed());
    eprintln!(
      "{} No modules found in std manifest at '{}/manifest.toml'",
      "Error:".red().bold(),
      input.source_dir.display()
    );
    return Err(());
  }

  section!(&config, "Scanning & parsing");

  let mut ctx = CompilationContext::new(&config);
  ctx.discover_all_std_modules(&config);

  let test_sources = discover_std_test_companions(&input.source_dir, &config.manifest);
  let allowed_test_sources: HashSet<PathBuf> = test_sources.iter().cloned().collect();

  for source_path in &test_sources {
    if ctx
      .discover_modules(source_path.to_string_lossy().as_ref(), &config)
      .is_err()
    {
      cmd_fail!(&config, "Test setup failed", start.elapsed());
      return Err(());
    }
  }

  if config.std && config.auto_load_std {
    ctx.discover_prelude_modules_for_all(&config);
  }

  section!(&config, "Analyzing");

  let order = ctx.module_graph.all_modules_topological();
  let (output, has_errors, _) = ctx.analyze_modules_collect_all(&order, &config, true);

  if has_errors {
    cmd_fail!(&config, "Test setup failed", start.elapsed());
    return Err(());
  }

  let module_paths = build_module_paths_from_graph(&ctx.module_graph);
  if let Err(error) = validate_std_test_layout(&output.defs, &module_paths) {
    cmd_fail!(&config, "Test setup failed", start.elapsed());
    eprintln!("{} {}", "Error:".red().bold(), error);
    return Err(());
  }

  let discovered_tests = {
    let symbols = output.symbols.borrow();
    discover_std_test_cases(
      &output.defs,
      &output.namespaces,
      &symbols,
      &module_paths,
      &input.source_dir,
      &allowed_test_sources,
    )
  };
  let plan = build_test_harness_plan(discovered_tests, filter);

  section!(&config, "Planning");
  if plan.tests.is_empty() {
    section_item!(&config, "No tests selected");
    cmd_ok!(&config, "No tests selected", start.elapsed());
    return Ok(());
  }

  for test in &plan.tests {
    section_item!(&config, "{}", test.fq_name);
  }

  section!(&config, "Codegen & linking");

  let used_modules = ctx.module_graph.all_modules_topological();

  let manifest = if config.manifest.modules.is_empty() {
    None
  } else {
    Some(&config.manifest)
  };
  let build_dir = config
    .build_config
    .as_ref()
    .map(|build_config| build_config.output_dir.clone())
    .unwrap_or_else(|| "build".to_string());

  // `test-std` exercises the shipped std modules themselves. Reusing a cached
  // std archive compiled by an earlier compiler build can leave this command
  // running stale code even after lowering/codegen fixes land in the workspace.
  // Rebuild the std archive for each std-test invocation so the direct CLI path
  // and the temp-output test path observe the same fresh compiler output.
  if build_std(config.clone(), &build_dir).is_err() {
    cmd_fail!(&config, "Test setup failed", start.elapsed());
    return Err(());
  }

  let mut link_plan = LinkPlan::from_modules(
    &used_modules,
    &ctx.module_graph,
    Path::new(&config.std_path),
    Path::new(&build_dir),
    manifest,
  );
  link_plan.cc = config.c_compiler.clone();
  link_plan.cflags = config.cflags.clone();
  let mut types = output.types.clone();
  let mono_output = ignis_analyzer::mono::Monomorphizer::new(
    &output.hir,
    &output.defs,
    &output.namespaces,
    &mut types,
    output.symbols.clone(),
  )
  .run(&collect_test_mono_roots(&plan));

  #[cfg(debug_assertions)]
  mono_output.verify_no_generics(&types);

  let sym_table = output.symbols.borrow();

  let ownership_checker =
    ignis_analyzer::HirOwnershipChecker::new(&mono_output.hir, &types, &mono_output.defs, &sym_table)
      .with_source_map(&ctx.source_map);
  let (drop_schedules, ownership_diagnostics) = ownership_checker.check();

  let borrow_checker = ignis_analyzer::HirBorrowChecker::new(&mono_output.hir, &types, &mono_output.defs, &sym_table)
    .with_source_map(&ctx.source_map);
  let borrow_diagnostics = borrow_checker.check();

  for diagnostic in ownership_diagnostics.iter().chain(borrow_diagnostics.iter()) {
    ignis_diagnostics::render(diagnostic, &ctx.source_map);
  }

  if ownership_diagnostics
    .iter()
    .chain(borrow_diagnostics.iter())
    .any(|diagnostic| matches!(diagnostic.severity, ignis_diagnostics::diagnostic_report::Severity::Error))
  {
    cmd_fail!(&config, "Test setup failed", start.elapsed());
    return Err(());
  }

  let used_module_set: std::collections::HashSet<ModuleId> = used_modules.iter().copied().collect();
  let (lir_program, verify_result) = ignis_lir::lowering::lower_and_verify(
    &mono_output.hir,
    &mut types,
    &mono_output.defs,
    &sym_table,
    &drop_schedules,
    Some(&used_module_set),
  );

  if let Err(errors) = &verify_result {
    eprintln!("{} Cannot execute tests: LIR verification failed", "Error:".red().bold());
    for error in errors {
      eprintln!("  {:?}", error);
    }
    cmd_fail!(&config, "Test setup failed", start.elapsed());
    return Err(());
  }

  let selected_backend = select_backend_or_report(&config, "Test setup failed", start)?;

  if let Err(error) = input.layout.create_user_dirs() {
    cmd_fail!(&config, "Test setup failed", start.elapsed());
    eprintln!("{} Failed to create user build directories: {}", "Error:".red().bold(), error);
    return Err(());
  }

  let mut link_plan_with_user_includes = link_plan.clone();
  link_plan_with_user_includes
    .include_dirs
    .push(input.layout.user_include_dir());

  // Symbols already provided by the std archive. Per-module emission needs this so it
  // can force-emit (with internal linkage) std generic specializations that std never
  // instantiated -- e.g. `Vector<u8>::toSlice` over a primitive element type -- which
  // would otherwise be left as an unresolved external reference.
  let std_defined_symbols = match &link_plan.std_archive {
    Some(archive_path) => match collect_archive_defined_symbols(archive_path) {
      Ok(symbols) => Some(symbols),
      Err(error) => {
        cmd_fail!(&config, "Test setup failed", start.elapsed());
        eprintln!("{} {}", "Error:".red().bold(), error);
        return Err(());
      },
    },
    None => None,
  };

  let mut user_object_paths = Vec::new();

  for source_path in &test_sources {
    if !plan.tests.iter().any(|test| &test.source_path == source_path) {
      continue;
    }

    if let Err(error) = input.layout.ensure_user_module_dirs(source_path) {
      cmd_fail!(&config, "Test setup failed", start.elapsed());
      eprintln!("{} Failed to create module directories: {}", "Error:".red().bold(), error);
      return Err(());
    }

    let module_id = ctx
      .module_graph
      .get_by_path(&ModulePath::Project(source_path.clone()))
      .ok_or_else(|| {
        cmd_fail!(&config, "Test setup failed", start.elapsed());
        eprintln!(
          "{} Missing discovered std test module '{}'",
          "Error:".red().bold(),
          source_path.display()
        );
      })?;

    let dep_ids = ctx.module_graph.transitive_deps(module_id);
    let mut user_module_headers = Vec::new();
    let self_header_rel = input.layout.relativize(source_path).with_extension("h");
    user_module_headers.push(ignis_config::CHeader {
      path: normalize_path_for_include(&self_header_rel),
      quoted: true,
    });

    let header_content = match emit_text(
      &selected_backend,
      BackendInput::Lowered {
        root_id: module_id,
        types: &types,
        defs: &mono_output.defs,
        program: &lir_program,
      },
      BackendRequest::Header(HeaderBackendRequest::EmitUserModuleHeader {
        module_id,
        source_path,
        namespaces: &output.namespaces,
        symbols: &sym_table,
      }),
    ) {
      Ok(contents) => contents,
      Err(stage_error) => {
        cmd_fail!(&config, "Test setup failed", start.elapsed());
        eprintln!("{} {}", "Error:".red().bold(), stage_error);
        return Err(());
      },
    };

    let header_path = input.layout.user_module_header(source_path);
    if let Err(error) = std::fs::write(&header_path, &header_content) {
      cmd_fail!(&config, "Test setup failed", start.elapsed());
      eprintln!(
        "{} Failed to write header '{}': {}",
        "Error:".red().bold(),
        header_path.display(),
        error
      );
      return Err(());
    }

    for dep_id in &dep_ids {
      let dep_module = ctx.module_graph.modules.get(dep_id);
      if let ModulePath::Project(dep_path) = &dep_module.path {
        if dep_path == source_path {
          continue;
        }

        let dep_header_rel = input.layout.relativize(dep_path).with_extension("h");
        user_module_headers.push(ignis_config::CHeader {
          path: normalize_path_for_include(&dep_header_rel),
          quoted: true,
        });
      }
    }

    let module_c_code = match emit_text(
      &selected_backend,
      BackendInput::Lowered {
        root_id: module_id,
        types: &types,
        defs: &mono_output.defs,
        program: &lir_program,
      },
      BackendRequest::Lowered(LoweredBackendRequest::EmitUserModule {
        module_id,
        namespaces: &output.namespaces,
        symbols: &sym_table,
        headers: &link_plan_with_user_includes.headers,
        module_paths: &module_paths,
        user_module_headers: &user_module_headers,
        std_path: input.source_dir.as_path(),
        std_defined_symbols: std_defined_symbols.as_ref(),
      }),
    ) {
      Ok(contents) => contents,
      Err(stage_error) => {
        cmd_fail!(&config, "Test setup failed", start.elapsed());
        eprintln!("{} {}", "Error:".red().bold(), stage_error);
        return Err(());
      },
    };

    let module_c_path = input.layout.user_module_src(source_path);
    if let Err(error) = std::fs::write(&module_c_path, &module_c_code) {
      cmd_fail!(&config, "Test setup failed", start.elapsed());
      eprintln!(
        "{} Failed to write module source '{}': {}",
        "Error:".red().bold(),
        module_c_path.display(),
        error
      );
      return Err(());
    }

    let module_object_path = input.layout.user_module_obj(source_path);
    if let Err(error) = compile_to_object(
      &module_c_path,
      &module_object_path,
      &link_plan_with_user_includes,
      !ignis_log::show_verbose(&config),
    ) {
      cmd_fail!(&config, "Test setup failed", start.elapsed());
      eprintln!("{} {}", "Error:".red().bold(), error);
      return Err(());
    }

    user_object_paths.push(module_object_path);
  }

  let Some(harness_root_id) = plan.tests.first().and_then(|test| {
    ctx
      .module_graph
      .get_by_path(&ModulePath::Project(test.source_path.clone()))
  }) else {
    cmd_fail!(&config, "Test setup failed", start.elapsed());
    eprintln!("{} Missing discovered std test root module", "Error:".red().bold());
    return Err(());
  };

  let harness_c_code = match emit_text(
    &selected_backend,
    BackendInput::Lowered {
      root_id: harness_root_id,
      types: &types,
      defs: &mono_output.defs,
      program: &lir_program,
    },
    BackendRequest::Lowered(LoweredBackendRequest::EmitUserTestHarness {
      namespaces: &output.namespaces,
      symbols: &sym_table,
      headers: &link_plan.headers,
      module_paths: &module_paths,
      plan: &plan,
    }),
  ) {
    Ok(contents) => contents,
    Err(stage_error) => {
      cmd_fail!(&config, "Test setup failed", start.elapsed());
      eprintln!("{} {}", "Error:".red().bold(), stage_error);
      return Err(());
    },
  };

  if let Err(error) = std::fs::create_dir_all(&input.out_dir) {
    cmd_fail!(&config, "Test setup failed", start.elapsed());
    eprintln!(
      "{} Failed to create build directory '{}': {}",
      "Error:".red().bold(),
      input.out_dir.display(),
      error
    );
    return Err(());
  }

  if let Some(parent) = input.bin_path.parent()
    && let Err(error) = std::fs::create_dir_all(parent)
  {
    cmd_fail!(&config, "Test setup failed", start.elapsed());
    eprintln!(
      "{} Failed to create binary directory '{}': {}",
      "Error:".red().bold(),
      parent.display(),
      error
    );
    return Err(());
  }

  if let Err(error) = std::fs::write(&input.harness_c_path, &harness_c_code) {
    cmd_fail!(&config, "Test setup failed", start.elapsed());
    eprintln!(
      "{} Failed to write harness source '{}': {}",
      "Error:".red().bold(),
      input.harness_c_path.display(),
      error
    );
    return Err(());
  }

  let harness_object_path = input.harness_c_path.with_extension("o");
  let suppress_link_logs = !ignis_log::show_verbose(&config);

  if let Err(error) = compile_to_object(&input.harness_c_path, &harness_object_path, &link_plan, suppress_link_logs) {
    cmd_fail!(&config, "Test setup failed", start.elapsed());
    eprintln!("{} {}", "Error:".red().bold(), error);
    return Err(());
  }

  let mut executable_objects = user_object_paths;
  executable_objects.push(harness_object_path);

  if let Err(error) = link_executable_multi(&executable_objects, &input.bin_path, &link_plan, suppress_link_logs) {
    cmd_fail!(&config, "Test setup failed", start.elapsed());
    eprintln!("{} {}", "Error:".red().bold(), error);
    return Err(());
  }

  section!(&config, "Running");

  let results = match execute_test_harness_binary(&input.bin_path, &plan, update_snapshots) {
    Ok(results) => results,
    Err(error) => {
      cmd_fail!(&config, "Test setup failed", start.elapsed());
      eprintln!("{} {}", "Error:".red().bold(), error);
      return Err(());
    },
  };

  let passed = results.iter().filter(|result| result.success).count();
  let total = results.len();
  let failed = total - passed;

  for result in &results {
    let status = if result.success {
      "ok".green()
    } else {
      "FAILED".red().bold()
    };
    section_item!(&config, "{} ... {}", result.fq_name, status);
  }

  let failed_results: Vec<_> = results.iter().filter(|result| !result.success).collect();
  if !failed_results.is_empty() {
    section!(&config, "Failures");

    for result in failed_results {
      for line in format_failed_test_details(result).lines() {
        println!("  {}", line);
      }

      println!();
    }
  }

  section!(&config, "Summary");
  section_item!(&config, "{} total", total);
  section_item!(&config, "{} passed", passed);
  section_item!(&config, "{} failed", failed);
  cmd_artifact!(&config, "Test binary", input.bin_path.display());

  if failed == 0 {
    cmd_ok!(&config, "Tests passed", start.elapsed());
    Ok(())
  } else {
    cmd_fail!(&config, "Tests failed", start.elapsed());
    Err(())
  }
}

/// Build the standard library into a static archive
pub fn build_std(
  config: Arc<IgnisConfig>,
  output_dir: &str,
) -> Result<(), ()> {
  use std::collections::HashSet;

  let start = Instant::now();

  cmd_header!(&config, "Building std", &config.std_path);

  if config.std_path.is_empty() {
    cmd_fail!(&config, "Build failed", start.elapsed());
    eprintln!(
      "{} std_path not set. Use --std-path or set IGNIS_STD_PATH env var",
      "Error:".red().bold()
    );
    return Err(());
  }

  let std_path = Path::new(&config.std_path);

  if !std_path.exists() {
    cmd_fail!(&config, "Build failed", start.elapsed());
    eprintln!("{} std_path '{}' does not exist", "Error:".red().bold(), std_path.display());
    return Err(());
  }

  if config.manifest.modules.is_empty() {
    cmd_fail!(&config, "Build failed", start.elapsed());
    eprintln!(
      "{} No modules found in std manifest at '{}/manifest.toml'",
      "Error:".red().bold(),
      std_path.display()
    );
    return Err(());
  }

  trace_dbg!(&config, DebugTrace::Std, "building standard library");

  // Create build layout with new directory structure
  let layout = BuildLayout::new("std", Path::new(output_dir));
  if let Err(e) = layout.create_std_dirs() {
    cmd_fail!(&config, "Build failed", start.elapsed());
    eprintln!("{} Failed to create output directories: {}", "Error:".red().bold(), e);
    return Err(());
  }

  section!(&config, "Scanning & parsing");

  log_dbg!(&config, "std manifest modules {}", config.manifest.modules.len());

  let mut ctx = CompilationContext::new(&config);

  // Sort module names for deterministic discovery order (HashMap iteration is random).
  let mut module_names: Vec<&str> = config.manifest.modules.keys().map(|s| s.as_str()).collect();
  module_names.sort();

  for module_name in &module_names {
    if let Err(()) = ctx.discover_std_module(module_name, &config) {
      cmd_fail!(&config, "Build failed", start.elapsed());
      eprintln!("{} Failed to discover std module '{}'", "Error:".red().bold(), module_name);
      return Err(());
    }
  }

  ctx.discover_prelude_modules_for_all(&config);

  section!(&config, "Analyzing");

  let output = ctx.compile_all(&config)?;
  let mut link_plan = LinkPlan::from_manifest(&config.manifest, std_path);
  link_plan.cc = config.c_compiler.clone();
  link_plan.cflags = config.cflags.clone();

  // Add std include directory for std module inter-dependencies
  // This allows std module C files to #include "ignis_std.h"
  link_plan.include_dirs.push(layout.std_include_dir());

  // Build module paths map for classification
  let module_paths = build_module_paths_from_graph(&ctx.module_graph);

  section!(&config, "Codegen & linking");
  let selected_backend = select_backend_or_report(&config, "Build failed", start)?;

  let all_module_ids = ctx.module_graph.all_modules_topological();
  let mut processed_modules: HashSet<String> = HashSet::new();
  let mut generated_module_names: Vec<String> = Vec::new();

  // Phase 1: Generate all module headers first
  // We need to do a first pass to just generate headers, then umbrella,
  // then C files. This is necessary because std modules can depend on each other.
  for module_id in &all_module_ids {
    let module = ctx.module_graph.modules.get(module_id);
    let module_name = module.path.module_name();

    if module
      .path
      .std_module_name()
      .is_some_and(|std_name| config.manifest.is_compile_only(std_name))
    {
      continue;
    }

    if processed_modules.contains(&module_name) {
      continue;
    }
    processed_modules.insert(module_name.clone());

    let single_module_set: HashSet<ModuleId> = [*module_id].into_iter().collect();

    let mut types = output.types.clone();

    let mono_roots = collect_mono_roots_for_std(&output.defs);
    let mono_output = ignis_analyzer::mono::Monomorphizer::new(
      &output.hir,
      &output.defs,
      &output.namespaces,
      &mut types,
      output.symbols.clone(),
    )
    .run(&mono_roots);

    #[cfg(debug_assertions)]
    mono_output.verify_no_generics(&types);

    let sym_table = output.symbols.borrow();

    let ownership_checker =
      ignis_analyzer::HirOwnershipChecker::new(&mono_output.hir, &types, &mono_output.defs, &sym_table)
        .with_source_map(&ctx.source_map);
    let (drop_schedules, _ownership_diagnostics) = ownership_checker.check();

    let (lir_program, _verify_result) = ignis_lir::lowering::lower_and_verify(
      &mono_output.hir,
      &mut types,
      &mono_output.defs,
      &sym_table,
      &drop_schedules,
      Some(&single_module_set),
    );

    // Generate module header
    let header_content = match emit_text(
      &selected_backend,
      BackendInput::Lowered {
        root_id: *module_id,
        types: &types,
        defs: &mono_output.defs,
        program: &lir_program,
      },
      BackendRequest::Header(HeaderBackendRequest::EmitStdModuleHeader {
        module_name: &module_name,
        namespaces: &output.namespaces,
        symbols: &sym_table,
        module_paths: &module_paths,
      }),
    ) {
      Ok(contents) => contents,
      Err(stage_error) => {
        cmd_fail!(&config, "Build failed", start.elapsed());
        eprintln!("{} {}", "Error:".red().bold(), stage_error);
        return Err(());
      },
    };

    let header_path = layout.std_module_header(&module_name);
    if let Err(e) = std::fs::write(&header_path, &header_content) {
      cmd_fail!(&config, "Build failed", start.elapsed());
      eprintln!(
        "{} Failed to write header '{}': {}",
        "Error:".red().bold(),
        header_path.display(),
        e
      );
      return Err(());
    }

    let nested_header_path = if let Some(std_module_name) = module.path.std_module_name()
      && std_module_name.contains("::")
    {
      Some(
        layout
          .std_include_dir()
          .join(std_module_name.replace("::", std::path::MAIN_SEPARATOR_STR))
          .with_extension("h"),
      )
    } else if let ModulePath::Project(project_path) = &module.path {
      project_path
        .strip_prefix(std_path)
        .ok()
        .map(|relative| layout.std_include_dir().join(relative).with_extension("h"))
    } else {
      None
    };

    if let Some(nested_header_path) = nested_header_path {
      if let Some(parent) = nested_header_path.parent()
        && let Err(error) = std::fs::create_dir_all(parent)
      {
        cmd_fail!(&config, "Build failed", start.elapsed());
        eprintln!(
          "{} Failed to create std header directory '{}': {}",
          "Error:".red().bold(),
          parent.display(),
          error
        );
        return Err(());
      }

      if let Err(error) = std::fs::write(&nested_header_path, &header_content) {
        cmd_fail!(&config, "Build failed", start.elapsed());
        eprintln!(
          "{} Failed to write header '{}': {}",
          "Error:".red().bold(),
          nested_header_path.display(),
          error
        );
        return Err(());
      }
    }

    generated_module_names.push(module_name);
  }

  if generated_module_names.is_empty() {
    cmd_fail!(&config, "Build failed", start.elapsed());
    eprintln!("{} No modules generated headers", "Error:".red().bold());
    return Err(());
  }

  // Phase 2: Generate umbrella header (before C files, so they can include it)
  let module_names_ref: Vec<&str> = generated_module_names.iter().map(|s| s.as_str()).collect();
  let umbrella_content = generate_std_umbrella_header(&module_names_ref);
  let umbrella_path = layout.std_umbrella_header();
  if let Err(e) = std::fs::write(&umbrella_path, &umbrella_content) {
    cmd_fail!(&config, "Build failed", start.elapsed());
    eprintln!(
      "{} Failed to write umbrella header '{}': {}",
      "Error:".red().bold(),
      umbrella_path.display(),
      e
    );
    return Err(());
  }

  phase_ok!(&config, "Generated {} module headers + umbrella", generated_module_names.len());

  // Phase 3: Generate C files and compile objects
  // We need to redo the processing because we can't easily store all the intermediate state
  let mut object_files: Vec<std::path::PathBuf> = Vec::new();
  processed_modules.clear();

  for module_id in &all_module_ids {
    let module = ctx.module_graph.modules.get(module_id);
    let module_name = module.path.module_name();

    if module
      .path
      .std_module_name()
      .is_some_and(|std_name| config.manifest.is_compile_only(std_name))
    {
      continue;
    }

    if processed_modules.contains(&module_name) {
      continue;
    }
    processed_modules.insert(module_name.clone());

    // Skip modules that didn't generate headers (no functions)
    if !generated_module_names.contains(&module_name) {
      continue;
    }

    let single_module_set: HashSet<ModuleId> = [*module_id].into_iter().collect();
    let mut types = output.types.clone();

    let mono_roots = collect_mono_roots_for_std(&output.defs);
    let mono_output = ignis_analyzer::mono::Monomorphizer::new(
      &output.hir,
      &output.defs,
      &output.namespaces,
      &mut types,
      output.symbols.clone(),
    )
    .run(&mono_roots);

    let sym_table = output.symbols.borrow();

    let ownership_checker =
      ignis_analyzer::HirOwnershipChecker::new(&mono_output.hir, &types, &mono_output.defs, &sym_table)
        .with_source_map(&ctx.source_map);
    let (drop_schedules, _ownership_diagnostics) = ownership_checker.check();

    let (lir_program, verify_result) = ignis_lir::lowering::lower_and_verify(
      &mono_output.hir,
      &mut types,
      &mono_output.defs,
      &sym_table,
      &drop_schedules,
      Some(&single_module_set),
    );

    trace_dbg!(&config, DebugTrace::Lir, "std lowering completed for module {}", module_name);

    if let Err(errors) = &verify_result {
      eprintln!(
        "{} LIR verification errors for module '{}':",
        "Warning:".yellow().bold(),
        module_name
      );
      for err in errors {
        eprintln!("  {:?}", err);
      }

      if config.debug {
        cmd_fail!(&config, "Build failed", start.elapsed());
        eprintln!("{} LIR verification failed in debug mode", "Error:".red().bold());
        return Err(());
      }
    }

    trace_dbg!(&config, DebugTrace::Codegen, "std emitting C for module {}", module_name);

    // Generate module C code with umbrella header for inter-module dependencies
    let umbrella_header = "ignis_std.h";
    let c_code = match emit_text(
      &selected_backend,
      BackendInput::Lowered {
        root_id: *module_id,
        types: &types,
        defs: &mono_output.defs,
        program: &lir_program,
      },
      BackendRequest::Lowered(LoweredBackendRequest::EmitStdModule {
        module_name: &module_name,
        namespaces: &output.namespaces,
        symbols: &sym_table,
        headers: &link_plan.headers,
        module_paths: &module_paths,
        umbrella_header_path: Some(umbrella_header),
        std_path,
      }),
    ) {
      Ok(contents) => contents,
      Err(stage_error) => {
        cmd_fail!(&config, "Build failed", start.elapsed());
        eprintln!("{} {}", "Error:".red().bold(), stage_error);
        return Err(());
      },
    };

    let c_path = layout.std_module_src(&module_name);
    if let Err(e) = std::fs::write(&c_path, &c_code) {
      cmd_fail!(&config, "Build failed", start.elapsed());
      eprintln!("{} Failed to write C file '{}': {}", "Error:".red().bold(), c_path.display(), e);
      return Err(());
    }

    // Compile to object file
    let obj_path = layout.std_module_obj(&module_name);
    trace_dbg!(&config, DebugTrace::Link, "std compiling object for module {}", module_name);

    let suppress_link_logs = !ignis_log::show_verbose(&config);
    if let Err(e) = compile_to_object(&c_path, &obj_path, &link_plan, suppress_link_logs) {
      cmd_fail!(&config, "Build failed", start.elapsed());
      eprintln!("{} {}", "Error:".red().bold(), e);
      return Err(());
    }

    object_files.push(obj_path);
  }

  if object_files.is_empty() {
    cmd_fail!(&config, "Build failed", start.elapsed());
    eprintln!("{} No object files generated", "Error:".red().bold());
    return Err(());
  }

  // Create static archive
  let archive_path = layout.std_lib_path();
  trace_dbg!(&config, DebugTrace::Link, "archiving standard library objects");

  if let Err(e) = create_static_archive_multi(&object_files, &archive_path, log_phase(&config)) {
    cmd_fail!(&config, "Build failed", start.elapsed());
    eprintln!("{} {}", "Error:".red().bold(), e);
    return Err(());
  }

  // Write stamp file for cache invalidation
  let stamp_path = layout.std_stamp_path();
  let fingerprint = BuildFingerprint {
    compiler_version: COMPILER_VERSION.to_string(),
    codegen_abi_version: ignis_codegen_c::CODEGEN_ABI_VERSION,
    target: String::new(),
  };

  match StdStamp::compute(std_path, fingerprint) {
    Ok(stamp) => {
      if let Err(e) = write_std_stamp(&stamp_path, &stamp) {
        // Non-fatal: warn but don't fail the build
        eprintln!(
          "{} Failed to write stamp file '{}': {}",
          "Warning:".yellow().bold(),
          stamp_path.display(),
          e
        );
      }
    },
    Err(e) => {
      eprintln!("{} Failed to compute stamp: {}", "Warning:".yellow().bold(), e);
    },
  }

  cmd_ok!(&config, "Built standard library", start.elapsed());
  cmd_artifact!(&config, "Archive", archive_path.display());

  Ok(())
}

pub fn check_std(
  config: Arc<IgnisConfig>,
  output_dir: &str,
) -> Result<(), ()> {
  use std::collections::HashSet;
  use ignis_type::module::ModuleId;

  let start = Instant::now();

  cmd_header!(&config, "Checking std", &config.std_path);

  if config.std_path.is_empty() {
    cmd_fail!(&config, "Check failed", start.elapsed());
    eprintln!(
      "{} std_path not set. Use --std-path or set IGNIS_STD_PATH env var",
      "Error:".red().bold()
    );
    return Err(());
  }

  let std_path = Path::new(&config.std_path);

  if !std_path.exists() {
    cmd_fail!(&config, "Check failed", start.elapsed());
    eprintln!("{} std_path '{}' does not exist", "Error:".red().bold(), std_path.display());
    return Err(());
  }

  if config.manifest.modules.is_empty() {
    cmd_fail!(&config, "Check failed", start.elapsed());
    eprintln!(
      "{} No modules found in std manifest at '{}/manifest.toml'",
      "Error:".red().bold(),
      std_path.display()
    );
    return Err(());
  }

  section!(&config, "Scanning & parsing");

  let output_path = Path::new(output_dir);
  if !output_path.exists()
    && let Err(e) = std::fs::create_dir_all(output_path)
  {
    cmd_fail!(&config, "Check failed", start.elapsed());
    eprintln!(
      "{} Failed to create output directory '{}': {}",
      "Error:".red().bold(),
      output_path.display(),
      e
    );
    return Err(());
  }

  let mut ctx = CompilationContext::new(&config);

  let mut module_names: Vec<&str> = config.manifest.modules.keys().map(|s| s.as_str()).collect();
  module_names.sort();

  for module_name in &module_names {
    if let Err(()) = ctx.discover_std_module(module_name, &config) {
      cmd_fail!(&config, "Check failed", start.elapsed());
      eprintln!("{} Failed to discover std module '{}'", "Error:".red().bold(), module_name);
      return Err(());
    }
  }

  ctx.discover_prelude_modules_for_all(&config);

  section!(&config, "Analyzing");

  let output = ctx.compile_all(&config)?;
  let mut link_plan = LinkPlan::from_manifest(&config.manifest, std_path);
  link_plan.cc = config.c_compiler.clone();
  link_plan.cflags = config.cflags.clone();

  section!(&config, "Codegen (check)");
  let selected_backend = select_backend_or_report(&config, "Check failed", start)?;

  let header_content = {
    let sym_table = output.symbols.borrow();
    match emit_text(
      &selected_backend,
      BackendInput::Header {
        types: &output.types,
        defs: &output.defs,
      },
      BackendRequest::Header(HeaderBackendRequest::EmitStdHeader {
        namespaces: &output.namespaces,
        symbols: &sym_table,
      }),
    ) {
      Ok(contents) => contents,
      Err(stage_error) => {
        cmd_fail!(&config, "Check failed", start.elapsed());
        eprintln!("{} {}", "Error:".red().bold(), stage_error);
        return Err(());
      },
    }
  };
  let header_path = output_path.join("ignis_std.h");
  if let Err(e) = std::fs::write(&header_path, &header_content) {
    cmd_fail!(&config, "Check failed", start.elapsed());
    eprintln!(
      "{} Failed to write header file '{}': {}",
      "Error:".red().bold(),
      header_path.display(),
      e
    );
    return Err(());
  }

  let module_paths = build_module_paths_from_graph(&ctx.module_graph);
  let all_module_ids = ctx.module_graph.all_modules_topological();
  let mut processed_modules: HashSet<String> = HashSet::new();

  for module_id in &all_module_ids {
    let module = ctx.module_graph.modules.get(module_id);
    let module_name = module.path.module_name();

    if module
      .path
      .std_module_name()
      .is_some_and(|std_name| config.manifest.is_compile_only(std_name))
    {
      continue;
    }

    if processed_modules.contains(&module_name) {
      continue;
    }
    processed_modules.insert(module_name.clone());

    let single_module_set: HashSet<ModuleId> = [*module_id].into_iter().collect();
    let mut types = output.types.clone();

    let mono_roots = collect_mono_roots_for_std(&output.defs);
    let mono_output = ignis_analyzer::mono::Monomorphizer::new(
      &output.hir,
      &output.defs,
      &output.namespaces,
      &mut types,
      output.symbols.clone(),
    )
    .run(&mono_roots);

    #[cfg(debug_assertions)]
    mono_output.verify_no_generics(&types);

    let sym_table = output.symbols.borrow();

    let ownership_checker =
      ignis_analyzer::HirOwnershipChecker::new(&mono_output.hir, &types, &mono_output.defs, &sym_table)
        .with_source_map(&ctx.source_map);
    let (drop_schedules, _ownership_diagnostics) = ownership_checker.check();

    let (lir_program, verify_result) = ignis_lir::lowering::lower_and_verify(
      &mono_output.hir,
      &mut types,
      &mono_output.defs,
      &sym_table,
      &drop_schedules,
      Some(&single_module_set),
    );

    if let Err(errors) = &verify_result {
      eprintln!(
        "{} LIR verification errors for module '{}':",
        "Warning:".yellow().bold(),
        module_name
      );
      for err in errors {
        eprintln!("  {:?}", err);
      }

      if config.debug {
        cmd_fail!(&config, "Check failed", start.elapsed());
        eprintln!("{} LIR verification failed in debug mode", "Error:".red().bold());
        return Err(());
      }
    }

    if verify_result.is_err() {
      continue;
    }

    let c_code = match emit_text(
      &selected_backend,
      BackendInput::Lowered {
        root_id: *module_id,
        types: &types,
        defs: &mono_output.defs,
        program: &lir_program,
      },
      BackendRequest::Lowered(LoweredBackendRequest::EmitStdModule {
        module_name: &module_name,
        namespaces: &output.namespaces,
        symbols: &sym_table,
        headers: &link_plan.headers,
        module_paths: &module_paths,
        umbrella_header_path: None,
        std_path,
      }),
    ) {
      Ok(contents) => contents,
      Err(stage_error) => {
        cmd_fail!(&config, "Check failed", start.elapsed());
        eprintln!("{} {}", "Error:".red().bold(), stage_error);
        return Err(());
      },
    };

    let c_path = output_path.join(format!("{}.c", module_name));
    if let Err(e) = std::fs::write(&c_path, &c_code) {
      cmd_fail!(&config, "Check failed", start.elapsed());
      eprintln!("{} Failed to write C file '{}': {}", "Error:".red().bold(), c_path.display(), e);
      return Err(());
    }
  }

  cmd_ok!(&config, "No errors found", start.elapsed());

  Ok(())
}

pub fn check_runtime(
  config: Arc<IgnisConfig>,
  runtime_path: Option<&str>,
) -> Result<(), ()> {
  let root = if let Some(path) = runtime_path {
    PathBuf::from(path)
  } else {
    Path::new(&config.std_path).join("runtime")
  };

  if !root.exists() {
    eprintln!("{} runtime path '{}' does not exist", "Error:".red().bold(), root.display());
    return Err(());
  }

  let mut c_files = Vec::new();
  collect_c_files(&root, &mut c_files);

  if c_files.is_empty() {
    eprintln!("{} No runtime C files found under '{}'", "Error:".red().bold(), root.display());
    return Err(());
  }

  for file in &c_files {
    let status = Command::new("cc").arg("-fsyntax-only").arg(file).status();

    let status = match status {
      Ok(s) => s,
      Err(e) => {
        eprintln!("{} Failed to run cc: {}", "Error:".red().bold(), e);
        return Err(());
      },
    };

    if !status.success() {
      eprintln!("{} Runtime check failed for '{}'", "Error:".red().bold(), file.display());
      return Err(());
    }
  }

  phase_ok!(&config, "Runtime check complete");

  Ok(())
}

fn collect_c_files(
  root: &Path,
  out: &mut Vec<PathBuf>,
) {
  if let Ok(entries) = std::fs::read_dir(root) {
    for entry in entries.flatten() {
      let path = entry.path();
      if path.is_dir() {
        collect_c_files(&path, out);
      } else if let Some(ext) = path.extension()
        && ext == "c"
      {
        out.push(path);
      }
    }
  }
}

fn ensure_std_built(
  used_modules: &[ignis_type::module::ModuleId],
  module_graph: &ignis_analyzer::modules::ModuleGraph,
  config: &Arc<IgnisConfig>,
) -> Result<(), ()> {
  let uses_std = used_modules.iter().any(|id| module_graph.modules.get(id).path.is_std());

  if !uses_std {
    return Ok(());
  }

  let build_dir = config
    .build_config
    .as_ref()
    .map(|bc| bc.output_dir.clone())
    .unwrap_or_else(|| "build".to_string());
  let layout = BuildLayout::new("std", Path::new(&build_dir));
  let archive_path = layout.std_lib_path();
  let stamp_path = layout.std_stamp_path();
  let std_path = Path::new(&config.std_path);

  let fingerprint = BuildFingerprint {
    compiler_version: COMPILER_VERSION.to_string(),
    codegen_abi_version: ignis_codegen_c::CODEGEN_ABI_VERSION,
    target: String::new(),
  };

  // Check if archive exists AND stamp is valid
  if archive_path.exists() {
    if is_std_stamp_valid(&stamp_path, std_path, &fingerprint) {
      return Ok(());
    }
    phase_warn!(config, "std library outdated, rebuilding...");
  } else {
    phase_warn!(config, "std library not found, building...");
  }

  build_std(config.clone(), &build_dir)
}

/// Create a static archive (.a) from multiple object files.
/// Deletes any existing archive first (`ar rcs` would otherwise append).
fn create_static_archive_multi(
  objects: &[std::path::PathBuf],
  archive_path: &Path,
  log_info: bool,
) -> Result<(), String> {
  use std::process::Command;

  if archive_path.exists() {
    std::fs::remove_file(archive_path).map_err(|e| format!("Failed to remove old archive: {}", e))?;
  }

  if log_info {
    let obj_names: Vec<_> = objects.iter().filter_map(|p| p.file_name()).collect();
    eprintln!(
      "{} Creating archive {} <- {:?}",
      "-->".bright_green().bold(),
      archive_path.display(),
      obj_names
    );
  }

  let output = Command::new("ar")
    .arg("rcs")
    .arg(archive_path)
    .args(objects)
    .output()
    .map_err(|e| format!("Failed to run ar: {}", e))?;

  if !output.status.success() {
    let stderr = String::from_utf8_lossy(&output.stderr);
    return Err(format_tool_error("ar", "archive creation", &stderr));
  }

  Ok(())
}

fn collect_archive_defined_symbols(archive_path: &Path) -> Result<HashSet<String>, String> {
  let output = Command::new("nm")
    .arg("-g")
    .arg("--defined-only")
    .arg(archive_path)
    .output()
    .map_err(|error| format!("Failed to run nm for '{}': {}", archive_path.display(), error))?;

  if !output.status.success() {
    let stderr = String::from_utf8_lossy(&output.stderr);
    return Err(format_tool_error("nm", "archive symbol scan", &stderr));
  }

  let stdout = String::from_utf8_lossy(&output.stdout);
  let mut symbols = HashSet::new();

  for line in stdout.lines() {
    let Some(symbol) = line.split_whitespace().last() else {
      continue;
    };

    if !symbol.ends_with(':') {
      symbols.insert(symbol.to_string());
    }
  }

  Ok(symbols)
}

/// Collect root definitions for monomorphization.
///
/// For executables: starts from main function.
/// For libraries: includes all public exports.
fn collect_mono_roots(
  defs: &DefinitionStore,
  symbols: &SymbolTable,
) -> Vec<DefinitionId> {
  let mut roots = Vec::new();

  for (def_id, def) in defs.iter() {
    match &def.kind {
      // Include main function
      DefinitionKind::Function(fd) if !fd.is_extern && !fd.is_compile_time_only() => {
        let name = symbols.get(&def.name);
        if name == "main" {
          roots.push(def_id);
        }
      },

      // Include all public functions
      DefinitionKind::Function(fd)
        if def.visibility == Visibility::Public && !fd.is_extern && !fd.is_compile_time_only() =>
      {
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

/// Collect monomorphization roots for std builds.
/// Includes all non-extern functions regardless of visibility.
fn collect_mono_roots_for_std(defs: &DefinitionStore) -> Vec<DefinitionId> {
  let mut roots = Vec::new();

  for (def_id, def) in defs.iter() {
    match &def.kind {
      DefinitionKind::Function(fd) if !fd.is_extern && fd.type_params.is_empty() && !fd.is_compile_time_only() => {
        roots.push(def_id);
      },
      DefinitionKind::Method(md) if md.type_params.is_empty() => {
        roots.push(def_id);
      },
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

fn build_module_paths_from_graph(module_graph: &ignis_analyzer::modules::ModuleGraph) -> HashMap<ModuleId, ModulePath> {
  module_graph
    .by_path
    .iter()
    .map(|(path, id)| (*id, path.clone()))
    .collect()
}

fn generate_std_umbrella_header(module_names: &[&str]) -> String {
  use std::fmt::Write;

  let mut out = String::new();
  writeln!(out, "#ifndef IGNIS_STD_H").unwrap();
  writeln!(out, "#define IGNIS_STD_H").unwrap();
  writeln!(out).unwrap();
  writeln!(out, "#include \"runtime/ignis_rt.h\"").unwrap();
  writeln!(out).unwrap();
  for name in module_names {
    writeln!(out, "#include \"std_{}.h\"", name).unwrap();
  }
  writeln!(out).unwrap();
  writeln!(out, "#endif // IGNIS_STD_H").unwrap();
  out
}

fn generate_user_umbrella_header(source_paths: &[PathBuf]) -> String {
  use std::fmt::Write;

  let mut out = String::new();
  writeln!(out, "#ifndef IGNIS_USER_H").unwrap();
  writeln!(out, "#define IGNIS_USER_H").unwrap();
  writeln!(out).unwrap();
  writeln!(out, "#include \"runtime/ignis_rt.h\"").unwrap();
  writeln!(out).unwrap();

  for source_path in source_paths {
    let normalized: PathBuf = source_path
      .components()
      .filter(|c| !matches!(c, std::path::Component::CurDir))
      .collect();
    writeln!(out, "#include \"{}\"", normalized.with_extension("h").display()).unwrap();
  }

  writeln!(out).unwrap();
  writeln!(out, "#endif // IGNIS_USER_H").unwrap();
  out
}

/// Normalize a path for use in #include directives.
/// Removes `.` components and converts to forward slashes.
fn normalize_path_for_include(path: &Path) -> String {
  let normalized: PathBuf = path
    .components()
    .filter(|c| !matches!(c, std::path::Component::CurDir))
    .collect();
  normalized.display().to_string().replace('\\', "/")
}

#[cfg(test)]
mod tests {
  use std::fs;
  use std::collections::HashMap;
  use std::path::{Path, PathBuf};

  use insta::assert_snapshot;
  use tempfile::TempDir;

  use ignis_type::attribute::{DirectiveEffect, DirectiveMetadata, DirectivePhase, DirectiveTarget, FunctionAttr};
  use ignis_type::definition::{Definition, DefinitionKind, DefinitionStore, FunctionDefinition, Visibility};
  use ignis_type::module::{ModuleId, ModulePath};
  use ignis_type::namespace::NamespaceStore;
  use ignis_type::span::Span;
  use ignis_type::symbol::SymbolTable;
  use ignis_type::types::TypeStore;

  use super::{
    build_test_harness_plan, discover_project_test_sources, discover_std_test_companions, discover_test_cases,
    execute_test_harness_binary, format_test_plan_snapshot, format_failed_test_details, format_test_summary_snapshot,
    is_project_test_file_name, plan_project_tests_for_snapshot, validate_std_test_layout,
  };
  use crate::backend::{TestCase, TestHarnessPlan};

  fn workspace_std_path() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../std")
  }

  fn write_test_project(source: &str) -> TempDir {
    let temp_dir = TempDir::new().expect("temporary project dir");
    let src_dir = temp_dir.path().join("src");

    fs::create_dir_all(&src_dir).expect("create src dir");
    fs::write(src_dir.join("main.ign"), source).expect("write main module");
    fs::write(
      temp_dir.path().join("ignis.toml"),
      format!(
        "[package]\nname = \"native_test_runner_fixture\"\nversion = \"0.1.0\"\nauthors = []\ndescription = \"fixture\"\nkeywords = []\nlicense = \"MIT\"\nrepository = \"\"\n\n[ignis]\nstd = true\nstd_path = \"{}\"\n\n[build]\nbin = true\nsource_dir = \"src\"\nentry = \"main.ign\"\nout_dir = \"build\"\nopt_level = 0\ndebug = false\ntarget = \"c\"\ncc = \"cc\"\ncflags = []\nemit = []\n",
        workspace_std_path().display()
      ),
    )
    .expect("write ignis.toml");

    temp_dir
  }

  #[test]
  fn discover_test_cases_collects_only_marked_functions_from_known_modules() {
    let mut symbols = SymbolTable::new();
    let mut namespaces = NamespaceStore::new();
    let types = TypeStore::new();
    let mut defs = DefinitionStore::new();

    let helpers_name = symbols.intern("helpers");
    let adds_name = symbols.intern("adds");
    let smoke_name = symbols.intern("smoke");
    let ignored_name = symbols.intern("ignored");
    let not_a_test_name = symbols.intern("helper");

    let helpers_namespace = namespaces.get_or_create(&[helpers_name], false);

    alloc_test_function(&mut defs, adds_name, ModuleId::new(1), Some(helpers_namespace), &types);
    alloc_test_function(&mut defs, smoke_name, ModuleId::new(0), None, &types);
    alloc_plain_function(&mut defs, not_a_test_name, ModuleId::new(0), None, Visibility::Private, &types);
    alloc_test_function(&mut defs, ignored_name, ModuleId::new(9), None, &types);

    let module_paths = HashMap::from([
      (
        ModuleId::new(0),
        ModulePath::Project(PathBuf::from("/tmp/project/src/main.ign")),
      ),
      (
        ModuleId::new(1),
        ModulePath::Project(PathBuf::from("/tmp/project/src/math.ign")),
      ),
    ]);

    let discovered = discover_test_cases(&defs, &namespaces, &symbols, &module_paths, Path::new("/tmp/project/src"));

    let names: Vec<&str> = discovered.iter().map(|test| test.fq_name.as_str()).collect();
    assert_eq!(names, vec!["math::helpers::adds", "main::smoke"]);
    assert_eq!(discovered[0].source_path, PathBuf::from("/tmp/project/src/math.ign"));
    assert_eq!(discovered[1].source_path, PathBuf::from("/tmp/project/src/main.ign"));
  }

  #[test]
  fn build_test_harness_plan_sorts_and_filters_by_case_sensitive_substring() {
    let discovered = vec![
      TestCase {
        def_id: ignis_type::definition::DefinitionId::new(2),
        fq_name: "math::helpers::Adds".to_string(),
        source_path: PathBuf::from("/tmp/project/src/math.ign"),
      },
      TestCase {
        def_id: ignis_type::definition::DefinitionId::new(1),
        fq_name: "io::writes".to_string(),
        source_path: PathBuf::from("/tmp/project/src/io.ign"),
      },
      TestCase {
        def_id: ignis_type::definition::DefinitionId::new(0),
        fq_name: "math::adds".to_string(),
        source_path: PathBuf::from("/tmp/project/src/math.ign"),
      },
    ];

    let sorted = build_test_harness_plan(discovered.clone(), None);
    let sorted_names: Vec<&str> = sorted.tests.iter().map(|test| test.fq_name.as_str()).collect();
    assert_eq!(sorted_names, vec!["io::writes", "math::adds", "math::helpers::Adds"]);

    let filtered = build_test_harness_plan(discovered, Some("adds"));
    let filtered_names: Vec<&str> = filtered.tests.iter().map(|test| test.fq_name.as_str()).collect();
    assert_eq!(filtered_names, vec!["math::adds"]);
  }

  #[test]
  fn test_plan_snapshot_reports_filtered_execution_order() {
    let plan = build_test_harness_plan(
      vec![
        TestCase {
          def_id: ignis_type::definition::DefinitionId::new(2),
          fq_name: "math::helpers::adds".to_string(),
          source_path: PathBuf::from("/tmp/project/src/math.ign"),
        },
        TestCase {
          def_id: ignis_type::definition::DefinitionId::new(1),
          fq_name: "io::writes".to_string(),
          source_path: PathBuf::from("/tmp/project/src/io.ign"),
        },
        TestCase {
          def_id: ignis_type::definition::DefinitionId::new(0),
          fq_name: "math::adds".to_string(),
          source_path: PathBuf::from("/tmp/project/src/math.ign"),
        },
      ],
      Some("adds"),
    );

    assert_snapshot!("native_test_runner_plan_snapshot", format_test_plan_snapshot(&plan));
  }

  #[test]
  fn project_test_planning_is_stable_across_repeated_runs() {
    let project = write_test_project(
      r#"
@test
function zebra(): void {}

@test
function alpha(): void {}

@test
function middle(): void {}
"#,
    );

    let first = plan_project_tests_for_snapshot(project.path(), None).expect("first test plan");
    let second = plan_project_tests_for_snapshot(project.path(), None).expect("second test plan");

    let first_names: Vec<&str> = first.tests.iter().map(|test| test.fq_name.as_str()).collect();
    let second_names: Vec<&str> = second.tests.iter().map(|test| test.fq_name.as_str()).collect();

    assert_eq!(first_names, vec!["main::alpha", "main::middle", "main::zebra"]);
    assert_eq!(first_names, second_names);
  }

  #[test]
  fn project_test_file_name_matcher_accepts_supported_suffixes_only() {
    for file_name in [
      "tests.ign",
      "math.test.ign",
      "math.tests.ign",
      "math-test.ign",
      "math-tests.ign",
      "math_test.ign",
      "math_tests.ign",
    ] {
      assert!(is_project_test_file_name(file_name), "expected '{file_name}' to be discovered");
    }

    for file_name in ["test.ign", "tests.rs", "contest.ign", "math.ign"] {
      assert!(!is_project_test_file_name(file_name), "expected '{file_name}' to be ignored");
    }
  }

  #[test]
  fn discover_project_test_sources_recurses_sorts_and_skips_entry() {
    let project = write_test_project("function main(): i32 { return 0; }\n");

    fs::create_dir_all(project.path().join("src/a/deep")).expect("create nested test dir");
    fs::create_dir_all(project.path().join("src/z")).expect("create z dir");
    fs::write(project.path().join("src/z/late_tests.ign"), "@test\nfunction late(): void {}\n")
      .expect("write late tests");
    fs::write(project.path().join("src/a/deep/tests.ign"), "@test\nfunction deep(): void {}\n")
      .expect("write deep tests");
    fs::write(
      project.path().join("src/a/alpha_test.ign"),
      "@test\nfunction alpha(): void {}\n",
    )
    .expect("write alpha tests");
    fs::write(
      project.path().join("src/main_tests.ign"),
      "@test\nfunction mainTests(): void {}\n",
    )
    .expect("write main-adjacent tests");
    fs::write(project.path().join("src/not_a_test.ignx"), "").expect("write ignored file");

    let discovered = discover_project_test_sources(&project.path().join("src"), &project.path().join("src/main.ign"));

    assert_eq!(
      discovered,
      vec![
        project.path().join("src/a/alpha_test.ign"),
        project.path().join("src/a/deep/tests.ign"),
        project.path().join("src/main_tests.ign"),
        project.path().join("src/z/late_tests.ign"),
      ]
    );
  }

  #[test]
  fn plan_project_tests_discovers_recursive_test_roots_without_duplicate_entries() {
    let project = write_test_project(
      r#"
import _ from "./nested/helpers_test";

function main(): i32 {
    return 0;
}
"#,
    );

    fs::create_dir_all(project.path().join("src/nested/deeper")).expect("create nested dirs");
    fs::write(
      project.path().join("src/nested/helpers_test.ign"),
      "@test\nfunction helperSmoke(): void {}\n",
    )
    .expect("write imported helper tests");
    fs::write(
      project.path().join("src/nested/deeper/tests.ign"),
      "@test\nfunction deeperSmoke(): void {}\n",
    )
    .expect("write recursive tests");

    let plan = plan_project_tests_for_snapshot(project.path(), None).expect("project test plan");
    let names: Vec<&str> = plan.tests.iter().map(|test| test.fq_name.as_str()).collect();

    assert_eq!(
      names,
      vec![
        "nested::deeper::tests::deeperSmoke",
        "nested::helpers_test::helperSmoke",
      ]
    );
  }

  #[test]
  fn discover_std_test_companions_uses_sorted_manifest_roots() {
    let temp_dir = TempDir::new().expect("temporary std root");
    let std_root = temp_dir.path();

    fs::create_dir_all(std_root.join("vector")).expect("create vector dir");
    fs::create_dir_all(std_root.join("string")).expect("create string dir");
    fs::create_dir_all(std_root.join("hash")).expect("create hash dir");

    fs::write(std_root.join("vector/mod.ign"), "export namespace Vector {}\n").expect("write vector module");
    fs::write(std_root.join("string/mod.ign"), "export namespace String {}\n").expect("write string module");
    fs::write(std_root.join("hash/mod.ign"), "export namespace Hash {}\n").expect("write hash module");
    fs::write(std_root.join("vector/tests.ign"), "@test\nfunction vectorSmoke(): void {}\n")
      .expect("write vector tests");
    fs::write(std_root.join("hash/tests.ign"), "@test\nfunction hashSmoke(): void {}\n").expect("write hash tests");

    let manifest = ignis_config::IgnisSTDManifest {
      modules: HashMap::from([
        ("vector".to_string(), "vector/mod.ign".to_string()),
        ("string".to_string(), "string/mod.ign".to_string()),
        ("hash".to_string(), "hash/mod.ign".to_string()),
      ]),
      ..Default::default()
    };

    let discovered = discover_std_test_companions(std_root, &manifest);

    assert_eq!(
      discovered,
      vec![std_root.join("hash/tests.ign"), std_root.join("vector/tests.ign")]
    );
  }

  #[test]
  fn validate_std_test_layout_rejects_inline_std_tests() {
    let mut symbols = SymbolTable::new();
    let mut defs = DefinitionStore::new();
    let types = TypeStore::new();

    let std_test_name = symbols.intern("inlineStdTest");
    let helper_name = symbols.intern("helper");

    alloc_test_function(&mut defs, std_test_name, ModuleId::new(0), None, &types);
    alloc_plain_function(&mut defs, helper_name, ModuleId::new(1), None, Visibility::Private, &types);

    let module_paths = HashMap::from([
      (ModuleId::new(0), ModulePath::Std("string".to_string())),
      (
        ModuleId::new(1),
        ModulePath::Project(PathBuf::from("/tmp/std/string/tests.ign")),
      ),
    ]);

    let error = validate_std_test_layout(&defs, &module_paths).expect_err("expected inline std test to be rejected");

    assert!(error.contains("std::string"), "expected module name in layout error: {error}");
    assert!(error.contains("@test"), "expected @test mention in layout error: {error}");
  }

  #[test]
  fn collect_mono_roots_skips_compile_time_only_directive_functions() {
    let mut symbols = SymbolTable::new();
    let types = TypeStore::new();
    let mut defs = DefinitionStore::new();

    let main_name = symbols.intern("main");
    let directive_name = symbols.intern("derive");

    let main_def = alloc_plain_function(&mut defs, main_name, ModuleId::new(0), None, Visibility::Private, &types);
    let directive_def =
      alloc_directive_function(&mut defs, directive_name, ModuleId::new(0), None, Visibility::Public, &types);

    let roots = super::collect_mono_roots(&defs, &symbols);

    assert!(roots.contains(&main_def), "expected main to remain a runtime root");
    assert!(
      !roots.contains(&directive_def),
      "expected compile-time-only directive functions to stay out of executable monomorphization roots"
    );
  }

  #[test]
  fn collect_mono_roots_for_std_skips_compile_time_only_directive_functions() {
    let mut symbols = SymbolTable::new();
    let types = TypeStore::new();
    let mut defs = DefinitionStore::new();

    let runtime_name = symbols.intern("runtimeHelper");
    let directive_name = symbols.intern("derive");

    let runtime_def =
      alloc_plain_function(&mut defs, runtime_name, ModuleId::new(0), None, Visibility::Private, &types);
    let directive_def =
      alloc_directive_function(&mut defs, directive_name, ModuleId::new(0), None, Visibility::Private, &types);

    let roots = super::collect_mono_roots_for_std(&defs);

    assert!(
      roots.contains(&runtime_def),
      "expected std runtime helper to remain a monomorphization root"
    );
    assert!(
      !roots.contains(&directive_def),
      "expected compile-time-only directive functions to stay out of std monomorphization roots"
    );
  }

  #[test]
  fn execute_test_harness_binary_continues_after_failures() {
    let temp_dir = TempDir::new().expect("temporary harness dir");
    let harness_path = temp_dir.path().join("fake-harness.sh");

    fs::write(
      &harness_path,
      "#!/bin/sh\nif [ \"$1\" != \"--ignis-test\" ]; then exit 2; fi\ncase \"$2\" in\n  \"math::pass\") printf \"pass\\n\"; exit 0 ;;&\n  \"math::fail\") printf \"boom\\n\" >&2; exit 101 ;;&\n  \"math::later\") printf \"later\\n\"; exit 0 ;;&\n  *) exit 2 ;;&\nesac\n",
    )
    .expect("write harness script");

    #[cfg(unix)]
    {
      use std::os::unix::fs::PermissionsExt;

      let mut permissions = fs::metadata(&harness_path).expect("script metadata").permissions();
      permissions.set_mode(0o755);
      fs::set_permissions(&harness_path, permissions).expect("mark script executable");
    }

    let results = execute_test_harness_binary(
      &harness_path,
      &TestHarnessPlan {
        tests: vec![
          TestCase {
            def_id: ignis_type::definition::DefinitionId::new(0),
            fq_name: "math::pass".to_string(),
            source_path: PathBuf::from("/tmp/project/src/math.ign"),
          },
          TestCase {
            def_id: ignis_type::definition::DefinitionId::new(1),
            fq_name: "math::fail".to_string(),
            source_path: PathBuf::from("/tmp/project/src/math.ign"),
          },
          TestCase {
            def_id: ignis_type::definition::DefinitionId::new(2),
            fq_name: "math::later".to_string(),
            source_path: PathBuf::from("/tmp/project/src/math.ign"),
          },
        ],
      },
      false,
    )
    .expect("execute harness binary");

    let statuses: Vec<(bool, i32)> = results
      .iter()
      .map(|result| (result.success, result.exit_code))
      .collect();
    assert_eq!(statuses, vec![(true, 0), (false, 101), (true, 0)]);
    assert_eq!(results[0].stdout, "pass\n");
    assert_eq!(results[1].stderr, "boom\n");
    assert_eq!(results[2].stdout, "later\n");
  }

  #[test]
  fn execute_test_harness_binary_injects_snapshot_context_env_vars() {
    let temp_dir = TempDir::new().expect("temporary harness dir");
    let harness_path = temp_dir.path().join("fake-harness.sh");

    fs::write(
      &harness_path,
      "#!/bin/sh\nif [ \"$1\" != \"--ignis-test\" ]; then exit 2; fi\nprintf '%s|%s|%s\\n' \"$IGNIS_TEST_NAME\" \"$IGNIS_TEST_SNAPSHOT_DIR\" \"$IGNIS_TEST_UPDATE_SNAPSHOTS\"\n",
    )
    .expect("write harness script");

    #[cfg(unix)]
    {
      use std::os::unix::fs::PermissionsExt;

      let mut permissions = fs::metadata(&harness_path).expect("script metadata").permissions();
      permissions.set_mode(0o755);
      fs::set_permissions(&harness_path, permissions).expect("mark script executable");
    }

    let source_path = temp_dir.path().join("project/src/math.ign");
    fs::create_dir_all(source_path.parent().expect("source dir")).expect("create source dir");
    fs::write(&source_path, "@test\nfunction math(): void {}\n").expect("write source file");

    let results = execute_test_harness_binary(
      &harness_path,
      &TestHarnessPlan {
        tests: vec![TestCase {
          def_id: ignis_type::definition::DefinitionId::new(0),
          fq_name: "math::updatesSnapshot".to_string(),
          source_path: source_path.clone(),
        }],
      },
      true,
    )
    .expect("execute harness binary");

    let snapshot_dir = source_path.parent().expect("module dir").join("__snapshots__");

    assert_eq!(
      results[0].stdout,
      format!("math::updatesSnapshot|{}|1\n", snapshot_dir.display())
    );
  }

  #[test]
  fn test_summary_snapshot_reports_project_results() {
    let results = vec![
      super::TestExecutionResult {
        fq_name: "main::passes".to_string(),
        success: true,
        exit_code: 0,
        stdout: String::new(),
        stderr: String::new(),
      },
      super::TestExecutionResult {
        fq_name: "main::fails".to_string(),
        success: false,
        exit_code: 101,
        stdout: "stdout before panic\n".to_string(),
        stderr: "panic: boom\n".to_string(),
      },
      super::TestExecutionResult {
        fq_name: "main::laterPass".to_string(),
        success: true,
        exit_code: 0,
        stdout: String::new(),
        stderr: String::new(),
      },
    ];

    assert_snapshot!("native_test_runner_summary_snapshot", format_test_summary_snapshot(&results));
  }

  #[test]
  fn failed_test_details_truncate_long_streams() {
    let result = super::TestExecutionResult {
      fq_name: "main::fails".to_string(),
      success: false,
      exit_code: 101,
      stdout: "1234567890".repeat(20),
      stderr: "abcdefghijklmnopqrstuvwxyz".repeat(20),
    };

    let details = format_failed_test_details(&result);

    assert!(
      details.contains("main::fails"),
      "expected test name in formatted failure details"
    );
    assert!(
      details.contains("stderr:"),
      "expected stderr section in formatted failure details"
    );
    assert!(
      details.contains("stdout:"),
      "expected stdout section in formatted failure details"
    );
    assert!(
      details.contains("[truncated]"),
      "expected truncation marker in formatted failure details"
    );
  }

  #[test]
  fn failed_test_details_lock_snapshot_mismatch_contract() {
    let result = super::TestExecutionResult {
      fq_name: "main::mismatchSnapshot".to_string(),
      success: false,
      exit_code: 101,
      stdout: String::new(),
      stderr: "snapshot mismatch\nsnapshot: /tmp/project/src/__snapshots__/main__rendered.snap.txt\nexpected-bytes: 12\nactual-bytes: 16\npanic: snapshot assertion failed\n".to_string(),
    };

    let details = format_failed_test_details(&result);

    assert!(
      details.contains("main::mismatchSnapshot"),
      "expected test name in snapshot failure details"
    );
    assert!(
      details.contains("snapshot mismatch"),
      "expected mismatch reason in snapshot failure details"
    );
    assert!(details.contains("snapshot: /tmp/project/src/__snapshots__/main__rendered.snap.txt"));
    assert!(details.contains("expected-bytes: 12"));
    assert!(details.contains("actual-bytes: 16"));
    assert!(details.contains("panic: snapshot assertion failed"));
  }

  fn alloc_test_function(
    defs: &mut DefinitionStore,
    name: ignis_type::symbol::SymbolId,
    owner_module: ModuleId,
    owner_namespace: Option<ignis_type::namespace::NamespaceId>,
    types: &TypeStore,
  ) -> ignis_type::definition::DefinitionId {
    defs.alloc(Definition {
      kind: DefinitionKind::Function(FunctionDefinition {
        type_params: Vec::new(),
        params: Vec::new(),
        return_type: types.void(),
        is_extern: false,
        is_variadic: false,
        inline_mode: Default::default(),
        attrs: vec![FunctionAttr::Test],
      }),
      name,
      span: Span::default(),
      name_span: Span::default(),
      visibility: Visibility::Private,
      owner_module,
      owner_namespace,
      doc: None,
    })
  }

  fn alloc_plain_function(
    defs: &mut DefinitionStore,
    name: ignis_type::symbol::SymbolId,
    owner_module: ModuleId,
    owner_namespace: Option<ignis_type::namespace::NamespaceId>,
    visibility: Visibility,
    types: &TypeStore,
  ) -> ignis_type::definition::DefinitionId {
    defs.alloc(Definition {
      kind: DefinitionKind::Function(FunctionDefinition {
        type_params: Vec::new(),
        params: Vec::new(),
        return_type: types.void(),
        is_extern: false,
        is_variadic: false,
        inline_mode: Default::default(),
        attrs: Vec::new(),
      }),
      name,
      span: Span::default(),
      name_span: Span::default(),
      visibility,
      owner_module,
      owner_namespace,
      doc: None,
    })
  }

  fn alloc_directive_function(
    defs: &mut DefinitionStore,
    name: ignis_type::symbol::SymbolId,
    owner_module: ModuleId,
    owner_namespace: Option<ignis_type::namespace::NamespaceId>,
    visibility: Visibility,
    types: &TypeStore,
  ) -> ignis_type::definition::DefinitionId {
    defs.alloc(Definition {
      kind: DefinitionKind::Function(FunctionDefinition {
        type_params: Vec::new(),
        params: Vec::new(),
        return_type: types.void(),
        is_extern: false,
        is_variadic: false,
        inline_mode: Default::default(),
        attrs: vec![FunctionAttr::Directive(DirectiveMetadata {
          target: DirectiveTarget::Record,
          phase: DirectivePhase::Check,
          effect: DirectiveEffect::Diagnose,
          group: None,
          capabilities: Vec::new(),
        })],
      }),
      name,
      span: Span::default(),
      name_span: Span::default(),
      visibility,
      owner_module,
      owner_namespace,
      doc: None,
    })
  }
}
