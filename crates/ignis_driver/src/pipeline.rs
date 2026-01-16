use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::Arc;
use std::time::Instant;
use std::{cell::RefCell, rc::Rc};

use colored::*;
use ignis_ast::display::format_ast_nodes;
use ignis_config::{DebugTrace, DumpKind, IgnisConfig};

use ignis_log::{
  cmd_artifact, cmd_fail, cmd_header, cmd_ok, cmd_stats, log_dbg, log_phase, log_trc, phase_log, phase_ok, phase_warn,
  section, section_item, trace_dbg,
};
use ignis_parser::{IgnisLexer, IgnisParser};
use ignis_type::definition::{DefinitionId, DefinitionKind, DefinitionStore, Visibility, SymbolEntry};
use ignis_type::file::SourceMap;
use ignis_type::module::{ModuleId, ModulePath};
use ignis_type::symbol::SymbolTable;
use ignis_diagnostics::message::DiagnosticMessage;

use crate::build_layout::{
  hash_file, is_module_stamp_valid, is_std_stamp_valid, write_if_changed, write_module_stamp, write_std_stamp,
  BuildFingerprint, BuildLayout, FileEntry, ModuleStamp, StdStamp,
};
use crate::context::CompilationContext;
use crate::link::{
  compile_to_object, format_tool_error, link_executable, link_executable_multi, rebuild_std_runtime, LinkPlan,
};

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
  if let Some(build_config) = config.build_config.as_ref() {
    if let Some(dump_dir) = &build_config.dump_dir {
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
  }

  println!("\n{}", content);
  Ok(())
}

/// Compile a single file (used for simple single-file compilation without imports)
pub fn compile_file(
  config: Arc<IgnisConfig>,
  file_path: &str,
) -> Result<(), ()> {
  let mut sm = SourceMap::new();

  let text = match std::fs::read_to_string(file_path) {
    Ok(content) => content,
    Err(e) => {
      eprintln!("{} Failed to read file '{}': {}", "Error:".red().bold(), file_path, e);
      return Err(());
    },
  };

  warn_unsupported_dumps(&config);

  phase_log!(&config, "Scanning... {}", file_path);

  let file_id = sm.add_file(file_path, text);
  let src = &sm.get(&file_id).text;

  let mut lexer = IgnisLexer::new(file_id, src);
  lexer.scan_tokens();

  if !lexer.diagnostics.is_empty() {
    for diag_msg in &lexer.diagnostics {
      let diag = diag_msg.report();
      ignis_diagnostics::render(&diag, &sm);
    }
    return Err(());
  }

  if dump_requested(&config, DumpKind::Lexer) {
    let mut output = String::new();
    for token in &lexer.tokens {
      output.push_str(&format!("{}\n", token));
    }

    write_dump_output(&config, "dump-lexer.txt", &output)?;
  }

  trace_dbg!(&config, DebugTrace::Lexer, "produced {} tokens", lexer.tokens.len());

  let symbol_table = Rc::new(RefCell::new(SymbolTable::new()));

  phase_log!(&config, "Parsing... {}", file_path);

  log_dbg!(&config, "parsing {}", file_path);

  let mut parser = IgnisParser::new(lexer.tokens, symbol_table.clone());
  let parse_result = parser.parse();
  if parse_result.is_err() {
    let err = parse_result.unwrap_err();
    for diag_msg in &err {
      let diag = diag_msg.report();
      ignis_diagnostics::render(&diag, &sm);
    }
    return Err(());
  }

  let (nodes, roots) = parse_result.unwrap();

  if dump_requested(&config, DumpKind::Ast) {
    let ast_lisp = format_ast_nodes(nodes.clone(), symbol_table.clone(), &roots);
    write_dump_output(&config, "dump-ast.txt", &ast_lisp)?;
  }

  trace_dbg!(&config, DebugTrace::Parser, "produced {} nodes", nodes.len());

  phase_ok!(&config, "{}", "Running analyzer...".bright_cyan().bold());

  let analyzer_result = ignis_analyzer::Analyzer::analyze(&nodes, &roots, symbol_table);

  trace_dbg!(
    &config,
    DebugTrace::Analyzer,
    "emitted {} diagnostics",
    analyzer_result.diagnostics.len()
  );

  if !config.quiet {
    for diag in &analyzer_result.diagnostics {
      ignis_diagnostics::render(diag, &sm);
    }
  }

  let has_errors = analyzer_result
    .diagnostics
    .iter()
    .any(|d| matches!(d.severity, ignis_diagnostics::diagnostic_report::Severity::Error));

  if has_errors {
    return Err(());
  }

  let sym_table = analyzer_result.symbols.borrow();

  if dump_requested(&config, DumpKind::Types) {
    let types_dump = ignis_analyzer::dump::dump_types(&analyzer_result.types);
    write_dump_output(&config, "dump-types.txt", &types_dump)?;
  }

  if dump_requested(&config, DumpKind::Defs) {
    let defs_dump = ignis_analyzer::dump::dump_defs(&analyzer_result.defs, &analyzer_result.types, &sym_table);
    write_dump_output(&config, "dump-defs.txt", &defs_dump)?;
  }

  if dump_requested(&config, DumpKind::HirSummary) {
    let summary_dump = ignis_analyzer::dump::dump_hir_summary(&analyzer_result.hir, &analyzer_result.defs, &sym_table);
    write_dump_output(&config, "dump-hir-summary.txt", &summary_dump)?;
  }

  if let Some(build_config) = config.build_config.as_ref() {
    if let Some(func_name) = &build_config.dump_hir {
      match ignis_analyzer::dump::dump_hir_function(&analyzer_result.hir, &analyzer_result.defs, &sym_table, func_name)
      {
        Ok(output) => {
          let file_name = format!("dump-hir-{}.txt", sanitize_dump_name(func_name));
          write_dump_output(&config, &file_name, &output)?;
        },
        Err(err) => eprintln!("{} {}", "Error:".red().bold(), err),
      }
    }
  }

  if dump_requested(&config, DumpKind::Hir) {
    let hir_dump = ignis_analyzer::dump::dump_hir_complete(
      &analyzer_result.hir,
      &analyzer_result.types,
      &analyzer_result.defs,
      &sym_table,
    );
    write_dump_output(&config, "dump-hir.txt", &hir_dump)?;
  }

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

  let std_module_names: Vec<String> = ctx
    .module_graph
    .modules
    .iter()
    .filter(|(_, m)| m.path.is_std())
    .map(|(_, m)| m.path.module_name())
    .collect();
  let user_module_names: Vec<String> = ctx
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

  let output = match ctx.compile(root_id, &config) {
    Ok(o) => o,
    Err(()) => {
      cmd_fail!(
        &config,
        if is_check_mode { "Check failed" } else { "Build failed" },
        start.elapsed()
      );
      return Err(());
    },
  };

  trace_dbg!(
    &config,
    DebugTrace::Analyzer,
    "compilation produced {} diagnostics",
    output.diagnostics.len()
  );

  // Dump phase: use a scoped borrow that ends before monomorphization
  {
    let sym_table = output.symbols.borrow();

    if dump_requested(&config, DumpKind::Types) {
      let types_dump = ignis_analyzer::dump::dump_types(&output.types);
      write_dump_output(&config, "dump-types.txt", &types_dump)?;
    }

    if dump_requested(&config, DumpKind::Defs) {
      let defs_dump = ignis_analyzer::dump::dump_defs(&output.defs, &output.types, &sym_table);
      write_dump_output(&config, "dump-defs.txt", &defs_dump)?;
    }

    if dump_requested(&config, DumpKind::HirSummary) {
      let summary_dump = ignis_analyzer::dump::dump_hir_summary(&output.hir, &output.defs, &sym_table);
      write_dump_output(&config, "dump-hir-summary.txt", &summary_dump)?;
    }

    if let Some(build_config) = config.build_config.as_ref() {
      if let Some(func_name) = &build_config.dump_hir {
        match ignis_analyzer::dump::dump_hir_function(&output.hir, &output.defs, &sym_table, func_name) {
          Ok(out) => {
            let file_name = format!("dump-hir-{}.txt", sanitize_dump_name(func_name));
            write_dump_output(&config, &file_name, &out)?;
          },
          Err(err) => eprintln!("{} {}", "Error:".red().bold(), err),
        }
      }
    }

    if dump_requested(&config, DumpKind::Hir) {
      let hir_dump = ignis_analyzer::dump::dump_hir_complete(&output.hir, &output.types, &output.defs, &sym_table);
      write_dump_output(&config, "dump-hir.txt", &hir_dump)?;
    }
  }

  if let Some(bc) = config.build_config.as_ref() {
    let has_entry_point = output.hir.entry_point.is_some();
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
      let entry_def_id = output.hir.entry_point.unwrap();
      let span = output.defs.get(&entry_def_id).span.clone();
      cmd_fail!(&config, "Build failed", start.elapsed());
      ignis_diagnostics::render(
        &DiagnosticMessage::LibraryCannotHaveMainFunction { span }.report(),
        &ctx.source_map,
      );
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
      let link_plan = LinkPlan::from_modules(
        &used_modules,
        &ctx.module_graph,
        Path::new(&config.std_path),
        Path::new(&build_dir),
        manifest,
      );

      if bc.rebuild_std {
        trace_dbg!(&config, DebugTrace::Std, "rebuilding standard library runtime");

        if let Err(e) = rebuild_std_runtime(Path::new(&config.std_path), config.quiet) {
          cmd_fail!(&config, "Build failed", start.elapsed());
          eprintln!("{} {}", "Error:".red().bold(), e);
          return Err(());
        }
      }

      let mut types = output.types.clone();

      // Monomorphization: transform generic HIR into concrete HIR
      // Use a temporary borrow for collect_mono_roots that drops before Monomorphizer::run()
      let mono_roots = collect_mono_roots(&output.defs, &output.symbols.borrow());
      let mono_output =
        ignis_analyzer::mono::Monomorphizer::new(&output.hir, &output.defs, &mut types, output.symbols.clone())
          .run(&mono_roots);

      trace_dbg!(&config, DebugTrace::Mono, "monomorphization completed");

      // In debug builds, verify no generic types remain
      #[cfg(debug_assertions)]
      mono_output.verify_no_generics(&types);

      // Re-borrow symbols for ownership checking and codegen
      let sym_table = output.symbols.borrow();

      let ownership_checker =
        ignis_analyzer::HirOwnershipChecker::new(&mono_output.hir, &types, &mono_output.defs, &sym_table)
          .with_source_map(&ctx.source_map)
          .suppress_ffi_warnings_for(&config.std_path);
      let (drop_schedules, ownership_diagnostics) = ownership_checker.check();

      trace_dbg!(
        &config,
        DebugTrace::Ownership,
        "check produced {} diagnostics",
        ownership_diagnostics.len()
      );

      if !config.quiet {
        for diag in &ownership_diagnostics {
          ignis_diagnostics::render(diag, &ctx.source_map);
        }
      }

      let error_count = ownership_diagnostics
        .iter()
        .filter(|d| matches!(d.severity, ignis_diagnostics::diagnostic_report::Severity::Error))
        .count();
      let warning_count = ownership_diagnostics
        .iter()
        .filter(|d| matches!(d.severity, ignis_diagnostics::diagnostic_report::Severity::Warning))
        .count();

      if error_count > 0 {
        cmd_fail!(
          &config,
          if check_mode { "Check failed" } else { "Build failed" },
          start.elapsed()
        );
        cmd_stats!(&config, error_count, warning_count);
        return Err(());
      }

      // TODO: Filter to project_modules only when std is pre-compiled
      let used_module_set: std::collections::HashSet<ignis_type::module::ModuleId> =
        used_modules.iter().copied().collect();
      let (lir_program, verify_result) = ignis_lir::lowering::lower_and_verify(
        &mono_output.hir,
        &mut types,
        &mono_output.defs,
        &sym_table,
        &drop_schedules,
        Some(&used_module_set),
      );

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
          let layout = BuildLayout::new(base_name, Path::new(&bc.output_dir));

          // Create user directories
          if let Err(e) = layout.create_user_dirs() {
            cmd_fail!(&config, "Build failed", start.elapsed());
            eprintln!("{} Failed to create user directories: {}", "Error:".red().bold(), e);
            return Err(());
          }

          // Collect user modules (non-std)
          let user_modules: Vec<_> = used_modules
            .iter()
            .filter(|id| {
              let module = ctx.module_graph.modules.get(id);
              module.path.is_project()
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
            let header_content = ignis_codegen_c::emit_user_module_h(
              **module_id,
              &source_path,
              &mono_output.defs,
              &types,
              &sym_table,
              &output.namespaces,
            );

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
          let umbrella_content = generate_user_umbrella_header(&source_paths);
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

            // Build per-module headers: self header + transitive dependency headers
            let mut user_module_headers: Vec<ignis_config::CHeader> = Vec::new();

            // Add own header first
            let self_header_path = source_path.with_extension("h");
            user_module_headers.push(ignis_config::CHeader {
              path: normalize_path_for_include(&self_header_path),
              quoted: true,
            });

            // Add transitive dependency headers (user modules only)
            for dep_id in &dep_ids {
              let dep_module = ctx.module_graph.modules.get(dep_id);
              if let ModulePath::Project(dep_path) = &dep_module.path {
                let dep_header_path = dep_path.with_extension("h");
                user_module_headers.push(ignis_config::CHeader {
                  path: normalize_path_for_include(&dep_header_path),
                  quoted: true,
                });
              }
            }

            // Generate C code for this module
            let c_code = ignis_codegen_c::emit_user_module_c(
              **module_id,
              &lir_program,
              &types,
              &mono_output.defs,
              &output.namespaces,
              &sym_table,
              &link_plan_with_user_includes.headers,
              &module_paths,
              &user_module_headers,
            );

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
            ignis_codegen_c::emit_user_c(
              &lir_program,
              &types,
              &mono_output.defs,
              &output.namespaces,
              &sym_table,
              &link_plan.headers,
              &module_paths,
            )
          } else {
            ignis_codegen_c::emit_c(
              &lir_program,
              &types,
              &mono_output.defs,
              &output.namespaces,
              &sym_table,
              &link_plan.headers,
            )
          };

          if dump_c_requested {
            write_dump_output(&config, "dump-c.c", &c_code)?;
          }

          let c_path = if let Some(path) = &bc.emit_c {
            PathBuf::from(path)
          } else {
            let output_dir = Path::new(&bc.output_dir);
            if !output_dir.exists() {
              if let Err(e) = std::fs::create_dir_all(output_dir) {
                cmd_fail!(&config, "Build failed", start.elapsed());
                eprintln!(
                  "{} Failed to create output directory '{}': {}",
                  "Error:".red().bold(),
                  bc.output_dir,
                  e
                );
                return Err(());
              }
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

  for (module_name, _) in &config.manifest.modules {
    if let Err(()) = ctx.discover_std_module(module_name, &config) {
      cmd_fail!(&config, "Build failed", start.elapsed());
      eprintln!("{} Failed to discover std module '{}'", "Error:".red().bold(), module_name);
      return Err(());
    }
  }

  section!(&config, "Analyzing");

  let output = ctx.compile_all(&config)?;
  let mut link_plan = LinkPlan::from_manifest(&config.manifest, std_path);

  // Add std include directory for std module inter-dependencies
  // This allows std module C files to #include "ignis_std.h"
  link_plan.include_dirs.push(layout.std_include_dir());

  // Build module paths map for classification
  let module_paths = build_module_paths_from_graph(&ctx.module_graph);

  section!(&config, "Codegen & linking");

  let all_module_ids = ctx.module_graph.all_modules_topological();
  let mut processed_modules: HashSet<String> = HashSet::new();
  let mut generated_module_names: Vec<String> = Vec::new();

  // Phase 1: Generate all module headers first
  // We need to do a first pass to just generate headers, then umbrella,
  // then C files. This is necessary because std modules can depend on each other.
  for module_id in &all_module_ids {
    let module = ctx.module_graph.modules.get(module_id);
    let module_name = module.path.module_name();

    if processed_modules.contains(&module_name) {
      continue;
    }
    processed_modules.insert(module_name.clone());

    let single_module_set: HashSet<ModuleId> = [*module_id].into_iter().collect();
    let mut types = output.types.clone();

    let mono_roots = collect_mono_roots_for_std(&output.defs);
    let mono_output =
      ignis_analyzer::mono::Monomorphizer::new(&output.hir, &output.defs, &mut types, output.symbols.clone())
        .run(&mono_roots);

    #[cfg(debug_assertions)]
    mono_output.verify_no_generics(&types);

    let sym_table = output.symbols.borrow();

    let ownership_checker =
      ignis_analyzer::HirOwnershipChecker::new(&mono_output.hir, &types, &mono_output.defs, &sym_table)
        .with_source_map(&ctx.source_map)
        .suppress_ffi_warnings_for(&config.std_path);
    let (drop_schedules, _ownership_diagnostics) = ownership_checker.check();

    let (lir_program, _verify_result) = ignis_lir::lowering::lower_and_verify(
      &mono_output.hir,
      &mut types,
      &mono_output.defs,
      &sym_table,
      &drop_schedules,
      Some(&single_module_set),
    );

    if lir_program.functions.is_empty() {
      continue;
    }

    // Generate module header
    let header_content = ignis_codegen_c::emit_std_module_h(
      &module_name,
      &mono_output.defs,
      &types,
      &sym_table,
      &output.namespaces,
      &module_paths,
    );

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
    let mono_output =
      ignis_analyzer::mono::Monomorphizer::new(&output.hir, &output.defs, &mut types, output.symbols.clone())
        .run(&mono_roots);

    let sym_table = output.symbols.borrow();

    let ownership_checker =
      ignis_analyzer::HirOwnershipChecker::new(&mono_output.hir, &types, &mono_output.defs, &sym_table)
        .with_source_map(&ctx.source_map)
        .suppress_ffi_warnings_for(&config.std_path);
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
    let c_code = ignis_codegen_c::emit_std_module_c(
      &module_name,
      &lir_program,
      &types,
      &mono_output.defs,
      &output.namespaces,
      &sym_table,
      &link_plan.headers,
      &module_paths,
      Some(umbrella_header),
    );

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
  if !output_path.exists() {
    if let Err(e) = std::fs::create_dir_all(output_path) {
      cmd_fail!(&config, "Check failed", start.elapsed());
      eprintln!(
        "{} Failed to create output directory '{}': {}",
        "Error:".red().bold(),
        output_path.display(),
        e
      );
      return Err(());
    }
  }

  let mut ctx = CompilationContext::new(&config);

  for (module_name, _) in &config.manifest.modules {
    if let Err(()) = ctx.discover_std_module(module_name, &config) {
      cmd_fail!(&config, "Check failed", start.elapsed());
      eprintln!("{} Failed to discover std module '{}'", "Error:".red().bold(), module_name);
      return Err(());
    }
  }

  section!(&config, "Analyzing");

  let output = ctx.compile_all(&config)?;
  let link_plan = LinkPlan::from_manifest(&config.manifest, std_path);

  section!(&config, "Codegen (check)");

  let header_content = {
    let sym_table = output.symbols.borrow();
    ignis_codegen_c::emit_std_header(&output.defs, &output.types, &sym_table)
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

  let all_module_ids = ctx.module_graph.all_modules_topological();
  let mut processed_modules: HashSet<String> = HashSet::new();

  for module_id in &all_module_ids {
    let module = ctx.module_graph.modules.get(module_id);
    let module_name = module.path.module_name();

    if processed_modules.contains(&module_name) {
      continue;
    }
    processed_modules.insert(module_name.clone());

    let single_module_set: HashSet<ModuleId> = [*module_id].into_iter().collect();
    let mut types = output.types.clone();

    let mono_roots = collect_mono_roots_for_std(&output.defs);
    let mono_output =
      ignis_analyzer::mono::Monomorphizer::new(&output.hir, &output.defs, &mut types, output.symbols.clone())
        .run(&mono_roots);

    #[cfg(debug_assertions)]
    mono_output.verify_no_generics(&types);

    let sym_table = output.symbols.borrow();

    let ownership_checker =
      ignis_analyzer::HirOwnershipChecker::new(&mono_output.hir, &types, &mono_output.defs, &sym_table)
        .with_source_map(&ctx.source_map)
        .suppress_ffi_warnings_for(&config.std_path);
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

    let c_code = ignis_codegen_c::emit_c(
      &lir_program,
      &types,
      &mono_output.defs,
      &output.namespaces,
      &sym_table,
      &link_plan.headers,
    );

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
      } else if let Some(ext) = path.extension() {
        if ext == "c" {
          out.push(path);
        }
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

/// Create a static archive (.a) from multiple object files
fn create_static_archive_multi(
  objects: &[std::path::PathBuf],
  archive_path: &Path,
  log_info: bool,
) -> Result<(), String> {
  use std::process::Command;

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
      DefinitionKind::Function(fd) if !fd.is_extern => {
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
  writeln!(out, "#include \"runtime/types/types.h\"").unwrap();
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
  writeln!(out, "#include \"runtime/types/types.h\"").unwrap();
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
