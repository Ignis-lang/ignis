use std::path::Path;
use std::sync::Arc;
use std::{cell::RefCell, rc::Rc};

use colored::*;
use ignis_ast::display::format_ast_nodes;
use ignis_config::{DebugTrace, DumpKind, IgnisConfig};

use crate::logging::{debug_trace_enabled, log_debug, log_info, log_phase, log_trace};
use ignis_parser::{IgnisLexer, IgnisParser};
use ignis_type::definition::{DefinitionId, DefinitionKind, DefinitionStore, Visibility, SymbolEntry};
use ignis_type::file::SourceMap;
use ignis_type::symbol::SymbolTable;
use ignis_diagnostics::message::DiagnosticMessage;

use crate::context::CompilationContext;
use crate::link::{compile_to_object, format_tool_error, link_executable, rebuild_std_runtime, LinkPlan};

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

  if dump_requested(config, DumpKind::C) {
    eprintln!("{} Dump kind 'c' is not supported yet.", "Warning:".yellow().bold());
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

  if log_phase(&config) {
    println!("{:indent$}Scanning... {}", "-->".bright_green().bold(), file_path, indent = 4);
  }

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

  if debug_trace_enabled(&config, DebugTrace::Lexer) {
    println!("debug: lexer produced {} tokens", lexer.tokens.len());
  }

  let symbol_table = Rc::new(RefCell::new(SymbolTable::new()));

  if log_phase(&config) {
    println!("{:indent$}Parsing... {}", "-->".bright_green().bold(), file_path, indent = 4);
  }

  if log_debug(&config) {
    println!("debug: parsing {}", file_path);
  }

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

  if debug_trace_enabled(&config, DebugTrace::Parser) {
    println!("debug: parser produced {} nodes", nodes.len());
  }

  if log_phase(&config) {
    println!("\n{}", "Running analyzer...".bright_cyan().bold());
  }

  let analyzer_result = ignis_analyzer::Analyzer::analyze(&nodes, &roots, symbol_table);

  if debug_trace_enabled(&config, DebugTrace::Analyzer) {
    println!("debug: analyzer emitted {} diagnostics", analyzer_result.diagnostics.len());
  }

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
  warn_unsupported_dumps(&config);

  if log_phase(&config) {
    println!("{} Discovering modules...", "-->".bright_cyan().bold());
  }

  let mut ctx = CompilationContext::new(&config);
  let root_id = ctx.discover_modules(entry_path, &config)?;

  if log_debug(&config) {
    println!("debug: compiling project entry {}", entry_path);
  }

  let output = ctx.compile(root_id, &config)?;

  if debug_trace_enabled(&config, DebugTrace::Analyzer) {
    println!("debug: compilation produced {} diagnostics", output.diagnostics.len());
  }

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

    if is_bin && !has_entry_point {
      let root_module = ctx.module_graph.modules.get(&root_id);
      let file_id = root_module.file_id;
      let span = ignis_type::span::Span::empty_at(file_id, ignis_type::BytePosition::default());
      ignis_diagnostics::render(
        &DiagnosticMessage::ExecutableMustHaveMainFunction { span }.report(),
        &ctx.source_map,
      );
      return Err(());
    }

    if is_lib && has_entry_point {
      let entry_def_id = output.hir.entry_point.unwrap();
      let span = output.defs.get(&entry_def_id).span.clone();
      ignis_diagnostics::render(
        &DiagnosticMessage::LibraryCannotHaveMainFunction { span }.report(),
        &ctx.source_map,
      );
      return Err(());
    }

    let needs_codegen = dump_requested(&config, DumpKind::Lir)
      || bc.emit_c.is_some()
      || bc.emit_obj.is_some()
      || bc.emit_bin.is_some()
      || bc.lib;

    if needs_codegen {
      let used_modules = ctx.module_graph.topological_sort();

      if log_phase(&config) {
        println!("{} Lowering + codegen...", "-->".bright_cyan().bold());
      }

      if log_debug(&config) {
        println!("debug: preparing codegen for {} modules", used_modules.len());
      }

      if log_trace(&config) {
        println!("trace: codegen module order {:?}", used_modules);
      }

      if debug_trace_enabled(&config, DebugTrace::Std) {
        println!("debug: ensuring standard library is built");
      }

      ensure_std_built(&used_modules, &ctx.module_graph, &config)?;

      let manifest = if config.manifest.modules.is_empty() {
        None
      } else {
        Some(&config.manifest)
      };
      let link_plan = LinkPlan::from_modules(&used_modules, &ctx.module_graph, Path::new(&config.std_path), manifest);

      if bc.rebuild_std {
        if debug_trace_enabled(&config, DebugTrace::Std) {
          println!("debug: rebuilding standard library runtime");
        }

        if let Err(e) = rebuild_std_runtime(Path::new(&config.std_path), config.quiet) {
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

      if debug_trace_enabled(&config, DebugTrace::Mono) {
        println!("debug: monomorphization completed");
      }

      if config.debug {
        mono_output.verify_no_generics(&types);
      } else {
        // In debug builds, verify no generic types remain
        #[cfg(debug_assertions)]
        mono_output.verify_no_generics(&types);
      }

      // Re-borrow symbols for ownership checking and codegen
      let sym_table = output.symbols.borrow();

      let ownership_checker =
        ignis_analyzer::HirOwnershipChecker::new(&mono_output.hir, &types, &mono_output.defs, &sym_table)
          .with_source_map(&ctx.source_map)
          .suppress_ffi_warnings_for(&config.std_path);
      let (drop_schedules, ownership_diagnostics) = ownership_checker.check();

      if debug_trace_enabled(&config, DebugTrace::Ownership) {
        println!("debug: ownership check produced {} diagnostics", ownership_diagnostics.len());
      }

      if !config.quiet {
        for diag in &ownership_diagnostics {
          ignis_diagnostics::render(diag, &ctx.source_map);
        }
      }

      let has_errors = ownership_diagnostics
        .iter()
        .any(|d| matches!(d.severity, ignis_diagnostics::diagnostic_report::Severity::Error));

      if has_errors {
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

      if debug_trace_enabled(&config, DebugTrace::Lir) {
        println!("debug: LIR lowering completed");
      }

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
          eprintln!("{} LIR verification failed in debug mode", "Error:".red().bold());
          return Err(());
        }
      }

      let needs_emit = bc.emit_c.is_some() || bc.emit_obj.is_some() || bc.emit_bin.is_some() || bc.lib;
      if needs_emit {
        if debug_trace_enabled(&config, DebugTrace::Codegen) {
          println!("debug: emitting C code");
        }

        if verify_result.is_err() {
          eprintln!("{} Cannot emit C: LIR verification failed", "Error:".red().bold());
          return Err(());
        }

        let c_code = ignis_codegen_c::emit_c(
          &lir_program,
          &types,
          &mono_output.defs,
          &output.namespaces,
          &sym_table,
          &link_plan.headers,
        );

        let base_name = bc
          .file
          .as_ref()
          .and_then(|f| Path::new(f).file_stem())
          .and_then(|s| s.to_str())
          .unwrap_or("out");

        let c_path = if let Some(path) = &bc.emit_c {
          path.clone()
        } else {
          let output_dir = Path::new(&bc.output_dir);
          if !output_dir.exists() {
            if let Err(e) = std::fs::create_dir_all(output_dir) {
              eprintln!(
                "{} Failed to create output directory '{}': {}",
                "Error:".red().bold(),
                bc.output_dir,
                e
              );
              return Err(());
            }
          }
          format!("{}/{}.c", bc.output_dir, base_name)
        };

        if let Err(e) = std::fs::write(&c_path, &c_code) {
          eprintln!("{} Failed to write C file '{}': {}", "Error:".red().bold(), c_path, e);
          return Err(());
        }

        if bc.emit_c.is_some() && log_info(&config) {
          println!("{} Emitted C code to {}", "-->".bright_green().bold(), c_path);
        }

        if bc.emit_obj.is_some() || bc.emit_bin.is_some() || bc.lib {
          let obj_path = if let Some(path) = &bc.emit_obj {
            path.clone()
          } else {
            format!("{}/{}.o", bc.output_dir, base_name)
          };

          if debug_trace_enabled(&config, DebugTrace::Link) {
            println!("debug: compiling object file");
          }

          if let Err(e) = compile_to_object(Path::new(&c_path), Path::new(&obj_path), &link_plan, config.quiet) {
            eprintln!("{} {}", "Error:".red().bold(), e);
            return Err(());
          }

          if let Some(bin_path) = &bc.emit_bin {
            if debug_trace_enabled(&config, DebugTrace::Link) {
              println!("debug: linking executable");
            }

            if let Err(e) = link_executable(Path::new(&obj_path), Path::new(bin_path), &link_plan, config.quiet) {
              eprintln!("{} {}", "Error:".red().bold(), e);
              return Err(());
            }
          }
        }
      }
    }
  }

  Ok(())
}

/// Build the standard library into a static archive
pub fn build_std(
  config: Arc<IgnisConfig>,
  output_dir: &str,
) -> Result<(), ()> {
  use std::collections::HashSet;
  use ignis_type::module::ModuleId;

  if config.std_path.is_empty() {
    eprintln!(
      "{} std_path not set. Use --std-path or set IGNIS_STD_PATH env var",
      "Error:".red().bold()
    );
    return Err(());
  }

  let std_path = Path::new(&config.std_path);

  if !std_path.exists() {
    eprintln!("{} std_path '{}' does not exist", "Error:".red().bold(), std_path.display());
    return Err(());
  }

  if config.manifest.modules.is_empty() {
    eprintln!(
      "{} No modules found in std manifest at '{}/manifest.toml'",
      "Error:".red().bold(),
      std_path.display()
    );
    return Err(());
  }

  if debug_trace_enabled(&config, DebugTrace::Std) {
    println!("debug: building standard library");
  }

  if log_phase(&config) {
    println!("{} Building standard library...", "-->".bright_cyan().bold());
  }

  if log_debug(&config) {
    println!("debug: std manifest modules {}", config.manifest.modules.len());
  }

  let output_path = Path::new(output_dir);
  if !output_path.exists() {
    if let Err(e) = std::fs::create_dir_all(output_path) {
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
      eprintln!("{} Failed to discover std module '{}'", "Error:".red().bold(), module_name);
      return Err(());
    }
  }

  let output = ctx.compile_all(&config)?;
  let link_plan = LinkPlan::from_manifest(&config.manifest, std_path);

  // Use scoped borrow for header generation, drops before monomorphization
  let header_content = {
    let sym_table = output.symbols.borrow();
    ignis_codegen_c::emit_std_header(&output.defs, &output.types, &sym_table)
  };
  let header_path = output_path.join("ignis_std.h");
  if let Err(e) = std::fs::write(&header_path, &header_content) {
    eprintln!(
      "{} Failed to write header file '{}': {}",
      "Error:".red().bold(),
      header_path.display(),
      e
    );
    return Err(());
  }

  if log_phase(&config) {
    println!("{} Generated header {}", "-->".bright_green().bold(), header_path.display());
  }

  let all_module_ids = ctx.module_graph.all_modules_topological();
  let mut object_files: Vec<std::path::PathBuf> = Vec::new();
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

    // Monomorphization: use temporary borrow for collect_mono_roots
    let mono_roots = collect_mono_roots(&output.defs, &output.symbols.borrow());
    let mono_output =
      ignis_analyzer::mono::Monomorphizer::new(&output.hir, &output.defs, &mut types, output.symbols.clone())
        .run(&mono_roots);

    if debug_trace_enabled(&config, DebugTrace::Mono) {
      println!("debug: std monomorphization completed for module {}", module_name);
    }

    if config.debug {
      mono_output.verify_no_generics(&types);
    } else {
      #[cfg(debug_assertions)]
      mono_output.verify_no_generics(&types);
    }

    // Re-borrow symbols for ownership checking and codegen
    let sym_table = output.symbols.borrow();

    // Run ownership analysis on HIR to produce drop schedules
    let ownership_checker =
      ignis_analyzer::HirOwnershipChecker::new(&mono_output.hir, &types, &mono_output.defs, &sym_table)
        .with_source_map(&ctx.source_map)
        .suppress_ffi_warnings_for(&config.std_path);
    let (drop_schedules, _ownership_diagnostics) = ownership_checker.check();
    // Note: ownership diagnostics already reported in the first pass

    let (lir_program, verify_result) = ignis_lir::lowering::lower_and_verify(
      &mono_output.hir,
      &mut types,
      &mono_output.defs,
      &sym_table,
      &drop_schedules,
      Some(&single_module_set),
    );

    if debug_trace_enabled(&config, DebugTrace::Lir) {
      println!("debug: std LIR lowering completed for module {}", module_name);
    }

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
        eprintln!("{} LIR verification failed in debug mode", "Error:".red().bold());
        return Err(());
      }
    }

    if lir_program.functions.is_empty() {
      continue;
    }

    if debug_trace_enabled(&config, DebugTrace::Codegen) {
      println!("debug: std emitting C for module {}", module_name);
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
      eprintln!("{} Failed to write C file '{}': {}", "Error:".red().bold(), c_path.display(), e);
      return Err(());
    }

    let obj_path = output_path.join(format!("{}.o", module_name));
    if debug_trace_enabled(&config, DebugTrace::Link) {
      println!("debug: std compiling object for module {}", module_name);
    }

    if let Err(e) = compile_to_object(&c_path, &obj_path, &link_plan, config.quiet) {
      eprintln!("{} {}", "Error:".red().bold(), e);
      return Err(());
    }

    object_files.push(obj_path);
  }

  if object_files.is_empty() {
    eprintln!("{} No object files generated", "Error:".red().bold());
    return Err(());
  }

  let archive_path = output_path.join("libignis_std.a");
  if debug_trace_enabled(&config, DebugTrace::Link) {
    println!("debug: archiving standard library objects");
  }

  if let Err(e) = create_static_archive_multi(&object_files, &archive_path, log_phase(&config)) {
    eprintln!("{} {}", "Error:".red().bold(), e);
    return Err(());
  }

  if log_phase(&config) {
    println!("{} Build complete: {}", "-->".bright_green().bold(), archive_path.display());
  }

  Ok(())
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

  let std_path = Path::new(&config.std_path);
  let archive_path = std_path.join("build/libignis_std.a");

  if archive_path.exists() {
    return Ok(());
  }

  if log_phase(config) {
    println!("{} std library not found, building...", "-->".bright_yellow().bold());
  }

  build_std(config.clone(), &std_path.join("build").to_string_lossy())
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
    println!(
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
