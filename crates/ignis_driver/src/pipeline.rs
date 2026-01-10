use std::path::Path;
use std::sync::Arc;
use std::{cell::RefCell, rc::Rc};

use colored::*;
use ignis_ast::display::format_ast_nodes;
use ignis_config::IgnisConfig;
use ignis_parser::{IgnisLexer, IgnisParser};
use ignis_type::file::SourceMap;
use ignis_type::symbol::SymbolTable;

use crate::context::CompilationContext;
use crate::link::{compile_to_object, format_tool_error, link_executable, rebuild_std_runtime, LinkPlan};

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

  if !config.quiet {
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

  if config.debug.contains(&ignis_config::DebugPrint::Lexer) {
    println!("\n{}", "Tokens:".bright_cyan().bold());
    for token in &lexer.tokens {
      println!("{}", token);
    }
  }

  let symbol_table = Rc::new(RefCell::new(SymbolTable::new()));

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

  if config.debug.contains(&ignis_config::DebugPrint::Ast) {
    println!("\n{}", "AST:".bright_cyan().bold());
    let ast_lisp = format_ast_nodes(nodes.clone(), symbol_table.clone(), &roots);
    println!("{}", ast_lisp);
  }

  if !config.quiet {
    println!("\n{}", "Running analyzer...".bright_cyan().bold());
  }

  let analyzer_result = ignis_analyzer::Analyzer::analyze(&nodes, &roots, symbol_table);

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

  let build_config = config.build_config.as_ref();

  let sym_table = analyzer_result.symbols.borrow();
  if let Some(bc) = build_config {
    if bc.dump_types {
      println!("\n{}", ignis_analyzer::dump::dump_types(&analyzer_result.types));
    }
    if bc.dump_defs {
      println!(
        "\n{}",
        ignis_analyzer::dump::dump_defs(&analyzer_result.defs, &analyzer_result.types, &sym_table)
      );
    }
    if bc.dump_hir_summary {
      println!(
        "\n{}",
        ignis_analyzer::dump::dump_hir_summary(&analyzer_result.hir, &analyzer_result.defs, &sym_table)
      );
    }
    if let Some(func_name) = &bc.dump_hir {
      match ignis_analyzer::dump::dump_hir_function(&analyzer_result.hir, &analyzer_result.defs, &sym_table, func_name)
      {
        Ok(output) => println!("\n{}", output),
        Err(err) => eprintln!("{} {}", "Error:".red().bold(), err),
      }
    }
  }

  if config.debug.contains(&ignis_config::DebugPrint::Analyzer) {
    println!("\n{}", "Type Store & Definitions:".bright_cyan().bold());
    println!("{}", ignis_analyzer::dump::dump_types(&analyzer_result.types));
    println!(
      "{}",
      ignis_analyzer::dump::dump_defs(&analyzer_result.defs, &analyzer_result.types, &sym_table)
    );
  }

  if config.debug.contains(&ignis_config::DebugPrint::Hir) {
    println!("\n{}", "HIR:".bright_cyan().bold());
    println!(
      "{}",
      ignis_analyzer::dump::dump_hir_complete(
        &analyzer_result.hir,
        &analyzer_result.types,
        &analyzer_result.defs,
        &sym_table
      )
    );
  }

  Ok(())
}

/// Compile a project with multi-module support
pub fn compile_project(
  config: Arc<IgnisConfig>,
  entry_path: &str,
) -> Result<(), ()> {
  let mut ctx = CompilationContext::new(&config);
  let root_id = ctx.discover_modules(entry_path, &config)?;
  let output = ctx.compile(root_id, &config)?;

  let sym_table = output.symbols.borrow();
  if let Some(bc) = config.build_config.as_ref() {
    if bc.dump_types {
      println!("\n{}", ignis_analyzer::dump::dump_types(&output.types));
    }
    if bc.dump_defs {
      println!("\n{}", ignis_analyzer::dump::dump_defs(&output.defs, &output.types, &sym_table));
    }
    if bc.dump_hir_summary {
      println!(
        "\n{}",
        ignis_analyzer::dump::dump_hir_summary(&output.hir, &output.defs, &sym_table)
      );
    }
    if let Some(func_name) = &bc.dump_hir {
      match ignis_analyzer::dump::dump_hir_function(&output.hir, &output.defs, &sym_table, func_name) {
        Ok(out) => println!("\n{}", out),
        Err(err) => eprintln!("{} {}", "Error:".red().bold(), err),
      }
    }
    let needs_codegen = bc.dump_lir || bc.emit_c.is_some() || bc.emit_obj.is_some() || bc.emit_bin.is_some();

    if needs_codegen {
      let used_modules = ctx.module_graph.topological_sort();

      ensure_std_built(&used_modules, &ctx.module_graph, &config)?;

      let manifest = if config.manifest.modules.is_empty() {
        None
      } else {
        Some(&config.manifest)
      };
      let link_plan = LinkPlan::from_modules(&used_modules, &ctx.module_graph, Path::new(&config.std_path), manifest);

      if bc.rebuild_std {
        if let Err(e) = rebuild_std_runtime(Path::new(&config.std_path), config.quiet) {
          eprintln!("{} {}", "Error:".red().bold(), e);
          return Err(());
        }
      }

      let mut types = output.types.clone();

      // Only emit project modules; std functions become extern declarations
      let project_modules: std::collections::HashSet<ignis_type::module::ModuleId> = used_modules
        .iter()
        .filter(|id| ctx.module_graph.modules.get(id).path.is_project())
        .copied()
        .collect();

      let (lir_program, verify_result) = ignis_lir::lowering::lower_and_verify(
        &output.hir,
        &mut types,
        &output.defs,
        &sym_table,
        Some(&project_modules),
      );

      if bc.dump_lir {
        let lir_output = ignis_lir::display::print_lir(&lir_program, &types, &output.defs, &sym_table);
        println!("\n{}", lir_output);
      }

      if let Err(errors) = &verify_result {
        eprintln!("{} LIR verification errors:", "Warning:".yellow().bold());
        for err in errors {
          eprintln!("  {:?}", err);
        }
      }

      let needs_emit = bc.emit_c.is_some() || bc.emit_obj.is_some() || bc.emit_bin.is_some();
      if needs_emit {
        if verify_result.is_err() {
          eprintln!("{} Cannot emit C: LIR verification failed", "Error:".red().bold());
          return Err(());
        }

        let c_code = ignis_codegen_c::emit_c(&lir_program, &types, &output.defs, &sym_table, &link_plan.headers);

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

        if bc.emit_c.is_some() && !config.quiet {
          println!("{} Emitted C code to {}", "-->".bright_green().bold(), c_path);
        }

        if bc.emit_obj.is_some() || bc.emit_bin.is_some() {
          let obj_path = if let Some(path) = &bc.emit_obj {
            path.clone()
          } else {
            format!("{}/{}.o", bc.output_dir, base_name)
          };

          if let Err(e) = compile_to_object(Path::new(&c_path), Path::new(&obj_path), &link_plan, config.quiet) {
            eprintln!("{} {}", "Error:".red().bold(), e);
            return Err(());
          }

          if let Some(bin_path) = &bc.emit_bin {
            if let Err(e) = link_executable(Path::new(&obj_path), Path::new(bin_path), &link_plan, config.quiet) {
              eprintln!("{} {}", "Error:".red().bold(), e);
              return Err(());
            }
          }
        }
      }
    }
  }

  if config.debug.contains(&ignis_config::DebugPrint::Analyzer) {
    println!("\n{}", "Type Store & Definitions:".bright_cyan().bold());
    println!("{}", ignis_analyzer::dump::dump_types(&output.types));
    println!("{}", ignis_analyzer::dump::dump_defs(&output.defs, &output.types, &sym_table));
  }

  if config.debug.contains(&ignis_config::DebugPrint::Hir) {
    println!("\n{}", "HIR:".bright_cyan().bold());
    println!(
      "{}",
      ignis_analyzer::dump::dump_hir_complete(&output.hir, &output.types, &output.defs, &sym_table)
    );
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

  if !config.quiet {
    println!("{} Building standard library...", "-->".bright_cyan().bold());
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
  let sym_table = output.symbols.borrow();
  let link_plan = LinkPlan::from_manifest(&config.manifest, std_path);

  let header_content = ignis_codegen_c::emit_std_header(&output.defs, &output.types, &sym_table);
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

  if !config.quiet {
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
    let (lir_program, verify_result) = ignis_lir::lowering::lower_and_verify(
      &output.hir,
      &mut types,
      &output.defs,
      &sym_table,
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
    }

    if lir_program.functions.is_empty() {
      continue;
    }

    let c_code = ignis_codegen_c::emit_c(&lir_program, &types, &output.defs, &sym_table, &link_plan.headers);

    let c_path = output_path.join(format!("{}.c", module_name));
    if let Err(e) = std::fs::write(&c_path, &c_code) {
      eprintln!("{} Failed to write C file '{}': {}", "Error:".red().bold(), c_path.display(), e);
      return Err(());
    }

    let obj_path = output_path.join(format!("{}.o", module_name));
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
  if let Err(e) = create_static_archive_multi(&object_files, &archive_path, config.quiet) {
    eprintln!("{} {}", "Error:".red().bold(), e);
    return Err(());
  }

  if !config.quiet {
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

  if !config.quiet {
    println!("{} std library not found, building...", "-->".bright_yellow().bold());
  }

  build_std(config.clone(), &std_path.join("build").to_string_lossy())
}

/// Create a static archive (.a) from multiple object files
fn create_static_archive_multi(
  objects: &[std::path::PathBuf],
  archive_path: &Path,
  quiet: bool,
) -> Result<(), String> {
  use std::process::Command;

  if !quiet {
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
