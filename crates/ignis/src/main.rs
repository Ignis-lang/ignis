mod cli;
mod init;

use std::io::BufRead;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use clap::Parser as ClapParser;
use colored::*;

use cli::{BuildCommand, CheckCommand, Cli, FmtCommand, SubCommand, TestCommand, TestStdCommand};
use ignis_config::{IgnisBuildConfig, IgnisConfig, IgnisSTDManifest};
use ignis_driver::{
  build_std, check_runtime, check_std, compile_project, find_project_root, load_project_toml, resolve_project,
  run_project_tests, run_single_file_tests, run_std_tests, CliOverrides, Project,
};
use ignis_formatter::{FormatOptions, FormatterCliOverrides, FormatterConfigPaths, format_file, load_formatter_config};
use init::run_init;

// =============================================================================
// Compile Input Resolution
// =============================================================================

/// What we're compiling: either a project or a single file.
#[allow(clippy::large_enum_variant)]
enum CompileInput {
  /// Project mode: use ignis.toml configuration.
  Project(Project),

  /// Single-file mode: compile one file without project context.
  SingleFile(PathBuf),
}

#[derive(Debug, Clone)]
#[allow(clippy::large_enum_variant)]
enum TestInput {
  Project(Project),
  SingleFile(PathBuf),
}

#[derive(Debug, Clone)]
#[allow(clippy::large_enum_variant)]
enum FmtInput {
  Project(Project),
  ExplicitFiles(Vec<PathBuf>),
}

/// Resolve what to compile based on CLI arguments.
///
/// Priority:
/// 1. `--project <dir>` → use that project
/// 2. `file_path` argument → single-file mode
/// 3. No arguments → search for ignis.toml from cwd
fn resolve_compile_input(
  file_path: &Option<String>,
  project_arg: &Option<String>,
  overrides: &CliOverrides,
) -> Result<CompileInput, ()> {
  // 1. Explicit --project takes priority
  if let Some(dir) = project_arg {
    let root = PathBuf::from(dir);
    let toml_path = root.join("ignis.toml");

    if !toml_path.exists() {
      eprintln!("{} No ignis.toml found in '{}'", "Error:".red().bold(), root.display());
      return Err(());
    }

    let project = load_and_resolve_project(&root, overrides)?;
    return Ok(CompileInput::Project(project));
  }

  // 2. If file_path is given → single-file mode
  if let Some(path_str) = file_path {
    let path = PathBuf::from(path_str);

    if !path.exists() {
      eprintln!("{} File not found: '{}'", "Error:".red().bold(), path.display());
      return Err(());
    }

    return Ok(CompileInput::SingleFile(path));
  }

  // 3. No arguments → search for project from cwd
  let cwd = std::env::current_dir().map_err(|e| {
    eprintln!("{} Failed to get current directory: {}", "Error:".red().bold(), e);
  })?;

  match find_project_root(&cwd) {
    Some(root) => {
      let project = load_and_resolve_project(&root, overrides)?;
      Ok(CompileInput::Project(project))
    },
    None => {
      eprintln!("{} No ignis.toml found", "Error:".red().bold());
      eprintln!("Hint: Run 'ignis init' to create a project, or provide a file path.");
      Err(())
    },
  }
}

/// Load ignis.toml and resolve to a Project.
fn load_and_resolve_project(
  root: &Path,
  overrides: &CliOverrides,
) -> Result<Project, ()> {
  let toml_path = root.join("ignis.toml");

  let toml = load_project_toml(&toml_path).map_err(|e| {
    eprintln!("{} {}", "Error:".red().bold(), e);
  })?;

  resolve_project(root.to_path_buf(), toml, overrides).map_err(|e| {
    eprintln!("{} {}", "Error:".red().bold(), e);
  })
}

fn resolve_test_input(cmd: &TestCommand) -> Result<TestInput, ()> {
  if let Some(candidate) = &cmd.filter {
    let path = PathBuf::from(candidate);

    if candidate.ends_with(".ign") {
      return Ok(TestInput::SingleFile(path));
    }
  }

  let overrides = CliOverrides::default();

  if let Some(dir) = &cmd.project {
    let root = PathBuf::from(dir);
    let project = load_and_resolve_project(&root, &overrides)?;
    return Ok(TestInput::Project(project));
  }

  let cwd = std::env::current_dir().map_err(|e| {
    eprintln!("{} Failed to get current directory: {}", "Error:".red().bold(), e);
  })?;

  match find_project_root(&cwd) {
    Some(root) => load_and_resolve_project(&root, &overrides).map(TestInput::Project),
    None => {
      eprintln!("{} No ignis.toml found", "Error:".red().bold());
      eprintln!("Hint: Run 'ignis init' to create a project, or provide --project.");
      Err(())
    },
  }
}

fn resolve_fmt_input(cmd: &FmtCommand) -> Result<FmtInput, ()> {
  let overrides = CliOverrides::default();

  if let Some(dir) = &cmd.project {
    let root = PathBuf::from(dir);
    let toml_path = root.join("ignis.toml");

    if !toml_path.exists() {
      eprintln!("{} No ignis.toml found in '{}'", "Error:".red().bold(), root.display());
      return Err(());
    }

    let project = load_and_resolve_project(&root, &overrides)?;
    return Ok(FmtInput::Project(project));
  }

  if !cmd.file_paths.is_empty() {
    let paths = cmd.file_paths.iter().map(PathBuf::from).collect::<Vec<_>>();

    for path in &paths {
      if !path.exists() {
        eprintln!("{} File not found: '{}'", "Error:".red().bold(), path.display());
        return Err(());
      }
    }

    return Ok(FmtInput::ExplicitFiles(paths));
  }

  let cwd = std::env::current_dir().map_err(|e| {
    eprintln!("{} Failed to get current directory: {}", "Error:".red().bold(), e);
  })?;

  match find_project_root(&cwd) {
    Some(root) => load_and_resolve_project(&root, &overrides).map(FmtInput::Project),
    None => {
      eprintln!("{} No ignis.toml found", "Error:".red().bold());
      eprintln!("Hint: Run 'ignis init' to create a project, or provide a file path.");
      Err(())
    },
  }
}

fn run_fmt(cmd: &FmtCommand) -> Result<(), ()> {
  if cmd.stdin_json {
    return run_fmt_stdin_json(cmd);
  }

  let emit_diff = cmd.emit.iter().any(|e| e == "diff");

  match resolve_fmt_input(cmd)? {
    FmtInput::ExplicitFiles(paths) => {
      let mut dirty_files = Vec::new();

      for path in paths {
        let formatter_config = resolve_formatter_config_for_file(&path, cmd).map_err(|error| {
          eprintln!("{} {}: {}", "Error:".red().bold(), path.display(), error);
        })?;
        let options = FormatOptions {
          check: cmd.check,
          config: formatter_config,
        };

        let outcome = format_file(&path, &options).map_err(|error| {
          eprintln!("{} {}: {}", "Error:".red().bold(), path.display(), error);
        })?;

        if emit_diff && outcome.changed {
          print_diff_for_file(&path, &outcome.formatted);
        }

        if cmd.check && outcome.changed {
          dirty_files.push(path);
        }
      }

      if cmd.check && !dirty_files.is_empty() {
        for file in dirty_files {
          eprintln!("{} {}", "Check failed:".yellow().bold(), file.display());
        }
        return Err(());
      }
    },
    FmtInput::Project(project) => {
      let formatter_config = resolve_formatter_config_for_project(&project, cmd).map_err(|error| {
        eprintln!("{} {}", "Error:".red().bold(), error);
      })?;
      let options = FormatOptions {
        check: cmd.check,
        config: formatter_config,
      };

      let files = collect_project_ign_files(&project.source_dir).map_err(|error| {
        eprintln!("{} Failed to collect project files: {}", "Error:".red().bold(), error);
      })?;

      let mut dirty_files = Vec::new();

      for file in files {
        let outcome = format_file(&file, &options).map_err(|error| {
          eprintln!("{} {}: {}", "Error:".red().bold(), file.display(), error);
        })?;

        if emit_diff && outcome.changed {
          print_diff_for_file(&file, &outcome.formatted);
        }

        if cmd.check && outcome.changed {
          dirty_files.push(file);
        }
      }

      if cmd.check && !dirty_files.is_empty() {
        for file in dirty_files {
          eprintln!("{} {}", "Check failed:".yellow().bold(), file.display());
        }
        return Err(());
      }
    },
  }

  Ok(())
}

fn print_diff_for_file(
  path: &Path,
  formatted: &str,
) {
  let original = std::fs::read_to_string(path).unwrap_or_default();
  let diff_text = compute_unified_diff(path.to_string_lossy().as_ref(), &original, formatted);
  print!("{diff_text}");
}

fn compute_unified_diff(
  path: &str,
  old_text: &str,
  new_text: &str,
) -> String {
  let old_lines: Vec<&str> = old_text.lines().collect();
  let new_lines: Vec<&str> = new_text.lines().collect();

  let mut output = String::new();
  output.push_str(&format!("--- {path}\n+++ {path}\n"));

  let max_lines = old_lines.len().max(new_lines.len());
  let mut change_start = None;
  let mut changes = Vec::new();

  for i in 0..max_lines {
    let old_line = old_lines.get(i).copied();
    let new_line = new_lines.get(i).copied();

    match (old_line, new_line) {
      (Some(o), Some(n)) if o == n => {
        if let Some(start) = change_start.take() {
          changes.push((start, i));
        }
      },
      _ => {
        if change_start.is_none() {
          change_start = Some(i);
        }
      },
    }
  }

  if let Some(start) = change_start.take() {
    changes.push((start, max_lines));
  }

  for (start, end) in &changes {
    let context_before = start.saturating_sub(3);
    let context_after = (*end + 3).min(max_lines);

    output.push_str(&format!(
      "@@ -{},{} +{},{} @@\n",
      context_before + 1,
      context_after - context_before,
      context_before + 1,
      context_after - context_before
    ));

    for i in context_before..context_after {
      let in_change = i >= *start && i < *end;
      let old_line = old_lines.get(i).copied();
      let new_line = new_lines.get(i).copied();

      if in_change {
        if let Some(line) = old_line {
          output.push_str(&format!("-{line}\n"));
        }
        if let Some(line) = new_line {
          output.push_str(&format!("+{line}\n"));
        }
      } else {
        if let Some(line) = old_line.or(new_line) {
          output.push_str(&format!(" {line}\n"));
        }
      }
    }
  }

  output
}

// =============================================================================
// NDJSON stdin protocol
// =============================================================================

#[derive(serde::Deserialize)]
struct StdinRecord {
  path: String,
  text: String,
}

#[derive(serde::Serialize)]
struct StdinResult {
  path: String,
  #[serde(skip_serializing_if = "Option::is_none")]
  formatted: Option<String>,
  changed: bool,
  #[serde(skip_serializing_if = "Option::is_none")]
  diff: Option<String>,
  #[serde(skip_serializing_if = "Option::is_none")]
  error: Option<String>,
}

fn run_fmt_stdin_json(cmd: &FmtCommand) -> Result<(), ()> {
  let emit_diff = cmd.emit.iter().any(|e| e == "diff");

  let formatter_config = resolve_formatter_config_for_stdin(cmd).map_err(|error| {
    eprintln!("{} {}", "Error:".red().bold(), error);
  })?;

  let stdin = std::io::stdin();
  let mut any_error = false;
  let mut any_changed = false;

  for line in stdin.lock().lines() {
    let line = match line {
      Ok(l) => l,
      Err(error) => {
        eprintln!("{} Failed to read stdin: {error}", "Error:".red().bold());
        return Err(());
      },
    };

    let trimmed = line.trim();
    if trimmed.is_empty() {
      continue;
    }

    let record: StdinRecord = match serde_json::from_str(trimmed) {
      Ok(r) => r,
      Err(error) => {
        let result = StdinResult {
          path: String::new(),
          formatted: None,
          changed: false,
          diff: None,
          error: Some(format!("failed to parse NDJSON record: {error}")),
        };
        print_ndjson_result(&result);
        any_error = true;
        continue;
      },
    };

    let options = FormatOptions {
      check: false,
      config: formatter_config.clone(),
    };

    match ignis_formatter::format_text(&record.text, &options) {
      Ok(formatted) => {
        let changed = formatted != record.text;

        let diff = if emit_diff && changed {
          Some(compute_unified_diff(&record.path, &record.text, &formatted))
        } else {
          None
        };

        if changed {
          any_changed = true;
        }

        let result = StdinResult {
          path: record.path,
          formatted: if emit_diff { None } else { Some(formatted) },
          changed,
          diff,
          error: None,
        };
        print_ndjson_result(&result);
      },
      Err(error) => {
        let result = StdinResult {
          path: record.path,
          formatted: None,
          changed: false,
          diff: None,
          error: Some(error.to_string()),
        };
        print_ndjson_result(&result);
        any_error = true;
      },
    }
  }

  if cmd.check && any_changed {
    return Err(());
  }

  if any_error {
    return Err(());
  }

  Ok(())
}

fn print_ndjson_result(result: &StdinResult) {
  let json = serde_json::to_string(result)
    .unwrap_or_else(|error| format!("{{\"path\":\"\",\"error\":\"failed to serialize result: {error}\"}}"));
  println!("{json}");
}

fn resolve_formatter_config_for_stdin(
  cmd: &FmtCommand
) -> Result<ignis_formatter::FormatterConfig, ignis_formatter::FormatError> {
  let cwd = std::env::current_dir().unwrap_or_else(|_| PathBuf::from("."));
  let ignis_toml = cwd.join("ignis.toml");
  let dedicated_config = cwd.join("ignisfmt.toml");

  Ok(load_formatter_config(
    &FormatterConfigPaths {
      project_root: cwd,
      ignis_toml: ignis_toml.exists().then_some(ignis_toml),
      dedicated_config: dedicated_config.exists().then_some(dedicated_config),
      explicit_config: cmd.config.as_ref().map(PathBuf::from),
    },
    &build_formatter_cli_overrides(cmd),
  )?)
}

fn build_formatter_cli_overrides(cmd: &FmtCommand) -> FormatterCliOverrides {
  FormatterCliOverrides {
    indent_width: cmd.indent_width,
    line_width: cmd.line_width,
    use_tabs: if cmd.use_tabs {
      Some(true)
    } else if cmd.spaces {
      Some(false)
    } else {
      None
    },
    sort_imports: if cmd.sort_imports { Some(true) } else { None },
  }
}

fn resolve_formatter_config_for_project(
  project: &Project,
  cmd: &FmtCommand,
) -> Result<ignis_formatter::FormatterConfig, ignis_formatter::FormatError> {
  let explicit_config = cmd.config.as_ref().map(PathBuf::from);
  let dedicated_config = project.root.join("ignisfmt.toml");

  load_formatter_config(
    &FormatterConfigPaths {
      project_root: project.root.clone(),
      ignis_toml: Some(project.toml_path.clone()),
      dedicated_config: dedicated_config.exists().then_some(dedicated_config),
      explicit_config,
    },
    &build_formatter_cli_overrides(cmd),
  )
  .map_err(Into::into)
}

fn resolve_formatter_config_for_file(
  path: &Path,
  cmd: &FmtCommand,
) -> Result<ignis_formatter::FormatterConfig, ignis_formatter::FormatError> {
  let project_root = if let Some(root) = find_project_root(path.parent().unwrap_or_else(|| Path::new("."))) {
    root
  } else {
    path.parent().unwrap_or_else(|| Path::new(".")).to_path_buf()
  };

  let ignis_toml = project_root.join("ignis.toml");
  let dedicated_config = project_root.join("ignisfmt.toml");

  load_formatter_config(
    &FormatterConfigPaths {
      project_root,
      ignis_toml: ignis_toml.exists().then_some(ignis_toml),
      dedicated_config: dedicated_config.exists().then_some(dedicated_config),
      explicit_config: cmd.config.as_ref().map(PathBuf::from),
    },
    &build_formatter_cli_overrides(cmd),
  )
  .map_err(Into::into)
}

fn collect_project_ign_files(source_dir: &Path) -> Result<Vec<PathBuf>, std::io::Error> {
  let mut files = Vec::new();
  collect_project_ign_files_recursive(source_dir, &mut files)?;
  files.sort();
  Ok(files)
}

fn collect_project_ign_files_recursive(
  directory: &Path,
  files: &mut Vec<PathBuf>,
) -> Result<(), std::io::Error> {
  for entry in std::fs::read_dir(directory)? {
    let entry = entry?;
    let path = entry.path();

    if path.is_dir() {
      collect_project_ign_files_recursive(&path, files)?;
      continue;
    }

    if path.extension().and_then(|extension| extension.to_str()) == Some("ign") {
      files.push(path);
    }
  }

  Ok(())
}

fn run_test(
  _cli: &Cli,
  cmd: &TestCommand,
) -> Result<(), ()> {
  match resolve_test_input(cmd)? {
    TestInput::SingleFile(path) => run_single_file_tests(&path, None, cmd.update_snapshots, None),
    TestInput::Project(project) => run_project_tests(&project.root, cmd.filter.as_deref(), cmd.update_snapshots),
  }
}

fn run_test_std(
  _cli: &Cli,
  cmd: &TestStdCommand,
) -> Result<(), ()> {
  let std_path = resolve_std_path(cmd.std_path.as_deref());
  let output_dir = cmd.output_dir.as_deref().map(Path::new);

  run_std_tests(Path::new(&std_path), cmd.filter.as_deref(), cmd.update_snapshots, output_dir)
}

// =============================================================================
// Build Command
// =============================================================================

fn run_build(
  cli: &Cli,
  cmd: &BuildCommand,
) -> Result<(), ()> {
  let overrides = build_cli_overrides(cmd);
  let input = resolve_compile_input(&cmd.file_path, &cmd.project, &overrides)?;

  match input {
    CompileInput::Project(project) => {
      let config = build_config_from_project(&project, cli, cmd, false);
      compile_project(config, project.entry.to_str().unwrap())
    },
    CompileInput::SingleFile(path) => {
      let config = build_config_for_single_file(cli, cmd, &path, false);
      compile_project(config, path.to_str().unwrap())
    },
  }
}

fn build_cli_overrides(cmd: &BuildCommand) -> CliOverrides {
  CliOverrides {
    opt_level: cmd.opt_level,
    debug: if cmd.no_debug {
      Some(false)
    } else if cmd.debug {
      Some(true)
    } else {
      None
    },
    out_dir: cmd.output_dir.as_ref().map(PathBuf::from),
    std_path: cmd.std_path.as_ref().map(PathBuf::from),
    target: Some(cmd.target.clone().into()),
    target_triple: cmd.target_triple.clone(),
    cc: cmd.cc.clone(),
    cflags: None,
    emit: if cmd.emit.is_empty() {
      None
    } else {
      Some(cmd.emit.clone())
    },
  }
}

fn collect_cli_features(
  cmd_feature: &[String],
  cmd_features: &[String],
) -> std::collections::HashSet<String> {
  let mut set = std::collections::HashSet::new();
  for f in cmd_feature {
    set.insert(f.clone());
  }
  for f in cmd_features {
    set.insert(f.clone());
  }
  set
}

fn build_config_from_project(
  project: &Project,
  cli: &Cli,
  cmd: &BuildCommand,
  check_mode: bool,
) -> Arc<IgnisConfig> {
  let mut config = IgnisConfig::new_basic(
    cli.debug,
    cli.debug_trace.iter().copied().map(Into::into).collect(),
    cli.quiet,
    cli.verbose,
  );

  // Set std_path from project
  if let Some(ref std_path) = project.std_path {
    config.std_path = std_path.to_string_lossy().to_string();
  }
  config.std = project.std_path.is_some();
  config.auto_load_std = project.std_path.is_some();

  // Load manifest
  config.manifest = load_manifest(&config.std_path);
  config.c_compiler = project.cc.clone();
  config.cflags = project.cflags.clone();
  config.aliases = project.aliases.clone();

  // Determine emit paths
  let out_dir = &project.out_dir;
  let emit_c = if project.emit.c {
    Some(out_dir.join("c").to_string_lossy().to_string())
  } else {
    None
  };

  let emit_obj = if project.emit.obj && !check_mode {
    Some(out_dir.join("obj").to_string_lossy().to_string())
  } else {
    None
  };

  let emit_bin = if project.bin && !check_mode {
    Some(out_dir.join("bin").join(&project.name).to_string_lossy().to_string())
  } else {
    None
  };

  // Compile-time directives: target triple + features
  if let Some(ref triple) = project.target_triple {
    config.target_triple = triple.clone();
  }

  let mut features = collect_cli_features(&cmd.feature, &cmd.features);
  for f in &project.default_features {
    features.insert(f.clone());
  }
  config.enabled_features = features;

  if !project.known_features.is_empty() {
    config.known_features = Some(project.known_features.iter().cloned().collect());
  }

  config.build = true;
  config.build_config = Some(IgnisBuildConfig::new(
    Some(project.entry.to_string_lossy().to_string()),
    project.target,
    true, // is_project
    project.opt_level > 0,
    project.out_dir.to_string_lossy().to_string(),
    cli.dump.iter().copied().map(Into::into).collect(),
    cli.dump_dir.clone(),
    cli.dump_hir.clone(),
    emit_c,
    emit_obj,
    emit_bin,
    cmd.rebuild_std,
    project.bin,
    cmd.lib,
    check_mode,
    false, // analyze_only
  ));

  Arc::new(config)
}

fn build_config_for_single_file(
  cli: &Cli,
  cmd: &BuildCommand,
  file_path: &Path,
  check_mode: bool,
) -> Arc<IgnisConfig> {
  let mut config = IgnisConfig::new_basic(
    cli.debug,
    cli.debug_trace.iter().copied().map(Into::into).collect(),
    cli.quiet,
    cli.verbose,
  );

  // Set std_path from CLI or env
  config.std_path = resolve_std_path(cmd.std_path.as_deref());
  config.std = !cli.std;
  config.auto_load_std = !cli.auto_load_std;
  config.manifest = load_manifest(&config.std_path);
  config.c_compiler = cmd.cc.clone().unwrap_or_else(|| "gcc".to_string());
  config.cflags = Vec::new();

  let out_dir = cmd.output_dir.as_deref().unwrap_or("build");
  let stem = file_path.file_stem().and_then(|s| s.to_str()).unwrap_or("out");

  let emit_bin = if cmd.lib || check_mode {
    None
  } else {
    Some(format!("{}/{}", out_dir, stem))
  };

  // Compile-time directives: features from CLI
  config.enabled_features = collect_cli_features(&cmd.feature, &cmd.features);

  config.build = true;
  config.build_config = Some(IgnisBuildConfig::new(
    Some(file_path.to_string_lossy().to_string()),
    cmd.target.clone().into(),
    false, // is_project
    cmd.opt_level.unwrap_or(0) > 0,
    out_dir.to_string(),
    cli.dump.iter().copied().map(Into::into).collect(),
    cli.dump_dir.clone(),
    cli.dump_hir.clone(),
    None, // emit_c
    None, // emit_obj
    emit_bin,
    cmd.rebuild_std,
    cmd.bin,
    cmd.lib,
    check_mode,
    false,
  ));

  Arc::new(config)
}

// =============================================================================
// Check Command
// =============================================================================

fn run_check(
  cli: &Cli,
  cmd: &CheckCommand,
) -> Result<(), ()> {
  let overrides = check_cli_overrides(cmd);
  let input = resolve_compile_input(&cmd.file_path, &cmd.project, &overrides)?;

  match input {
    CompileInput::Project(project) => {
      let config = check_config_from_project(&project, cli, cmd);
      compile_project(config, project.entry.to_str().unwrap())
    },
    CompileInput::SingleFile(path) => {
      let config = check_config_for_single_file(cli, cmd, &path);
      compile_project(config, path.to_str().unwrap())
    },
  }
}

fn check_cli_overrides(cmd: &CheckCommand) -> CliOverrides {
  CliOverrides {
    opt_level: None,
    debug: None,
    out_dir: cmd.output_dir.as_ref().map(PathBuf::from),
    std_path: cmd.std_path.as_ref().map(PathBuf::from),
    target: Some(cmd.target.clone().into()),
    target_triple: cmd.target_triple.clone(),
    cc: None,
    cflags: None,
    emit: if cmd.emit.is_empty() {
      None
    } else {
      Some(cmd.emit.clone())
    },
  }
}

fn check_config_from_project(
  project: &Project,
  cli: &Cli,
  cmd: &CheckCommand,
) -> Arc<IgnisConfig> {
  let mut config = IgnisConfig::new_basic(
    cli.debug,
    cli.debug_trace.iter().copied().map(Into::into).collect(),
    cli.quiet,
    cli.verbose,
  );

  if let Some(ref std_path) = project.std_path {
    config.std_path = std_path.to_string_lossy().to_string();
  }
  config.std = project.std_path.is_some();
  config.auto_load_std = project.std_path.is_some();
  config.manifest = load_manifest(&config.std_path);
  config.c_compiler = project.cc.clone();
  config.cflags = project.cflags.clone();
  config.aliases = project.aliases.clone();

  let out_dir = &project.out_dir;
  let emit_c = if project.emit.c {
    Some(out_dir.join("c").to_string_lossy().to_string())
  } else {
    None
  };

  // Compile-time directives: target triple + features
  if let Some(ref triple) = project.target_triple {
    config.target_triple = triple.clone();
  }

  let mut features = collect_cli_features(&cmd.feature, &cmd.features);
  for f in &project.default_features {
    features.insert(f.clone());
  }
  config.enabled_features = features;

  if !project.known_features.is_empty() {
    config.known_features = Some(project.known_features.iter().cloned().collect());
  }

  config.build = true;
  config.build_config = Some(IgnisBuildConfig::new(
    Some(project.entry.to_string_lossy().to_string()),
    project.target,
    true,
    false, // optimize
    project.out_dir.to_string_lossy().to_string(),
    cli.dump.iter().copied().map(Into::into).collect(),
    cli.dump_dir.clone(),
    cli.dump_hir.clone(),
    emit_c,
    None, // emit_obj (not in check mode)
    None, // emit_bin (not in check mode)
    false,
    cmd.bin,
    cmd.lib,
    true, // check_mode
    cmd.analyze_only,
  ));

  Arc::new(config)
}

fn check_config_for_single_file(
  cli: &Cli,
  cmd: &CheckCommand,
  file_path: &Path,
) -> Arc<IgnisConfig> {
  let mut config = IgnisConfig::new_basic(
    cli.debug,
    cli.debug_trace.iter().copied().map(Into::into).collect(),
    cli.quiet,
    cli.verbose,
  );

  config.std_path = resolve_std_path(cmd.std_path.as_deref());
  config.std = !cli.std;
  config.auto_load_std = !cli.auto_load_std;
  config.manifest = load_manifest(&config.std_path);

  let out_dir = cmd.output_dir.as_deref().unwrap_or("build");

  // Compile-time directives: features from CLI
  config.enabled_features = collect_cli_features(&cmd.feature, &cmd.features);

  config.build = true;
  config.build_config = Some(IgnisBuildConfig::new(
    Some(file_path.to_string_lossy().to_string()),
    cmd.target.clone().into(),
    false,
    false,
    out_dir.to_string(),
    cli.dump.iter().copied().map(Into::into).collect(),
    cli.dump_dir.clone(),
    cli.dump_hir.clone(),
    None,
    None,
    None,
    false,
    cmd.bin,
    cmd.lib,
    true,
    cmd.analyze_only,
  ));

  Arc::new(config)
}

// =============================================================================
// Other Commands
// =============================================================================

fn run_lsp(cli: &Cli) {
  let mut config =
    IgnisConfig::new_basic(cli.debug, cli.debug_trace.iter().copied().map(Into::into).collect(), true, 0);

  config.std_path = resolve_std_path(None);
  config.std = !cli.std;
  config.auto_load_std = !cli.auto_load_std;
  config.manifest = load_manifest(&config.std_path);

  let config = Arc::new(config);

  #[allow(clippy::disallowed_methods)]
  let rt = tokio::runtime::Runtime::new().expect("Failed to create Tokio runtime");
  rt.block_on(ignis_lsp::run(config));
}

fn run_build_std(
  cli: &Cli,
  output_dir: &str,
) -> Result<(), ()> {
  let config = build_std_config(cli);
  build_std(config, output_dir)
}

fn run_check_std(
  cli: &Cli,
  output_dir: &str,
) -> Result<(), ()> {
  let config = build_std_config(cli);
  check_std(config, output_dir)
}

fn run_check_runtime(
  cli: &Cli,
  runtime_path: Option<&str>,
) -> Result<(), ()> {
  let config = build_std_config(cli);
  check_runtime(config, runtime_path)
}

fn build_std_config(cli: &Cli) -> Arc<IgnisConfig> {
  let mut config = IgnisConfig::new_basic(
    cli.debug,
    cli.debug_trace.iter().copied().map(Into::into).collect(),
    cli.quiet,
    cli.verbose,
  );

  config.std_path = resolve_std_path(None);
  config.std = true;
  config.auto_load_std = true;
  config.manifest = load_manifest(&config.std_path);

  Arc::new(config)
}

// =============================================================================
// Utilities
// =============================================================================

/// Resolve std_path from CLI override or environment variable.
fn resolve_std_path(cli_override: Option<&str>) -> String {
  if let Some(path) = cli_override {
    return path.to_string();
  }

  std::env::var("IGNIS_STD_PATH").unwrap_or_default()
}

/// Load the std manifest from std_path/manifest.toml.
fn load_manifest(std_path: &str) -> IgnisSTDManifest {
  if std_path.is_empty() {
    return IgnisSTDManifest::default();
  }

  let manifest_path = Path::new(std_path).join("manifest.toml");

  if !manifest_path.exists() {
    return IgnisSTDManifest::default();
  }

  match std::fs::read_to_string(&manifest_path) {
    Ok(content) => toml::from_str(&content).unwrap_or_default(),
    Err(_) => IgnisSTDManifest::default(),
  }
}

// =============================================================================
// Main
// =============================================================================

fn main() {
  let cli = Cli::parse();

  let result = match &cli.subcommand {
    SubCommand::Lsp(_) => {
      run_lsp(&cli);
      Ok(())
    },

    SubCommand::Build(cmd) => run_build(&cli, cmd),

    SubCommand::Test(cmd) => run_test(&cli, cmd),

    SubCommand::TestStd(cmd) => run_test_std(&cli, cmd),

    SubCommand::Check(cmd) => run_check(&cli, cmd),

    SubCommand::BuildStd(cmd) => run_build_std(&cli, &cmd.output_dir),

    SubCommand::CheckStd(cmd) => run_check_std(&cli, &cmd.output_dir),

    SubCommand::CheckRuntime(cmd) => run_check_runtime(&cli, cmd.runtime_path.as_deref()),

    SubCommand::Fmt(cmd) => run_fmt(cmd),

    SubCommand::Init(cmd) => run_init(cmd, cli.quiet),
  };

  if result.is_err() {
    std::process::exit(1);
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  fn make_temp_dir(label: &str) -> PathBuf {
    let nonce = std::time::SystemTime::now()
      .duration_since(std::time::UNIX_EPOCH)
      .expect("system time")
      .as_nanos();
    let dir = std::env::temp_dir().join(format!("ignis_test_command_{label}_{nonce}"));
    std::fs::create_dir_all(&dir).expect("create temp dir");
    dir
  }

  #[test]
  fn resolve_test_input_accepts_single_file_mode() {
    let temp_dir = make_temp_dir("single_file");
    let file_path = temp_dir.join("single.ign");
    std::fs::write(&file_path, "function main(): void { return; }").expect("write test file");

    let cmd = TestCommand {
      filter: Some(file_path.to_string_lossy().into_owned()),
      project: None,
      update_snapshots: true,
    };

    match resolve_test_input(&cmd).expect("resolve test input") {
      TestInput::SingleFile(resolved_path) => assert_eq!(resolved_path, file_path),
      other => panic!("expected single-file input, got {:?}", other),
    }

    let _ = std::fs::remove_dir_all(&temp_dir);
  }

  #[test]
  fn resolve_test_input_uses_project_mode_for_filter_text() {
    let temp_dir = make_temp_dir("project_mode");
    std::fs::write(temp_dir.join("ignis.toml"), "[package]\nname = \"demo\"\nversion = \"0.1.0\"\n")
      .expect("write ignis.toml");
    std::fs::create_dir_all(temp_dir.join("src")).expect("create src dir");
    std::fs::write(temp_dir.join("src").join("main.ign"), "function main(): void { return; }")
      .expect("write entry file");

    let cmd = TestCommand {
      filter: Some("math".to_string()),
      project: Some(temp_dir.to_string_lossy().into_owned()),
      update_snapshots: true,
    };

    let resolved = resolve_test_input(&cmd).expect("resolve test input");

    match resolved {
      TestInput::Project(project) => {
        assert_eq!(project.root, temp_dir.canonicalize().expect("canonical project root"));
        assert_eq!(cmd.filter.as_deref(), Some("math"));
        assert!(cmd.update_snapshots);
      },
      other => panic!("expected project input, got {:?}", other),
    }

    let _ = std::fs::remove_dir_all(&temp_dir);
  }

  #[test]
  fn resolve_test_input_keeps_existing_non_ign_file_as_filter_text() {
    let temp_dir = make_temp_dir("filter_file");
    let filter_path = temp_dir.join("math_filter.txt");
    std::fs::write(&filter_path, "math::adds").expect("write filter file");

    std::fs::write(temp_dir.join("ignis.toml"), "[package]\nname = \"demo\"\nversion = \"0.1.0\"\n")
      .expect("write ignis.toml");
    std::fs::create_dir_all(temp_dir.join("src")).expect("create src dir");
    std::fs::write(temp_dir.join("src").join("main.ign"), "function main(): void { return; }")
      .expect("write entry file");

    let cmd = TestCommand {
      filter: Some(filter_path.to_string_lossy().into_owned()),
      project: Some(temp_dir.to_string_lossy().into_owned()),
      update_snapshots: false,
    };

    let resolved = resolve_test_input(&cmd).expect("resolve test input");

    match resolved {
      TestInput::Project(project) => {
        assert_eq!(project.root, temp_dir.canonicalize().expect("canonical project root"));
        assert_eq!(cmd.filter.as_deref(), Some(filter_path.to_string_lossy().as_ref()));
        assert!(!cmd.update_snapshots);
      },
      other => panic!("expected project input, got {:?}", other),
    }

    let _ = std::fs::remove_dir_all(&temp_dir);
  }

  #[test]
  fn resolve_test_input_accepts_nonexistent_ign_path_as_single_file() {
    let temp_dir = make_temp_dir("missing_single_file");
    let file_path = temp_dir.join("single.ign");

    let cmd = TestCommand {
      filter: Some(file_path.to_string_lossy().into_owned()),
      project: None,
      update_snapshots: false,
    };

    match resolve_test_input(&cmd).expect("resolve test input") {
      TestInput::SingleFile(resolved_path) => assert_eq!(resolved_path, file_path),
      other => panic!("expected single-file input, got {:?}", other),
    }

    let _ = std::fs::remove_dir_all(&temp_dir);
  }
}
