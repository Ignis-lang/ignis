mod cli;
mod init;

use std::path::{Path, PathBuf};
use std::sync::Arc;

use clap::Parser as ClapParser;
use colored::*;

use cli::{BuildCommand, CheckCommand, Cli, SubCommand};
use ignis_config::{IgnisBuildConfig, IgnisConfig, IgnisSTDManifest};
use ignis_driver::{
  build_std, check_runtime, check_std, compile_project, find_project_root, load_project_toml, resolve_project,
  CliOverrides, Project,
};
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
    cc: cmd.cc.clone(),
    cflags: None,
    emit: if cmd.emit.is_empty() {
      None
    } else {
      Some(cmd.emit.clone())
    },
  }
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

  config.build = true;
  config.build_config = Some(IgnisBuildConfig::new(
    Some(project.entry.to_string_lossy().to_string()),
    ignis_config::TargetBackend::C,
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

  config.build = true;
  config.build_config = Some(IgnisBuildConfig::new(
    Some(file_path.to_string_lossy().to_string()),
    ignis_config::TargetBackend::C,
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

  let out_dir = &project.out_dir;
  let emit_c = if project.emit.c {
    Some(out_dir.join("c").to_string_lossy().to_string())
  } else {
    None
  };

  config.build = true;
  config.build_config = Some(IgnisBuildConfig::new(
    Some(project.entry.to_string_lossy().to_string()),
    ignis_config::TargetBackend::C,
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

  config.build = true;
  config.build_config = Some(IgnisBuildConfig::new(
    Some(file_path.to_string_lossy().to_string()),
    ignis_config::TargetBackend::C,
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

    SubCommand::Check(cmd) => run_check(&cli, cmd),

    SubCommand::BuildStd(cmd) => run_build_std(&cli, &cmd.output_dir),

    SubCommand::CheckStd(cmd) => run_check_std(&cli, &cmd.output_dir),

    SubCommand::CheckRuntime(cmd) => run_check_runtime(&cli, cmd.runtime_path.as_deref()),

    SubCommand::Init(cmd) => run_init(cmd, cli.quiet),
  };

  if result.is_err() {
    std::process::exit(1);
  }
}
