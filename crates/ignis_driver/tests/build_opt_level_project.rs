//! Proves that the resolved build profile (`ignis.toml [build] opt_level` and
//! `[build] debug`) reaches the C toolchain, instead of leaving the optimization
//! level to whatever the host C driver defaults to.

#![cfg(unix)]

use std::os::unix::fs::PermissionsExt;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use ignis_config::{IgnisBuildConfig, IgnisConfig, IgnisSTDManifest, StdToolchainConfig, TargetBackend};
use ignis_driver::{compile_project, resolve_project, CliOverrides, Project, ProjectToml};
use tempfile::TempDir;

const SOURCE: &str = r#"
function main(): i32 {
    return 0;
}
"#;

/// A stand-in for the C compiler that appends its own argument list to a log and
/// then forwards to gcc, so the flags the driver chose can be read back exactly as
/// the toolchain received them.
fn write_recording_compiler(
  dir: &Path,
  log_path: &Path,
) -> PathBuf {
  let script_path = dir.join("recording-cc");

  std::fs::write(
    &script_path,
    format!(
      "#!/bin/sh\nprintf '%s\\n' \"$*\" >> '{}'\nexec gcc \"$@\"\n",
      log_path.display()
    ),
  )
  .expect("write recording compiler");

  std::fs::set_permissions(&script_path, std::fs::Permissions::from_mode(0o755)).expect("make compiler executable");

  script_path
}

fn write_project(
  opt_level: u8,
  debug: bool,
) -> TempDir {
  let temp_dir = TempDir::new().expect("temporary project dir");
  let src_dir = temp_dir.path().join("src");

  std::fs::create_dir_all(&src_dir).expect("create src dir");
  std::fs::write(src_dir.join("main.ign"), SOURCE).expect("write main module");

  let compiler = write_recording_compiler(temp_dir.path(), &temp_dir.path().join("cc.log"));

  std::fs::write(
    temp_dir.path().join("ignis.toml"),
    format!(
      "[package]\nname = \"build_opt_level_fixture\"\nversion = \"0.1.0\"\nauthors = []\ndescription = \"fixture\"\nkeywords = []\nlicense = \"MIT\"\nrepository = \"\"\n\n[ignis]\nstd = false\n\n[build]\nbin = true\nsource_dir = \"src\"\nentry = \"main.ign\"\nout_dir = \"build\"\nopt_level = {opt_level}\ndebug = {debug}\ntarget = \"c\"\ncc = \"{compiler}\"\ncflags = []\nemit = []\n",
      compiler = compiler.display()
    ),
  )
  .expect("write ignis.toml");

  temp_dir
}

fn resolve_test_project(root: &Path) -> Project {
  let toml_text = std::fs::read_to_string(root.join("ignis.toml")).expect("read ignis.toml");
  let toml: ProjectToml = toml::from_str(&toml_text).expect("parse ignis.toml");

  resolve_project(root.to_path_buf(), toml, &CliOverrides::default()).expect("resolve project")
}

/// Mirrors `build_config_from_project` in `crates/ignis/src/main.rs` for the fields
/// that decide the C toolchain invocation.
fn build_config(project: &Project) -> Arc<IgnisConfig> {
  let fixtures_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures");

  let mut config = IgnisConfig::new_basic(false, Vec::new(), true, 0);
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
  config.c_compiler = project.cc.clone();
  config.cflags = project.cflags.clone();
  config.build = true;
  config.build_debug = project.debug;
  config.opt_level = project.opt_level;

  let bin_path = project.out_dir.join("bin").join(&project.name);
  config.build_config = Some(IgnisBuildConfig::new(
    Some(project.entry.to_string_lossy().to_string()),
    TargetBackend::C,
    true, // is_project
    project.opt_level > 0,
    project.out_dir.to_string_lossy().to_string(),
    Vec::new(),
    None,
    None,
    None,
    None,
    Some(bin_path.to_string_lossy().to_string()),
    false,
    project.bin,
    false,
    false,
    false,
  ));

  Arc::new(config)
}

/// Compiles the fixture project and returns every recorded C toolchain invocation.
fn recorded_invocations(project_root: &Path) -> Vec<String> {
  let project = resolve_test_project(project_root);
  let config = build_config(&project);
  let bin_path = project.out_dir.join("bin").join(&project.name);

  std::fs::create_dir_all(bin_path.parent().expect("bin dir")).expect("create bin dir");
  compile_project(config, project.entry.to_str().unwrap()).expect("compile_project failed");

  let log = std::fs::read_to_string(project_root.join("cc.log")).expect("read recorded invocations");

  let invocations: Vec<String> = log.lines().map(str::to_string).collect();

  assert!(!invocations.is_empty(), "the C toolchain was never invoked");

  invocations
}

#[test]
fn project_opt_level_reaches_every_c_toolchain_invocation() {
  let project = write_project(2, false);

  for invocation in recorded_invocations(project.path()) {
    assert!(
      invocation.split_whitespace().any(|argument| argument == "-O2"),
      "expected -O2 in: {}",
      invocation
    );
  }
}

#[test]
fn a_project_without_optimization_states_the_level_instead_of_omitting_it() {
  let project = write_project(0, false);

  for invocation in recorded_invocations(project.path()) {
    assert!(
      invocation.split_whitespace().any(|argument| argument == "-O0"),
      "expected an explicit -O0 in: {}",
      invocation
    );

    assert!(
      !invocation.split_whitespace().any(|argument| argument == "-g"),
      "did not expect -g in: {}",
      invocation
    );
  }
}

#[test]
fn project_debug_adds_debug_information_to_every_c_toolchain_invocation() {
  let project = write_project(0, true);

  for invocation in recorded_invocations(project.path()) {
    assert!(
      invocation.split_whitespace().any(|argument| argument == "-g"),
      "expected -g in: {}",
      invocation
    );
  }
}
