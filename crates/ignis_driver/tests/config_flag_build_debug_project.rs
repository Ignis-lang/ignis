//! Proves that `@debug()` follows the resolved project build profile
//! (`ignis.toml [build] debug`), not the host compiler's own build profile.

mod common;

use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::Arc;

use ignis_config::{IgnisBuildConfig, IgnisConfig, IgnisSTDManifest, StdToolchainConfig, TargetBackend};
use ignis_driver::{compile_project, resolve_project, CliOverrides, Project, ProjectToml};
use tempfile::TempDir;

const SOURCE: &str = r#"
function main(): i32 {
    let debug: boolean = @configFlag(@debug());
    if (debug) {
        return 42;
    }
    return 0;
}
"#;

fn write_project(toml_debug_line: &str) -> TempDir {
  let temp_dir = TempDir::new().expect("temporary project dir");
  let src_dir = temp_dir.path().join("src");

  std::fs::create_dir_all(&src_dir).expect("create src dir");
  std::fs::write(src_dir.join("main.ign"), SOURCE).expect("write main module");
  std::fs::write(
    temp_dir.path().join("ignis.toml"),
    format!(
      "[package]\nname = \"config_flag_build_debug_fixture\"\nversion = \"0.1.0\"\nauthors = []\ndescription = \"fixture\"\nkeywords = []\nlicense = \"MIT\"\nrepository = \"\"\n\n[ignis]\nstd = false\n\n[build]\nbin = true\nsource_dir = \"src\"\nentry = \"main.ign\"\nout_dir = \"build\"\nopt_level = 0\n{toml_debug_line}\ntarget = \"c\"\ncc = \"cc\"\ncflags = []\nemit = []\n",
    ),
  )
  .expect("write ignis.toml");

  temp_dir
}

fn resolve_test_project(
  root: &Path,
  overrides: &CliOverrides,
) -> Project {
  let toml_text = std::fs::read_to_string(root.join("ignis.toml")).expect("read ignis.toml");
  let toml: ProjectToml = toml::from_str(&toml_text).expect("parse ignis.toml");

  resolve_project(root.to_path_buf(), toml, overrides).expect("resolve project")
}

/// Mirrors `build_config_from_project` in `crates/ignis/src/main.rs`: builds
/// an `IgnisConfig` from a resolved `Project`, wiring `build_debug` from the
/// project's resolved build profile.
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

/// Compiles and runs the fixture project, returning the exit code.
fn compile_and_run_project(project_root: &Path) -> i32 {
  let project = resolve_test_project(project_root, &CliOverrides::default());
  let config = build_config(&project);
  let bin_path = project.out_dir.join("bin").join(&project.name);

  std::fs::create_dir_all(bin_path.parent().expect("bin dir")).expect("create bin dir");
  compile_project(config, project.entry.to_str().unwrap()).expect("compile_project failed");

  let output = Command::new(&bin_path).output().expect("run compiled binary");

  output.status.code().expect("exit code")
}

#[test]
fn ignis_toml_build_debug_true_makes_config_flag_debug_true() {
  let project = write_project("debug = true");

  assert_eq!(compile_and_run_project(project.path()), 42);
}

#[test]
fn ignis_toml_build_debug_false_makes_config_flag_debug_false() {
  let project = write_project("debug = false");

  assert_eq!(compile_and_run_project(project.path()), 0);
}

#[test]
fn no_debug_override_turns_off_config_flag_debug_even_when_toml_enables_it() {
  let project = write_project("debug = true");
  let overrides = CliOverrides {
    debug: Some(false),
    ..CliOverrides::default()
  };
  let resolved_project = resolve_test_project(project.path(), &overrides);
  let config = build_config(&resolved_project);
  let bin_path = resolved_project.out_dir.join("bin").join(&resolved_project.name);

  std::fs::create_dir_all(bin_path.parent().expect("bin dir")).expect("create bin dir");
  compile_project(config, resolved_project.entry.to_str().unwrap()).expect("compile_project failed");

  let output = Command::new(&bin_path).output().expect("run compiled binary");

  assert_eq!(output.status.code().expect("exit code"), 0);
}
