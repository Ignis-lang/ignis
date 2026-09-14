//! Regression coverage for issue #216 in a multi-module project: a closure struct
//! referencing a by-value record/enum defined in *another* module must still be
//! forward-declared, and a record embedding a closure struct by value that it pulls
//! in from another module must still get a complete definition for that closure
//! struct. Both fixtures below fail to compile on `main` (before the fix landed in
//! `emit_module_header`) and pass after it.

use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::Arc;

use ignis_config::{IgnisBuildConfig, IgnisConfig, IgnisSTDManifest, StdToolchainConfig, TargetBackend};
use ignis_driver::{compile_project, resolve_project, run_project_tests, CliOverrides, Project, ProjectToml};
use tempfile::TempDir;

/// Writes a two-module project (`geom.ign` + `main.ign`) under a fresh temp dir and
/// returns it. `geom.ign` exports a record holding a closure by value, whose closure
/// signature takes another exported record by value; `main.ign` never names either
/// the closure type or the closure's record parameter directly, only the record that
/// embeds it — reproducing the exact pattern from issue #216's multi-module report.
fn write_project() -> TempDir {
  let temp_dir = TempDir::new().expect("temporary project dir");
  let src_dir = temp_dir.path().join("src");
  std::fs::create_dir_all(&src_dir).expect("create src dir");

  std::fs::write(
    src_dir.join("geom.ign"),
    r#"
export record Point {
    public x: i32;
    public y: i32;
}

export record Handlers {
    public onPoint: (Point) -> i32;
}

export function makeHandlers(): Handlers {
    return Handlers { onPoint: (p: Point): i32 -> p.x + p.y };
}

export function run(h: Handlers): i32 {
    let f: (Point) -> i32 = h.onPoint;
    return f(Point { x: 20, y: 22 });
}
"#,
  )
  .expect("write geom module");

  std::fs::write(
    src_dir.join("main.ign"),
    r#"
import Handlers, makeHandlers, run from "./geom";

function main(): i32 {
    let h: Handlers = makeHandlers();
    return run(h);
}
"#,
  )
  .expect("write main module");

  std::fs::write(
    temp_dir.path().join("ignis.toml"),
    "[package]\nname = \"closure_record_by_value_multi_module\"\nversion = \"0.1.0\"\nauthors = []\ndescription = \"fixture\"\nkeywords = []\nlicense = \"MIT\"\nrepository = \"\"\n\n[ignis]\nstd = false\n\n[build]\nbin = true\nsource_dir = \"src\"\nentry = \"main.ign\"\nout_dir = \"build\"\nopt_level = 0\ndebug = false\ntarget = \"c\"\ncflags = []\nemit = []\n",
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
/// the C backend needs. `std = false` keeps this test independent of the standard
/// library build.
fn build_config(project: &Project) -> Arc<IgnisConfig> {
  let fixtures_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures");

  let mut config = IgnisConfig::new_basic(false, Vec::new(), true, 0);
  config.std = false;
  config.auto_load_std = false;
  config.std_path = fixtures_dir.to_string_lossy().to_string();
  config.manifest = IgnisSTDManifest {
    toolchain: StdToolchainConfig {
      include_dirs: vec![".".to_string()],
    },
    modules: std::collections::HashMap::from([("__test_base".to_string(), "runtime/ignis_rt.h".to_string())]),
    ..Default::default()
  };
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
    project.bin,
    false,
    false,
    false,
    false,
  ));

  Arc::new(config)
}

#[test]
fn multi_module_closure_record_by_value_compiles_and_runs() {
  let temp_dir = write_project();
  let project = resolve_test_project(temp_dir.path());
  let config = build_config(&project);
  let bin_path = project.out_dir.join("bin").join(&project.name);

  std::fs::create_dir_all(bin_path.parent().expect("bin dir")).expect("create bin dir");

  compile_project(config, project.entry.to_str().unwrap()).expect(
    "compile_project failed: a closure struct referencing a by-value record from another \
     module must be forward-declared, and a record pulling in a closure struct by value \
     from another module must get a complete definition for it (issue #216)",
  );

  let output = Command::new(&bin_path).output().expect("run compiled binary");

  assert_eq!(
    output.status.code(),
    Some(42),
    "stdout: {}\nstderr: {}",
    String::from_utf8_lossy(&output.stdout),
    String::from_utf8_lossy(&output.stderr)
  );
}

/// Same shape as above, but through an `ignis test` project (`run_project_tests`)
/// instead of a plain build: `@test`-annotated functions call across the module
/// boundary into a record that embeds a closure by value.
fn write_ignis_test_project() -> TempDir {
  let temp_dir = TempDir::new().expect("temporary project dir");
  let src_dir = temp_dir.path().join("src");
  std::fs::create_dir_all(&src_dir).expect("create src dir");

  std::fs::write(
    src_dir.join("geom.ign"),
    r#"
export record Point {
    public x: i32;
    public y: i32;
}

export record Handlers {
    public onPoint: (Point) -> i32;
}

export function makeHandlers(): Handlers {
    return Handlers { onPoint: (p: Point): i32 -> p.x + p.y };
}

export function run(h: Handlers): i32 {
    let f: (Point) -> i32 = h.onPoint;
    return f(Point { x: 20, y: 22 });
}
"#,
  )
  .expect("write geom module");

  std::fs::write(
    src_dir.join("main.ign"),
    r#"
import Handlers, makeHandlers, run from "./geom";
import Test from "std::test";

@test
function crossModuleClosureRecordByValue(): void {
    let h: Handlers = makeHandlers();
    Test::assertEq(run(h), 42);
}

function main(): void {}
"#,
  )
  .expect("write main module");

  let workspace_std_path = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../std");

  std::fs::write(
    temp_dir.path().join("ignis.toml"),
    format!(
      "[package]\nname = \"closure_record_by_value_multi_module_test\"\nversion = \"0.1.0\"\nauthors = []\ndescription = \"fixture\"\nkeywords = []\nlicense = \"MIT\"\nrepository = \"\"\n\n[ignis]\nstd = true\nstd_path = \"{}\"\n\n[build]\nbin = true\nsource_dir = \"src\"\nentry = \"main.ign\"\nout_dir = \"build\"\nopt_level = 0\ndebug = false\ntarget = \"c\"\ncc = \"cc\"\ncflags = []\nemit = []\n",
      workspace_std_path.display()
    ),
  )
  .expect("write ignis.toml");

  temp_dir
}

#[test]
fn ignis_test_project_multi_module_closure_record_by_value() {
  let project = write_ignis_test_project();

  let result = run_project_tests(project.path(), None, false);

  assert!(
    result.is_ok(),
    "expected the cross-module closure/record-by-value @test to compile and pass (issue #216)"
  );
}
