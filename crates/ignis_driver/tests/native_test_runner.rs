mod common;

use std::fs;
use std::path::{Path, PathBuf};

use ignis_driver::run_project_tests;
use tempfile::TempDir;

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

fn harness_binary_path(project_root: &Path) -> PathBuf {
  project_root.join("build/bin/native_test_runner_fixture-tests")
}

#[test]
fn run_project_tests_returns_ok_when_filtered_tests_pass() {
  let project = write_test_project(
    r#"
@test
function passes(): void {}

@test
function fails(): void {
    @panic("boom");
}
"#,
  );

  let result = run_project_tests(project.path(), Some("passes"));

  assert!(result.is_ok(), "expected filtered passing test run to succeed");
  assert!(
    harness_binary_path(project.path()).exists(),
    "expected test harness binary to be built"
  );
}

#[test]
fn run_project_tests_returns_err_when_any_selected_test_fails() {
  let project = write_test_project(
    r#"
@test
function passes(): void {}

@test
function fails(): void {
    @panic("boom");
}

@test
function laterPass(): void {}
"#,
  );

  let result = run_project_tests(project.path(), None);

  assert!(result.is_err(), "expected mixed pass/fail test run to return an error");
  assert!(
    harness_binary_path(project.path()).exists(),
    "expected test harness binary to be built"
  );
}
