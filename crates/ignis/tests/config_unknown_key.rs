//! IGN-239: an unknown key in `ignis.toml` warns instead of being silently
//! ignored, and never turns `ignis check`/`build`/`test` into a failure.

use std::path::Path;
use std::process::Command;

use tempfile::TempDir;

const SOURCE: &str = r#"
function main(): i32 {
    return 0;
}
"#;

fn write_project_with_unknown_key() -> TempDir {
  let temp_dir = TempDir::new().expect("temporary project dir");
  let src_dir = temp_dir.path().join("src");

  std::fs::create_dir_all(&src_dir).expect("create src dir");
  std::fs::write(src_dir.join("main.ign"), SOURCE).expect("write main module");

  std::fs::write(
    temp_dir.path().join("ignis.toml"),
    r#"
[package]
name = "config_unknown_key_fixture"
version = "0.1.0"

[ignis]
std = false
runtime_path = "../std"

[build]
source_dir = "src"
entry = "main.ign"
"#,
  )
  .expect("write ignis.toml");

  temp_dir
}

fn run_check(project_dir: &Path) -> std::process::Output {
  Command::new(env!("CARGO_BIN_EXE_ignis"))
    .current_dir(project_dir)
    .arg("check")
    .arg("--project")
    .arg(".")
    .output()
    .expect("run ignis check")
}

#[test]
fn unknown_project_key_warns_on_stderr_without_failing_the_build() {
  let project = write_project_with_unknown_key();
  let output = run_check(project.path());

  assert!(
    output.status.success(),
    "expected ignis check to succeed despite the unknown key\nstdout:\n{}\nstderr:\n{}",
    String::from_utf8_lossy(&output.stdout),
    String::from_utf8_lossy(&output.stderr)
  );

  let stderr = String::from_utf8_lossy(&output.stderr);

  assert!(
    stderr.contains("unknown key 'runtime_path' in [ignis]"),
    "expected an unknown-key warning on stderr, got:\n{stderr}"
  );

  assert!(
    stderr.contains("Warning[C1008]"),
    "expected the warning to carry the config unknown-key code, got:\n{stderr}"
  );
}

#[test]
fn a_project_without_unknown_keys_stays_silent_about_them() {
  let temp_dir = TempDir::new().expect("temporary project dir");
  let src_dir = temp_dir.path().join("src");

  std::fs::create_dir_all(&src_dir).expect("create src dir");
  std::fs::write(src_dir.join("main.ign"), SOURCE).expect("write main module");

  std::fs::write(
    temp_dir.path().join("ignis.toml"),
    r#"
[package]
name = "config_no_unknown_key_fixture"
version = "0.1.0"

[ignis]
std = false

[build]
source_dir = "src"
entry = "main.ign"
"#,
  )
  .expect("write ignis.toml");

  let output = run_check(temp_dir.path());

  assert!(
    output.status.success(),
    "expected ignis check to succeed\nstdout:\n{}\nstderr:\n{}",
    String::from_utf8_lossy(&output.stdout),
    String::from_utf8_lossy(&output.stderr)
  );

  let stderr = String::from_utf8_lossy(&output.stderr);

  assert!(
    !stderr.contains("unknown key"),
    "did not expect an unknown-key warning, got:\n{stderr}"
  );
}
