use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::{SystemTime, UNIX_EPOCH};

fn workspace_std_path() -> PathBuf {
  PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../std")
}

fn make_temp_project_dir(label: &str) -> PathBuf {
  let unique = SystemTime::now()
    .duration_since(UNIX_EPOCH)
    .expect("system time before unix epoch")
    .as_nanos();

  let project_dir = std::env::temp_dir().join(format!(
    "ignis-cli-test-{label}-{}-{unique}",
    std::process::id()
  ));

  fs::create_dir_all(project_dir.join("src")).expect("create src dir");
  project_dir
}

fn write_test_project(
  project_dir: &Path,
  source: &str,
) {
  fs::write(project_dir.join("src/main.ign"), source).expect("write main module");
  fs::write(
    project_dir.join("ignis.toml"),
    format!(
      "[package]\nname = \"native_test_runner_fixture\"\nversion = \"0.1.0\"\nauthors = []\ndescription = \"fixture\"\nkeywords = []\nlicense = \"MIT\"\nrepository = \"\"\n\n[ignis]\nstd = true\nstd_path = \"{}\"\n\n[build]\nbin = true\nsource_dir = \"src\"\nentry = \"main.ign\"\nout_dir = \"build\"\nopt_level = 0\ndebug = false\ntarget = \"c\"\ncc = \"cc\"\ncflags = []\nemit = []\n",
      workspace_std_path().display()
    ),
  )
  .expect("write ignis.toml");
}

fn cleanup_project_dir(project_dir: &Path) {
  let _ = fs::remove_dir_all(project_dir);
}

fn harness_binary_path(project_dir: &Path) -> PathBuf {
  project_dir.join("build/bin/native_test_runner_fixture-tests")
}

#[test]
fn ignis_test_executes_generic_assertions_via_cli() {
  let project_dir = make_temp_project_dir("generic-eq");
  write_test_project(
    &project_dir,
    r#"
import String from "std::string";
import Test from "std::test";

@test
function genericAssertionsPass(): void {
    let equalLeft: String = String::create("same");
    let equalRight: String = String::create("same");
    let notEqualLeft: String = String::create("same");
    let notEqualRight: String = String::create("other");

    Test::assertEq<String>(equalLeft, equalRight);
    Test::assertNe<String>(notEqualLeft, notEqualRight);
    Test::assertEq<str>("cli", "cli");
}
"#,
  );

  let output = Command::new(env!("CARGO_BIN_EXE_ignis"))
    .arg("test")
    .arg("--project")
    .arg(&project_dir)
    .output()
    .expect("run ignis test");

  if !output.status.success() {
    panic!(
      "expected ignis test to succeed\nstdout:\n{}\nstderr:\n{}",
      String::from_utf8_lossy(&output.stdout),
      String::from_utf8_lossy(&output.stderr)
    );
  }

  let stdout = String::from_utf8_lossy(&output.stdout);
  let stderr = String::from_utf8_lossy(&output.stderr);

  assert!(
    stdout.contains("Tests passed") || stderr.contains("Tests passed"),
    "expected successful cli test summary\nstdout:\n{stdout}\nstderr:\n{stderr}"
  );
  assert!(harness_binary_path(&project_dir).exists(), "expected cli test run to build the harness binary");

  cleanup_project_dir(&project_dir);
}

#[test]
fn ignis_test_reports_unsupported_generic_equality_before_harness_build() {
  let project_dir = make_temp_project_dir("unsupported-eq");
  write_test_project(
    &project_dir,
    r#"
import Test from "std::test";

record Pair {
    public value: i32;
}

@test
function invalidEq(): void {
    let left: Pair = Pair { value: 1 };
    let right: Pair = Pair { value: 1 };
    Test::assertNe<Pair>(left, right);
}
"#,
  );

  let output = Command::new(env!("CARGO_BIN_EXE_ignis"))
    .arg("test")
    .arg("--project")
    .arg(&project_dir)
    .output()
    .expect("run ignis test");

  assert!(
    !output.status.success(),
    "expected ignis test to fail for unsupported equality\nstdout:\n{}\nstderr:\n{}",
    String::from_utf8_lossy(&output.stdout),
    String::from_utf8_lossy(&output.stderr)
  );

  let stdout = String::from_utf8_lossy(&output.stdout);
  let stderr = String::from_utf8_lossy(&output.stderr);
  assert!(
    stdout.contains("A0191") || stderr.contains("A0191"),
    "expected unsupported equality diagnostic in command output\nstdout:\n{stdout}\nstderr:\n{stderr}"
  );
  assert!(
    !project_dir.join("build/bin/native_test_runner_fixture-tests").exists(),
    "expected harness build to stop before binary generation"
  );

  cleanup_project_dir(&project_dir);
}
