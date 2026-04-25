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

fn write_single_test_file(
  project_dir: &Path,
  file_name: &str,
  source: &str,
) -> PathBuf {
  let file_path = project_dir.join(file_name);
  fs::write(&file_path, source).expect("write single-file source");
  file_path
}

fn write_project_file(
  project_dir: &Path,
  relative_path: &str,
  contents: &str,
) {
  let file_path = project_dir.join(relative_path);

  fs::create_dir_all(file_path.parent().expect("project file parent")).expect("create project file dir");
  fs::write(file_path, contents).expect("write project file");
}

fn harness_binary_path(project_dir: &Path) -> PathBuf {
  project_dir.join("build/bin/native_test_runner_fixture-tests")
}

fn escape_snapshot_component(value: &str) -> String {
  let mut escaped = String::new();

  for byte in value.bytes() {
    let ch = byte as char;
    if ch.is_ascii_alphanumeric() || matches!(ch, '.' | '_' | '-') {
      escaped.push(ch);
    } else {
      escaped.push('_');
      escaped.push_str(&format!("{:02x}", byte));
    }
  }

  escaped
}

fn project_snapshot_path(
  project_dir: &Path,
  fq_name: &str,
  snapshot_name: &str,
) -> PathBuf {
  project_dir.join("src").join("__snapshots__").join(format!(
    "{}__{}.snap.txt",
    escape_snapshot_component(fq_name),
    escape_snapshot_component(snapshot_name)
  ))
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
fn ignis_test_surfaces_failing_assertion_diagnostics_in_project_mode() {
  let project_dir = make_temp_project_dir("failing-assertion-project");
  write_test_project(
    &project_dir,
    r#"
import Test from "std::test";

@test
function failingAssertion(): void {
    Test::assertEq(1, 2);
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
    "expected ignis test to fail for a failing assertion\nstdout:\n{}\nstderr:\n{}",
    String::from_utf8_lossy(&output.stdout),
    String::from_utf8_lossy(&output.stderr)
  );

  let stdout = String::from_utf8_lossy(&output.stdout);
  let stderr = String::from_utf8_lossy(&output.stderr);

  assert!(
    stdout.contains("Tests failed") || stderr.contains("Tests failed"),
    "expected failing cli test summary\nstdout:\n{stdout}\nstderr:\n{stderr}"
  );
  assert!(
    stdout.contains("main::failingAssertion") || stderr.contains("main::failingAssertion"),
    "expected fully qualified failing test name in command output\nstdout:\n{stdout}\nstderr:\n{stderr}"
  );
  assert!(
    stdout.contains("assertion failed: values are not equal") || stderr.contains("assertion failed: values are not equal"),
    "expected assertion panic text in command output\nstdout:\n{stdout}\nstderr:\n{stderr}"
  );
  assert!(
    stdout.contains("1 total") || stderr.contains("1 total"),
    "expected total count in command output\nstdout:\n{stdout}\nstderr:\n{stderr}"
  );
  assert!(
    stdout.contains("0 passed") || stderr.contains("0 passed"),
    "expected passed count in command output\nstdout:\n{stdout}\nstderr:\n{stderr}"
  );
  assert!(
    stdout.contains("1 failed") || stderr.contains("1 failed"),
    "expected failed count in command output\nstdout:\n{stdout}\nstderr:\n{stderr}"
  );
  assert!(harness_binary_path(&project_dir).exists(), "expected cli test failure to happen after harness build");

  cleanup_project_dir(&project_dir);
}

#[test]
fn ignis_test_updates_project_snapshots_via_cli() {
  let project_dir = make_temp_project_dir("project-snapshot-update");
  write_test_project(
    &project_dir,
    r#"
import Test from "std::test";

@test
function writesSnapshot(): void {
    Test::assertSnapshot("rendered", "new snapshot contents\n");
}
"#,
  );

  let snapshot_path = project_snapshot_path(&project_dir, "main::writesSnapshot", "rendered");
  write_project_file(
    &project_dir,
    "src/__snapshots__/main__writesSnapshot__rendered.snap.txt",
    "old snapshot contents\n",
  );

  let output = Command::new(env!("CARGO_BIN_EXE_ignis"))
    .arg("test")
    .arg("--project")
    .arg(&project_dir)
    .arg("--update-snapshots")
    .output()
    .expect("run ignis test project snapshot");

  if !output.status.success() {
    panic!(
      "expected project snapshot update to succeed\nstdout:\n{}\nstderr:\n{}",
      String::from_utf8_lossy(&output.stdout),
      String::from_utf8_lossy(&output.stderr)
    );
  }

  assert_eq!(
    fs::read_to_string(&snapshot_path).expect("read project snapshot"),
    "new snapshot contents\n"
  );

  cleanup_project_dir(&project_dir);
}

#[test]
fn ignis_test_creates_missing_project_snapshots_via_cli() {
  let project_dir = make_temp_project_dir("project-snapshot-create");
  write_test_project(
    &project_dir,
    r#"
import Test from "std::test";

@test
function writesSnapshot(): void {
    Test::assertSnapshot("rendered", "new snapshot contents\n");
}
"#,
  );

  let snapshot_path = project_snapshot_path(&project_dir, "main::writesSnapshot", "rendered");
  assert!(
    !snapshot_path.exists(),
    "expected project snapshot baseline to be missing before update mode"
  );

  let output = Command::new(env!("CARGO_BIN_EXE_ignis"))
    .arg("test")
    .arg("--project")
    .arg(&project_dir)
    .arg("--update-snapshots")
    .output()
    .expect("run ignis test project snapshot");

  if !output.status.success() {
    panic!(
      "expected missing project snapshot baseline creation to succeed\nstdout:\n{}\nstderr:\n{}",
      String::from_utf8_lossy(&output.stdout),
      String::from_utf8_lossy(&output.stderr)
    );
  }

  assert_eq!(
    fs::read_to_string(&snapshot_path).expect("read project snapshot"),
    "new snapshot contents\n"
  );

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

#[test]
fn ignis_test_reports_inline_snapshot_request_as_unsupported_in_project_mode() {
  let project_dir = make_temp_project_dir("inline-snapshot-project");
  write_test_project(
    &project_dir,
    r#"
import Test from "std::test";

@test
function invalidInlineSnapshot(): void {
    Test::assertInlineSnapshot("rendered", "value");
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
    "expected ignis test to reject unsupported inline snapshot requests\nstdout:\n{}\nstderr:\n{}",
    String::from_utf8_lossy(&output.stdout),
    String::from_utf8_lossy(&output.stderr)
  );

  let stdout = String::from_utf8_lossy(&output.stdout);
  let stderr = String::from_utf8_lossy(&output.stderr);
  assert!(
    stdout.contains("assertInlineSnapshot") || stderr.contains("assertInlineSnapshot"),
    "expected command output to name the unsupported inline snapshot surface\nstdout:\n{stdout}\nstderr:\n{stderr}"
  );
  assert!(
    !harness_binary_path(&project_dir).exists(),
    "expected harness build to stop before binary generation"
  );

  cleanup_project_dir(&project_dir);
}

#[test]
fn ignis_test_reports_inline_snapshot_request_as_unsupported_in_single_file_mode() {
  let project_dir = make_temp_project_dir("inline-snapshot-single-file");
  let file_path = write_single_test_file(
    &project_dir,
    "sample.ign",
    r#"
import Test from "std::test";

@test
function invalidInlineSnapshot(): void {
    Test::assertInlineSnapshot("rendered", "value");
}
"#,
  );

  let output = Command::new(env!("CARGO_BIN_EXE_ignis"))
    .arg("test")
    .arg(&file_path)
    .env("IGNIS_STD_PATH", workspace_std_path())
    .output()
    .expect("run ignis test single-file");

  assert!(
    !output.status.success(),
    "expected single-file ignis test to reject unsupported inline snapshot requests\nstdout:\n{}\nstderr:\n{}",
    String::from_utf8_lossy(&output.stdout),
    String::from_utf8_lossy(&output.stderr)
  );

  let stdout = String::from_utf8_lossy(&output.stdout);
  let stderr = String::from_utf8_lossy(&output.stderr);
  assert!(
    stdout.contains("assertInlineSnapshot") || stderr.contains("assertInlineSnapshot"),
    "expected single-file command output to name the unsupported inline snapshot surface\nstdout:\n{stdout}\nstderr:\n{stderr}"
  );
  assert!(
    !project_dir.join("build/bin/sample-tests").exists(),
    "expected harness build to stop before binary generation"
  );

  cleanup_project_dir(&project_dir);
}

#[test]
fn ignis_test_executes_single_file_input_via_cli() {
  let project_dir = make_temp_project_dir("single-file-generic-eq");
  let file_path = write_single_test_file(
    &project_dir,
    "sample.ign",
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
    .arg(&file_path)
    .env("IGNIS_STD_PATH", workspace_std_path())
    .output()
    .expect("run ignis test single-file");

  if !output.status.success() {
    panic!(
      "expected single-file ignis test to succeed\nstdout:\n{}\nstderr:\n{}",
      String::from_utf8_lossy(&output.stdout),
      String::from_utf8_lossy(&output.stderr)
    );
  }

  assert!(
    project_dir.join("build/bin/sample-tests").exists(),
    "expected single-file cli test run to build the sample test harness"
  );

  cleanup_project_dir(&project_dir);
}

#[test]
fn ignis_test_updates_single_file_snapshots_via_cli() {
  let project_dir = make_temp_project_dir("single-file-snapshot");
  let file_path = write_single_test_file(
    &project_dir,
    "sample.ign",
    r#"
import Test from "std::test";

@test
function writesSnapshot(): void {
    Test::assertSnapshot("rendered", "hello snapshot\n");
}
"#,
  );

  let output = Command::new(env!("CARGO_BIN_EXE_ignis"))
    .arg("test")
    .arg(&file_path)
    .arg("--update-snapshots")
    .env("IGNIS_STD_PATH", workspace_std_path())
    .output()
    .expect("run ignis test single-file snapshot");

  if !output.status.success() {
    panic!(
      "expected single-file snapshot update to succeed\nstdout:\n{}\nstderr:\n{}",
      String::from_utf8_lossy(&output.stdout),
      String::from_utf8_lossy(&output.stderr)
    );
  }

  let snapshot_path = project_dir.join("__snapshots__").join(format!(
    "{}__{}.snap.txt",
    escape_snapshot_component("sample::writesSnapshot"),
    escape_snapshot_component("rendered")
  ));
  assert_eq!(
    fs::read_to_string(&snapshot_path).expect("read single-file snapshot"),
    "hello snapshot\n"
  );

  cleanup_project_dir(&project_dir);
}
