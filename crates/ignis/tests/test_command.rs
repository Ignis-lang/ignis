use std::fs;
use std::io::Write;
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

  let project_dir = std::env::temp_dir().join(format!("ignis-cli-test-{label}-{}-{unique}", std::process::id()));

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

fn std_harness_binary_path(output_dir: &Path) -> PathBuf {
  output_dir.join("bin/std-tests")
}

fn std_snapshot_path(
  std_root: &Path,
  module_dir: &str,
  fq_name: &str,
  snapshot_name: &str,
) -> PathBuf {
  std_root.join(module_dir).join("__snapshots__").join(format!(
    "{}__{}.snap.txt",
    escape_snapshot_component(fq_name),
    escape_snapshot_component(snapshot_name)
  ))
}

fn read_optional_file(path: &Path) -> Option<Vec<u8>> {
  fs::read(path).ok()
}

struct FileRestoreGuard {
  path: PathBuf,
  contents: Vec<u8>,
}

impl Drop for FileRestoreGuard {
  fn drop(&mut self) {
    let _ = fs::write(&self.path, &self.contents);
  }
}

fn read_project_file(
  project_dir: &Path,
  relative_path: &str,
) -> String {
  fs::read_to_string(project_dir.join(relative_path)).expect("read project file")
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
  assert!(
    harness_binary_path(&project_dir).exists(),
    "expected cli test run to build the harness binary"
  );

  cleanup_project_dir(&project_dir);
}

#[test]
fn ignis_test_keeps_top_level_test_discovery_with_directive_functions_via_cli() {
  let project_dir = make_temp_project_dir("directive-test-compat");
  write_test_project(
    &project_dir,
    r#"
import Compile from "std::compile";
import Test from "std::test";

@directive(target: "record", phase: check, effect: diagnose)
function derive(context: Compile::Context, target: Compile::ItemReference): void {
    return;
}

@cold
function legacyHelper(): void {}

@test
function smoke(): void {
    legacyHelper();
    Test::assert(true);
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
    output.status.success(),
    "expected ignis test to preserve top-level @test discovery when directive functions coexist with legacy attrs\nstdout:\n{}\nstderr:\n{}",
    String::from_utf8_lossy(&output.stdout),
    String::from_utf8_lossy(&output.stderr)
  );

  let stdout = String::from_utf8_lossy(&output.stdout);
  let stderr = String::from_utf8_lossy(&output.stderr);

  assert!(
    stdout.contains("Tests passed") || stderr.contains("Tests passed"),
    "expected successful cli test summary\nstdout:\n{stdout}\nstderr:\n{stderr}"
  );
  assert!(
    harness_binary_path(&project_dir).exists(),
    "expected cli test run to build the harness binary"
  );

  cleanup_project_dir(&project_dir);
}

#[test]
fn ignis_test_std_executes_workspace_std_tests_via_cli_from_outside_project_root() {
  let project_dir = make_temp_project_dir("test-std-outside-project");
  let output_dir = project_dir.join("std-build");

  let output = Command::new(env!("CARGO_BIN_EXE_ignis"))
    .current_dir(&project_dir)
    .arg("test-std")
    .arg("vector::tests::clearDropsStringElementsBeforeReuse")
    .arg("--std-path")
    .arg(workspace_std_path())
    .arg("--output-dir")
    .arg(&output_dir)
    .output()
    .expect("run ignis test-std");

  assert!(
    output.status.success(),
    "expected ignis test-std to succeed from outside any project root\nstdout:\n{}\nstderr:\n{}",
    String::from_utf8_lossy(&output.stdout),
    String::from_utf8_lossy(&output.stderr)
  );

  let stdout = String::from_utf8_lossy(&output.stdout);
  let stderr = String::from_utf8_lossy(&output.stderr);

  assert!(
    stdout.contains("Tests passed") || stderr.contains("Tests passed"),
    "expected successful std cli test summary\nstdout:\n{stdout}\nstderr:\n{stderr}"
  );
  assert!(
    std_harness_binary_path(&output_dir).exists(),
    "expected std cli test run to build the harness binary"
  );

  cleanup_project_dir(&project_dir);
}

#[test]
fn ignis_test_std_executes_workspace_time_tests_via_cli() {
  let project_dir = make_temp_project_dir("test-std-time-module");
  let output_dir = project_dir.join("std-build");

  let output = Command::new(env!("CARGO_BIN_EXE_ignis"))
    .current_dir(&project_dir)
    .arg("test-std")
    .arg("time::tests")
    .arg("--std-path")
    .arg(workspace_std_path())
    .arg("--output-dir")
    .arg(&output_dir)
    .output()
    .expect("run ignis test-std time::tests");

  assert!(
    output.status.success(),
    "expected ignis test-std time::tests to succeed\nstdout:\n{}\nstderr:\n{}",
    String::from_utf8_lossy(&output.stdout),
    String::from_utf8_lossy(&output.stderr)
  );

  let stdout = String::from_utf8_lossy(&output.stdout);
  let stderr = String::from_utf8_lossy(&output.stderr);

  assert!(
    stdout.contains("time::tests::") || stderr.contains("time::tests::"),
    "expected std cli test output to mention time::tests entries\nstdout:\n{stdout}\nstderr:\n{stderr}"
  );
  assert!(
    stdout.contains("Tests passed") || stderr.contains("Tests passed"),
    "expected successful std cli test summary\nstdout:\n{stdout}\nstderr:\n{stderr}"
  );
  assert!(
    std_harness_binary_path(&output_dir).exists(),
    "expected std cli time test run to build the harness binary"
  );

  cleanup_project_dir(&project_dir);
}

#[test]
fn ignis_test_std_updates_workspace_snapshots_via_cli() {
  let project_dir = make_temp_project_dir("test-std-update-snapshots");
  let output_dir = project_dir.join("std-build");
  let snapshot_path = std_snapshot_path(
    &workspace_std_path(),
    "string",
    "string::tests::snapshotStdRunnerSmoke",
    "smoke",
  );

  let original_snapshot = read_optional_file(&snapshot_path).expect("expected committed std snapshot baseline");
  let _restore_snapshot = FileRestoreGuard {
    path: snapshot_path.clone(),
    contents: original_snapshot.clone(),
  };

  let _ = fs::remove_file(&snapshot_path);

  let output = Command::new(env!("CARGO_BIN_EXE_ignis"))
    .current_dir(&project_dir)
    .arg("test-std")
    .arg("string::tests::snapshotStdRunnerSmoke")
    .arg("--update-snapshots")
    .arg("--std-path")
    .arg(workspace_std_path())
    .arg("--output-dir")
    .arg(&output_dir)
    .output()
    .expect("run ignis test-std --update-snapshots");

  assert!(
    output.status.success(),
    "expected ignis test-std --update-snapshots to succeed\nstdout:\n{}\nstderr:\n{}",
    String::from_utf8_lossy(&output.stdout),
    String::from_utf8_lossy(&output.stderr)
  );
  assert!(snapshot_path.exists(), "expected std snapshot file to be created");
  assert_eq!(
    read_optional_file(&snapshot_path).as_deref(),
    Some(original_snapshot.as_slice()),
    "expected std snapshot update smoke test to restore the committed baseline contents"
  );

  cleanup_project_dir(&project_dir);
}

#[test]
fn ignis_fmt_rewrites_single_file_in_place() {
  let project_dir = make_temp_project_dir("fmt-single-file");
  let file_path = write_single_test_file(&project_dir, "sample.ign", "function   main ( ) : void {return;}\n");

  let output = Command::new(env!("CARGO_BIN_EXE_ignis"))
    .arg("fmt")
    .arg(&file_path)
    .output()
    .expect("run ignis fmt single file");

  assert!(
    output.status.success(),
    "expected ignis fmt single-file run to succeed\nstdout:\n{}\nstderr:\n{}",
    String::from_utf8_lossy(&output.stdout),
    String::from_utf8_lossy(&output.stderr)
  );
  assert_eq!(
    fs::read_to_string(&file_path).expect("read formatted file"),
    "function main(): void {\n  return;\n}\n"
  );

  cleanup_project_dir(&project_dir);
}

#[test]
fn ignis_fmt_rewrites_multiple_explicit_files_in_place() {
  let project_dir = make_temp_project_dir("fmt-multi-file");
  let first = write_single_test_file(&project_dir, "first.ign", "function   first ( ) : void {return;}\n");
  let second = write_single_test_file(&project_dir, "second.ign", "function   second ( ) : void {return;}\n");

  let output = Command::new(env!("CARGO_BIN_EXE_ignis"))
    .arg("fmt")
    .arg(&first)
    .arg(&second)
    .output()
    .expect("run ignis fmt multi file");

  assert!(
    output.status.success(),
    "expected ignis fmt multi-file run to succeed\nstdout:\n{}\nstderr:\n{}",
    String::from_utf8_lossy(&output.stdout),
    String::from_utf8_lossy(&output.stderr)
  );
  assert_eq!(
    fs::read_to_string(&first).expect("read first formatted file"),
    "function first(): void {\n  return;\n}\n"
  );
  assert_eq!(
    fs::read_to_string(&second).expect("read second formatted file"),
    "function second(): void {\n  return;\n}\n"
  );

  cleanup_project_dir(&project_dir);
}

#[test]
fn ignis_fmt_check_reports_dirty_single_file_without_rewriting() {
  let project_dir = make_temp_project_dir("fmt-check-single-file");
  let file_path = write_single_test_file(&project_dir, "sample.ign", "function   main ( ) : void {return;}\n");

  let output = Command::new(env!("CARGO_BIN_EXE_ignis"))
    .arg("fmt")
    .arg("--check")
    .arg(&file_path)
    .output()
    .expect("run ignis fmt --check single file");

  assert!(
    !output.status.success(),
    "expected ignis fmt --check to fail on dirty file\nstdout:\n{}\nstderr:\n{}",
    String::from_utf8_lossy(&output.stdout),
    String::from_utf8_lossy(&output.stderr)
  );
  assert_eq!(
    fs::read_to_string(&file_path).expect("read unchecked file"),
    "function   main ( ) : void {return;}\n"
  );

  cleanup_project_dir(&project_dir);
}

#[test]
fn ignis_fmt_formats_all_project_sources() {
  let project_dir = make_temp_project_dir("fmt-project");
  write_test_project(&project_dir, "function   main ( ) : void {return;}\n");
  write_project_file(&project_dir, "src/util.ign", "function   helper ( ) : void {return;}\n");

  let output = Command::new(env!("CARGO_BIN_EXE_ignis"))
    .arg("fmt")
    .arg("--project")
    .arg(&project_dir)
    .output()
    .expect("run ignis fmt project");

  assert!(
    output.status.success(),
    "expected ignis fmt project run to succeed\nstdout:\n{}\nstderr:\n{}",
    String::from_utf8_lossy(&output.stdout),
    String::from_utf8_lossy(&output.stderr)
  );
  assert_eq!(
    read_project_file(&project_dir, "src/main.ign"),
    "function main(): void {\n  return;\n}\n"
  );
  assert_eq!(
    read_project_file(&project_dir, "src/util.ign"),
    "function helper(): void {\n  return;\n}\n"
  );

  cleanup_project_dir(&project_dir);
}

#[test]
fn ignis_fmt_cli_indent_width_override_beats_project_and_dedicated_config() {
  let project_dir = make_temp_project_dir("fmt-indent-width-precedence");
  write_test_project(
    &project_dir,
    r#"@if(featureFlag) {
/// docs
function   main ( ) : void {return;}
}
"#,
  );
  fs::write(
    project_dir.join("ignis.toml"),
    format!(
      "[package]\nname = \"native_test_runner_fixture\"\nversion = \"0.1.0\"\nauthors = []\ndescription = \"fixture\"\nkeywords = []\nlicense = \"MIT\"\nrepository = \"\"\n\n[ignis]\nstd = true\nstd_path = \"{}\"\n\n[build]\nbin = true\nsource_dir = \"src\"\nentry = \"main.ign\"\nout_dir = \"build\"\nopt_level = 0\ndebug = false\ntarget = \"c\"\ncc = \"cc\"\ncflags = []\nemit = []\n\n[formatter]\nindent_width = 2\nline_width = 90\n",
      workspace_std_path().display()
    ),
  )
  .expect("rewrite ignis.toml with formatter bridge");
  write_project_file(&project_dir, "ignisfmt.toml", "indent_width = 6\nline_width = 120\n");

  let file_path = project_dir.join("src/main.ign");
  let output = Command::new(env!("CARGO_BIN_EXE_ignis"))
    .arg("fmt")
    .arg("--indent-width")
    .arg("3")
    .arg(&file_path)
    .output()
    .expect("run ignis fmt with indent width override");

  assert!(
    output.status.success(),
    "expected ignis fmt indent-width override run to succeed\nstdout:\n{}\nstderr:\n{}",
    String::from_utf8_lossy(&output.stdout),
    String::from_utf8_lossy(&output.stderr)
  );
  assert_eq!(
    fs::read_to_string(&file_path).expect("read override-formatted file"),
    "@if(featureFlag) {\n   /// docs\n   function main(): void {\n      return;\n   }\n}\n"
  );

  cleanup_project_dir(&project_dir);
}

#[test]
fn ignis_fmt_cli_spaces_override_beats_project_and_dedicated_config_tabs() {
  let project_dir = make_temp_project_dir("fmt-tabs-precedence");
  write_test_project(
    &project_dir,
    r#"@if(featureFlag) {
/// docs
function   main ( ) : void {return;}
}
"#,
  );
  fs::write(
    project_dir.join("ignis.toml"),
    format!(
      "[package]\nname = \"native_test_runner_fixture\"\nversion = \"0.1.0\"\nauthors = []\ndescription = \"fixture\"\nkeywords = []\nlicense = \"MIT\"\nrepository = \"\"\n\n[ignis]\nstd = true\nstd_path = \"{}\"\n\n[build]\nbin = true\nsource_dir = \"src\"\nentry = \"main.ign\"\nout_dir = \"build\"\nopt_level = 0\ndebug = false\ntarget = \"c\"\ncc = \"cc\"\ncflags = []\nemit = []\n\n[formatter]\nindent_width = 4\nline_width = 90\nuse_tabs = true\n",
      workspace_std_path().display()
    ),
  )
  .expect("rewrite ignis.toml with formatter bridge");
  write_project_file(
    &project_dir,
    "ignisfmt.toml",
    "indent_width = 6\nline_width = 120\nuse_tabs = true\n",
  );

  let file_path = project_dir.join("src/main.ign");
  let output = Command::new(env!("CARGO_BIN_EXE_ignis"))
    .arg("fmt")
    .arg("--spaces")
    .arg("--indent-width")
    .arg("3")
    .arg(&file_path)
    .output()
    .expect("run ignis fmt with spaces override");

  assert!(
    output.status.success(),
    "expected ignis fmt spaces override run to succeed\nstdout:\n{}\nstderr:\n{}",
    String::from_utf8_lossy(&output.stdout),
    String::from_utf8_lossy(&output.stderr)
  );
  assert_eq!(
    fs::read_to_string(&file_path).expect("read spaces override-formatted file"),
    "@if(featureFlag) {\n   /// docs\n   function main(): void {\n      return;\n   }\n}\n"
  );

  cleanup_project_dir(&project_dir);
}

#[test]
fn ignis_fmt_cli_use_tabs_override_rewrites_with_tabs() {
  let project_dir = make_temp_project_dir("fmt-use-tabs");
  let file_path = write_single_test_file(&project_dir, "sample.ign", "function   main ( ) : void {return;}\n");

  let output = Command::new(env!("CARGO_BIN_EXE_ignis"))
    .arg("fmt")
    .arg("--use-tabs")
    .arg("--indent-width")
    .arg("4")
    .arg(&file_path)
    .output()
    .expect("run ignis fmt with tabs override");

  assert!(
    output.status.success(),
    "expected ignis fmt tabs override run to succeed\nstdout:\n{}\nstderr:\n{}",
    String::from_utf8_lossy(&output.stdout),
    String::from_utf8_lossy(&output.stderr)
  );
  assert_eq!(
    fs::read_to_string(&file_path).expect("read tab-formatted file"),
    "function main(): void {\n\treturn;\n}\n"
  );

  cleanup_project_dir(&project_dir);
}

#[test]
fn ignis_fmt_rejects_invalid_file_without_rewriting() {
  let project_dir = make_temp_project_dir("fmt-invalid-file");
  let file_path = write_single_test_file(
    &project_dir,
    "broken.ign",
    "function main(): void { let broken = \"unterminated; }\n",
  );

  let output = Command::new(env!("CARGO_BIN_EXE_ignis"))
    .arg("fmt")
    .arg(&file_path)
    .output()
    .expect("run ignis fmt invalid file");

  assert!(
    !output.status.success(),
    "expected ignis fmt invalid-file run to fail\nstdout:\n{}\nstderr:\n{}",
    String::from_utf8_lossy(&output.stdout),
    String::from_utf8_lossy(&output.stderr)
  );
  assert_eq!(
    fs::read_to_string(&file_path).expect("read invalid file after fmt"),
    "function main(): void { let broken = \"unterminated; }\n"
  );

  cleanup_project_dir(&project_dir);
}

#[test]
fn ignis_fmt_formats_valid_namespace_code_without_error() {
  let project_dir = make_temp_project_dir("fmt-namespace-file");
  let source = "export namespace CType {\n    type CVoidPtr = *mut void;\n}\n";
  let file_path = write_single_test_file(&project_dir, "namespace.ign", source);

  let output = Command::new(env!("CARGO_BIN_EXE_ignis"))
    .arg("fmt")
    .arg(&file_path)
    .output()
    .expect("run ignis fmt namespace file");

  assert!(
    output.status.success(),
    "expected ignis fmt to succeed on valid namespace code\nstdout:\n{}\nstderr:\n{}",
    String::from_utf8_lossy(&output.stdout),
    String::from_utf8_lossy(&output.stderr)
  );
  assert!(
    String::from_utf8_lossy(&output.stderr).is_empty(),
    "expected no warnings for valid namespace code\nstderr:\n{}",
    String::from_utf8_lossy(&output.stderr)
  );

  let formatted = fs::read_to_string(&file_path).expect("read namespace file after fmt");
  assert!(
    formatted.contains("CType") && formatted.contains("CVoidPtr"),
    "formatted output should preserve namespace content, got: {formatted}"
  );

  cleanup_project_dir(&project_dir);
}

#[test]
fn ignis_fmt_reports_failing_file_path_on_safety_validation_error() {
  let project_dir = make_temp_project_dir("fmt-error-path");
  let file_path = write_single_test_file(
    &project_dir,
    "problem.ign",
    "export inline function add(a: i32, b: i32): i32 { return a + b; }\n",
  );

  let output = Command::new(env!("CARGO_BIN_EXE_ignis"))
    .arg("fmt")
    .arg(&file_path)
    .output()
    .expect("run ignis fmt failing file");

  assert!(
    !output.status.success(),
    "expected ignis fmt to fail on safety validation drift\nstdout:\n{}\nstderr:\n{}",
    String::from_utf8_lossy(&output.stdout),
    String::from_utf8_lossy(&output.stderr)
  );

  let stderr = String::from_utf8_lossy(&output.stderr);
  assert!(
    stderr.contains(file_path.to_string_lossy().as_ref()),
    "expected stderr to include failing file path\nstderr:\n{stderr}"
  );
  assert!(
    stderr.contains("token shape") || stderr.contains("formatter safety validation failed"),
    "expected stderr to include formatter validation failure\nstderr:\n{stderr}"
  );

  cleanup_project_dir(&project_dir);
}

#[test]
fn ignis_fmt_cli_sort_imports_flag_is_parsed() {
  let project_dir = make_temp_project_dir("fmt-sort-imports-parse");
  let source = "import Io from \"std::io\";\nimport Fs from \"std::fs\";\n\nfunction main(): void { return; }\n";
  let file_path = write_single_test_file(&project_dir, "sample.ign", source);

  let output = Command::new(env!("CARGO_BIN_EXE_ignis"))
    .arg("fmt")
    .arg("--sort-imports")
    .arg(&file_path)
    .output()
    .expect("run ignis fmt --sort-imports");

  assert!(
    output.status.success(),
    "expected ignis fmt --sort-imports to succeed\nstdout:\n{}\nstderr:\n{}",
    String::from_utf8_lossy(&output.stdout),
    String::from_utf8_lossy(&output.stderr)
  );

  let formatted = fs::read_to_string(&file_path).expect("read formatted file");
  assert!(
    formatted.contains("import Fs") && formatted.contains("import Io"),
    "expected both imports present in formatted output:\n{formatted}"
  );

  cleanup_project_dir(&project_dir);
}

#[test]
fn ignis_fmt_cli_sort_imports_overrides_config_file_false() {
  let project_dir = make_temp_project_dir("fmt-sort-imports-override");
  write_test_project(
    &project_dir,
    "import Io from \"std::io\";\nimport Fs from \"std::fs\";\n\nfunction main(): void { return; }\n",
  );
  write_project_file(&project_dir, "ignisfmt.toml", "sort_imports = false\n");

  let file_path = project_dir.join("src/main.ign");
  let output = Command::new(env!("CARGO_BIN_EXE_ignis"))
    .arg("fmt")
    .arg("--sort-imports")
    .arg(&file_path)
    .output()
    .expect("run ignis fmt --sort-imports overriding config");

  assert!(
    output.status.success(),
    "expected ignis fmt --sort-imports override to succeed\nstdout:\n{}\nstderr:\n{}",
    String::from_utf8_lossy(&output.stdout),
    String::from_utf8_lossy(&output.stderr)
  );

  let formatted = fs::read_to_string(&file_path).expect("read formatted file");
  let fs_pos = formatted.find("import Fs").expect("import Fs line");
  let io_pos = formatted.find("import Io").expect("import Io line");
  assert!(
    fs_pos < io_pos,
    "expected imports sorted (Fs before Io) when --sort-imports overrides config=false\nformatted:\n{formatted}"
  );

  cleanup_project_dir(&project_dir);
}

#[test]
fn ignis_fmt_respects_sort_imports_from_config_file() {
  let project_dir = make_temp_project_dir("fmt-sort-imports-config");
  write_test_project(
    &project_dir,
    "import Io from \"std::io\";\nimport Fs from \"std::fs\";\n\nfunction main(): void { return; }\n",
  );
  write_project_file(&project_dir, "ignisfmt.toml", "sort_imports = true\n");

  let file_path = project_dir.join("src/main.ign");
  let output = Command::new(env!("CARGO_BIN_EXE_ignis"))
    .arg("fmt")
    .arg("--project")
    .arg(&project_dir)
    .output()
    .expect("run ignis fmt with sort_imports=true config");

  assert!(
    output.status.success(),
    "expected ignis fmt with sort_imports config to succeed\nstdout:\n{}\nstderr:\n{}",
    String::from_utf8_lossy(&output.stdout),
    String::from_utf8_lossy(&output.stderr)
  );

  let formatted = fs::read_to_string(&file_path).expect("read formatted file");
  let fs_pos = formatted.find("import Fs").expect("import Fs line");
  let io_pos = formatted.find("import Io").expect("import Io line");
  assert!(
    fs_pos < io_pos,
    "expected imports sorted when config sort_imports=true\nformatted:\n{formatted}"
  );

  cleanup_project_dir(&project_dir);
}

#[test]
fn ignis_fmt_default_does_not_sort_imports() {
  let project_dir = make_temp_project_dir("fmt-no-sort-default");
  let source = "import Io from \"std::io\";\nimport Fs from \"std::fs\";\n\nfunction main(): void { return; }\n";
  let file_path = write_single_test_file(&project_dir, "sample.ign", source);

  let output = Command::new(env!("CARGO_BIN_EXE_ignis"))
    .arg("fmt")
    .arg(&file_path)
    .output()
    .expect("run ignis fmt without --sort-imports");

  assert!(
    output.status.success(),
    "expected ignis fmt to succeed\nstdout:\n{}\nstderr:\n{}",
    String::from_utf8_lossy(&output.stdout),
    String::from_utf8_lossy(&output.stderr)
  );

  let formatted = fs::read_to_string(&file_path).expect("read formatted file");
  let io_pos = formatted.find("import Io").expect("import Io line");
  let fs_pos = formatted.find("import Fs").expect("import Fs line");
  assert!(
    io_pos < fs_pos,
    "expected imports NOT reordered by default (Io before Fs as written)\nformatted:\n{formatted}"
  );

  cleanup_project_dir(&project_dir);
}

#[test]
fn ignis_fmt_stdin_json_formats_single_record() {
  let input = "{\"path\":\"std/fs/mod.ign\",\"text\":\"function   main ( ) : void {return;}\\n\"}\n";

  let mut child = Command::new(env!("CARGO_BIN_EXE_ignis"))
    .arg("fmt")
    .arg("--stdin-json")
    .stdin(std::process::Stdio::piped())
    .stdout(std::process::Stdio::piped())
    .stderr(std::process::Stdio::piped())
    .spawn()
    .expect("spawn ignis fmt --stdin-json");

  {
    let stdin = child.stdin.as_mut().expect("stdin pipe");
    stdin.write_all(input.as_bytes()).expect("write stdin");
  }

  let result = child.wait_with_output().expect("wait for output");
  assert!(
    result.status.success(),
    "expected ignis fmt --stdin-json to succeed\nstdout:\n{}\nstderr:\n{}",
    String::from_utf8_lossy(&result.stdout),
    String::from_utf8_lossy(&result.stderr)
  );

  let stdout = String::from_utf8_lossy(&result.stdout);
  let response: serde_json::Value = serde_json::from_str(stdout.trim()).expect("parse NDJSON response");

  assert_eq!(response["path"], "std/fs/mod.ign", "path should echo back");
  assert!(
    response["changed"].as_bool().unwrap_or(false),
    "expected changed=true for dirty input\nresponse: {response}"
  );
  let formatted = response["formatted"]
    .as_str()
    .expect("formatted field should be a string");
  assert!(
    formatted.contains("function main(): void"),
    "expected formatted output to contain canonical layout\nformatted: {formatted}"
  );
}

#[test]
fn ignis_fmt_stdin_json_reports_unchanged() {
  let canonical = "function main(): void {\n  return;\n}\n";
  let input = format!(
    "{{\"path\":\"clean.ign\",\"text\":{}}}\n",
    serde_json::to_string(canonical).unwrap()
  );

  let mut child = Command::new(env!("CARGO_BIN_EXE_ignis"))
    .arg("fmt")
    .arg("--stdin-json")
    .stdin(std::process::Stdio::piped())
    .stdout(std::process::Stdio::piped())
    .stderr(std::process::Stdio::piped())
    .spawn()
    .expect("spawn ignis fmt --stdin-json");

  {
    let stdin = child.stdin.as_mut().expect("stdin pipe");
    stdin.write_all(input.as_bytes()).expect("write stdin");
  }

  let result = child.wait_with_output().expect("wait for output");
  assert!(
    result.status.success(),
    "expected ignis fmt --stdin-json to succeed\nstdout:\n{}\nstderr:\n{}",
    String::from_utf8_lossy(&result.stdout),
    String::from_utf8_lossy(&result.stderr)
  );

  let stdout = String::from_utf8_lossy(&result.stdout);
  let response: serde_json::Value = serde_json::from_str(stdout.trim()).expect("parse NDJSON response");
  assert_eq!(response["path"], "clean.ign");
  assert!(
    !response["changed"].as_bool().unwrap_or(true),
    "expected changed=false for already-canonical input\nresponse: {response}"
  );
}

#[test]
fn ignis_fmt_stdin_json_batch_processes_multiple_records() {
  let record1 = format!(
    "{{\"path\":\"a.ign\",\"text\":{}}}",
    serde_json::to_string("function   foo ( ) : void {return;}\n").unwrap()
  );
  let record2 = format!(
    "{{\"path\":\"b.ign\",\"text\":{}}}",
    serde_json::to_string("function bar(): void {\n  return;\n}\n").unwrap()
  );
  let input = format!("{record1}\n{record2}\n");

  let mut child = Command::new(env!("CARGO_BIN_EXE_ignis"))
    .arg("fmt")
    .arg("--stdin-json")
    .stdin(std::process::Stdio::piped())
    .stdout(std::process::Stdio::piped())
    .stderr(std::process::Stdio::piped())
    .spawn()
    .expect("spawn ignis fmt --stdin-json");

  {
    let stdin = child.stdin.as_mut().expect("stdin pipe");
    stdin.write_all(input.as_bytes()).expect("write stdin");
  }

  let result = child.wait_with_output().expect("wait for output");
  assert!(
    result.status.success(),
    "expected batch to succeed\nstdout:\n{}\nstderr:\n{}",
    String::from_utf8_lossy(&result.stdout),
    String::from_utf8_lossy(&result.stderr)
  );

  let stdout = String::from_utf8_lossy(&result.stdout);
  let lines: Vec<&str> = stdout.trim().lines().collect();
  assert_eq!(lines.len(), 2, "expected 2 NDJSON response lines\nstdout: {stdout}");

  let resp1: serde_json::Value = serde_json::from_str(lines[0]).expect("parse first record");
  let resp2: serde_json::Value = serde_json::from_str(lines[1]).expect("parse second record");

  assert_eq!(resp1["path"], "a.ign");
  assert!(resp1["changed"].as_bool().unwrap_or(false), "a.ign should be changed");
  assert_eq!(resp2["path"], "b.ign");
  assert!(!resp2["changed"].as_bool().unwrap_or(true), "b.ign should be unchanged");
}

#[test]
fn ignis_fmt_stdin_json_check_mode_reports_dirty() {
  let input = format!(
    "{{\"path\":\"dirty.ign\",\"text\":{}}}\n",
    serde_json::to_string("function   main ( ) : void {return;}\n").unwrap()
  );

  let mut child = Command::new(env!("CARGO_BIN_EXE_ignis"))
    .arg("fmt")
    .arg("--check")
    .arg("--stdin-json")
    .stdin(std::process::Stdio::piped())
    .stdout(std::process::Stdio::piped())
    .stderr(std::process::Stdio::piped())
    .spawn()
    .expect("spawn ignis fmt --check --stdin-json");

  {
    let stdin = child.stdin.as_mut().expect("stdin pipe");
    stdin.write_all(input.as_bytes()).expect("write stdin");
  }

  let result = child.wait_with_output().expect("wait for output");
  assert!(
    !result.status.success(),
    "expected --check --stdin-json to fail on dirty input\nstdout:\n{}\nstderr:\n{}",
    String::from_utf8_lossy(&result.stdout),
    String::from_utf8_lossy(&result.stderr)
  );

  let stdout = String::from_utf8_lossy(&result.stdout);
  let response: serde_json::Value = serde_json::from_str(stdout.trim()).expect("parse NDJSON response");
  assert_eq!(response["path"], "dirty.ign");
  assert!(
    response["changed"].as_bool().unwrap_or(false),
    "expected changed=true in check mode for dirty file\nresponse: {response}"
  );
}

#[test]
fn ignis_fmt_stdin_json_diff_mode_includes_diff() {
  let input = format!(
    "{{\"path\":\"diff.ign\",\"text\":{}}}\n",
    serde_json::to_string("function   main ( ) : void {return;}\n").unwrap()
  );

  let mut child = Command::new(env!("CARGO_BIN_EXE_ignis"))
    .arg("fmt")
    .arg("--emit")
    .arg("diff")
    .arg("--stdin-json")
    .stdin(std::process::Stdio::piped())
    .stdout(std::process::Stdio::piped())
    .stderr(std::process::Stdio::piped())
    .spawn()
    .expect("spawn ignis fmt --emit diff --stdin-json");

  {
    let stdin = child.stdin.as_mut().expect("stdin pipe");
    stdin.write_all(input.as_bytes()).expect("write stdin");
  }

  let result = child.wait_with_output().expect("wait for output");
  assert!(
    result.status.success(),
    "expected diff mode to succeed\nstdout:\n{}\nstderr:\n{}",
    String::from_utf8_lossy(&result.stdout),
    String::from_utf8_lossy(&result.stderr)
  );

  let stdout = String::from_utf8_lossy(&result.stdout);
  let response: serde_json::Value = serde_json::from_str(stdout.trim()).expect("parse NDJSON response");
  assert_eq!(response["path"], "diff.ign");
  assert!(
    response["changed"].as_bool().unwrap_or(false),
    "expected changed=true for dirty input in diff mode\nresponse: {response}"
  );
  assert!(
    response["diff"].as_str().is_some(),
    "expected 'diff' field in diff mode\nresponse: {response}"
  );
}

#[test]
fn ignis_fmt_stdin_json_reports_parse_error_for_invalid_record() {
  let input = "{\"path\":\"bad.ign\",\"text\":\"function main(): void { let broken = \\\"unterminated; }\\n\"}\n";

  let mut child = Command::new(env!("CARGO_BIN_EXE_ignis"))
    .arg("fmt")
    .arg("--stdin-json")
    .stdin(std::process::Stdio::piped())
    .stdout(std::process::Stdio::piped())
    .stderr(std::process::Stdio::piped())
    .spawn()
    .expect("spawn ignis fmt --stdin-json with parse error");

  {
    let stdin = child.stdin.as_mut().expect("stdin pipe");
    stdin.write_all(input.as_bytes()).expect("write stdin");
  }

  let result = child.wait_with_output().expect("wait for output");
  assert!(
    !result.status.success(),
    "expected failure when stdin-json record has parse error\nstdout:\n{}\nstderr:\n{}",
    String::from_utf8_lossy(&result.stdout),
    String::from_utf8_lossy(&result.stderr)
  );

  let stdout = String::from_utf8_lossy(&result.stdout);
  let response: serde_json::Value = serde_json::from_str(stdout.trim()).expect("parse NDJSON response");
  assert_eq!(response["path"], "bad.ign");
  assert!(
    response["error"].as_str().is_some(),
    "expected error field for parse-invalid record\nresponse: {response}"
  );
}

#[test]
fn ignis_fmt_stdin_json_sort_imports_sorts_via_cli_flag() {
  let source = "import Io from \"std::io\";\nimport Fs from \"std::fs\";\n\nfunction main(): void { return; }\n";
  let input = format!(
    "{{\"path\":\"sort.ign\",\"text\":{}}}\n",
    serde_json::to_string(source).unwrap()
  );

  let mut child = Command::new(env!("CARGO_BIN_EXE_ignis"))
    .arg("fmt")
    .arg("--stdin-json")
    .arg("--sort-imports")
    .stdin(std::process::Stdio::piped())
    .stdout(std::process::Stdio::piped())
    .stderr(std::process::Stdio::piped())
    .spawn()
    .expect("spawn ignis fmt --stdin-json --sort-imports");

  {
    let stdin = child.stdin.as_mut().expect("stdin pipe");
    stdin.write_all(input.as_bytes()).expect("write stdin");
  }

  let result = child.wait_with_output().expect("wait for output");
  assert!(
    result.status.success(),
    "expected --stdin-json --sort-imports to succeed\nstdout:\n{}\nstderr:\n{}",
    String::from_utf8_lossy(&result.stdout),
    String::from_utf8_lossy(&result.stderr)
  );

  let stdout = String::from_utf8_lossy(&result.stdout);
  let response: serde_json::Value = serde_json::from_str(stdout.trim()).expect("parse NDJSON response");
  let formatted = response["formatted"].as_str().expect("formatted field");
  let fs_pos = formatted.find("import Fs").expect("import Fs");
  let io_pos = formatted.find("import Io").expect("import Io");
  assert!(
    fs_pos < io_pos,
    "expected sorted imports (Fs before Io) via --sort-imports in stdin-json\nformatted:\n{formatted}"
  );
}

#[test]
fn ignis_fmt_help_mentions_sort_imports_and_stdin_json() {
  let output = Command::new(env!("CARGO_BIN_EXE_ignis"))
    .arg("fmt")
    .arg("--help")
    .output()
    .expect("run ignis fmt --help");

  assert!(
    output.status.success(),
    "expected ignis fmt --help to succeed\nstdout:\n{}\nstderr:\n{}",
    String::from_utf8_lossy(&output.stdout),
    String::from_utf8_lossy(&output.stderr)
  );

  let stdout = String::from_utf8_lossy(&output.stdout);
  assert!(
    stdout.contains("sort-imports"),
    "expected fmt help to mention --sort-imports\nstdout:\n{stdout}"
  );
  assert!(
    stdout.contains("stdin-json"),
    "expected fmt help to mention --stdin-json\nstdout:\n{stdout}"
  );
}

#[test]
fn ignis_fmt_help_mentions_safety_and_check_mode() {
  let output = Command::new(env!("CARGO_BIN_EXE_ignis"))
    .arg("fmt")
    .arg("--help")
    .output()
    .expect("run ignis fmt --help");

  assert!(
    output.status.success(),
    "expected ignis fmt --help to succeed\nstdout:\n{}\nstderr:\n{}",
    String::from_utf8_lossy(&output.stdout),
    String::from_utf8_lossy(&output.stderr)
  );

  let stdout = String::from_utf8_lossy(&output.stdout);

  assert!(
    stdout.contains("parse/reparse safety checks"),
    "expected fmt help to mention safety validation\nstdout:\n{stdout}"
  );
  assert!(
    stdout.contains("without rewriting files"),
    "expected fmt help to describe check mode as no-write validation\nstdout:\n{stdout}"
  );
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
    stdout.contains("assertion failed: values are not equal")
      || stderr.contains("assertion failed: values are not equal"),
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
  assert!(
    harness_binary_path(&project_dir).exists(),
    "expected cli test failure to happen after harness build"
  );

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
