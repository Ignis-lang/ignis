mod common;

use std::fs;
use std::path::{Path, PathBuf};

use ignis_driver::{run_project_tests, run_single_file_tests, run_std_tests};
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

fn write_project_module(
  project_root: &Path,
  relative_path: &str,
  source: &str,
) {
  let module_path = project_root.join("src").join(relative_path);
  fs::create_dir_all(module_path.parent().expect("module parent")).expect("create module dir");
  fs::write(module_path, source).expect("write module source");
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

fn snapshot_file_path(
  project_root: &Path,
  fq_name: &str,
  snapshot_name: &str,
) -> PathBuf {
  project_root.join("src").join("__snapshots__").join(format!(
    "{}__{}.snap.txt",
    escape_snapshot_component(fq_name),
    escape_snapshot_component(snapshot_name)
  ))
}

fn harness_binary_path(project_root: &Path) -> PathBuf {
  project_root.join("build/bin/native_test_runner_fixture-tests")
}

fn write_single_test_file(source: &str) -> (TempDir, PathBuf) {
  let temp_dir = TempDir::new().expect("temporary single-file dir");
  let file_path = temp_dir.path().join("sample.ign");

  fs::write(&file_path, source).expect("write single-file source");

  (temp_dir, file_path)
}

fn single_file_harness_binary_path(file_path: &Path) -> PathBuf {
  file_path
    .parent()
    .expect("single-file parent")
    .join("build/bin/sample-tests")
}

fn std_harness_binary_path(output_dir: &Path) -> PathBuf {
  output_dir.join("bin/std-tests")
}

#[test]
fn run_project_tests_returns_ok_when_filtered_tests_pass() {
  let project = write_test_project(
    r#"
import Test from "std::test";

@test
function passes(): void {}

@test
function fails(): void {
    Test::fail();
}
"#,
  );

  let result = run_project_tests(project.path(), Some("passes"), false);

  assert!(result.is_ok(), "expected filtered passing test run to succeed");
  assert!(
    harness_binary_path(project.path()).exists(),
    "expected test harness binary to be built"
  );
}

#[test]
fn run_project_tests_returns_ok_when_std_assertions_pass() {
  let project = write_test_project(
    r#"
import Test from "std::test";

@test
function assertPasses(): void {
    Test::assert(true);
}

@test
function assertEqPasses(): void {
    Test::assertEq(7, 7);
}
"#,
  );

  let result = run_project_tests(project.path(), None, false);

  assert!(result.is_ok(), "expected std assertions to pass");
  assert!(
    harness_binary_path(project.path()).exists(),
    "expected test harness binary to be built"
  );
}

#[test]
fn run_project_tests_returns_ok_when_generic_equality_assertions_pass() {
  let project = write_test_project(
    r#"
import String from "std::string";
import Test from "std::test";

@test
function genericAssertionsPass(): void {
    let equalLeft: String = String::create("shared");
    let equalRight: String = String::create("shared");
    let notEqualLeft: String = String::create("shared");
    let notEqualRight: String = String::create("different");

    Test::assertEq<String>(equalLeft, equalRight);
    Test::assertNe<String>(notEqualLeft, notEqualRight);
    Test::assertEq<str>("abc", "abc");
    Test::assertNe<i32>(1, 2);
}
"#,
  );

  let result = run_project_tests(project.path(), None, false);

  assert!(result.is_ok(), "expected generic equality assertions to pass");
  assert!(
    harness_binary_path(project.path()).exists(),
    "expected test harness binary to be built"
  );
}

#[test]
fn run_project_tests_keeps_top_level_test_discovery_with_directive_functions() {
  let project = write_test_project(
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

  let result = run_project_tests(project.path(), None, false);

  assert!(
    result.is_ok(),
    "expected top-level @test discovery to remain unchanged when directive functions coexist with legacy attrs"
  );
  assert!(
    harness_binary_path(project.path()).exists(),
    "expected test harness binary to be built"
  );
}

#[test]
fn run_single_file_tests_returns_ok_when_generic_equality_assertions_pass() {
  let (_temp_dir, file_path) = write_single_test_file(
    r#"
import String from "std::string";
import Test from "std::test";

@test
function genericAssertionsPass(): void {
    let equalLeft: String = String::create("shared");
    let equalRight: String = String::create("shared");
    let notEqualLeft: String = String::create("shared");
    let notEqualRight: String = String::create("different");

    Test::assertEq<String>(equalLeft, equalRight);
    Test::assertNe<String>(notEqualLeft, notEqualRight);
    Test::assertEq<str>("abc", "abc");
    Test::assertNe<i32>(1, 2);
}
"#,
  );

  let result = run_single_file_tests(&file_path, None, false, Some(&workspace_std_path()));

  assert!(result.is_ok(), "expected single-file generic equality assertions to pass");
  assert!(
    single_file_harness_binary_path(&file_path).exists(),
    "expected single-file harness binary to be built"
  );
}

#[test]
fn run_single_file_tests_keep_top_level_test_discovery_with_directive_functions() {
  let (_temp_dir, file_path) = write_single_test_file(
    r#"
import Compile from "std::compile";
import Test from "std::test";

@directive(target: "record", phase: check, effect: diagnose)
function derive(context: Compile::Context, target: Compile::ItemReference): void {
    return;
}

@deprecated("compat")
function legacyHelper(): void {}

@test
function smoke(): void {
    legacyHelper();
    Test::assert(true);
}
"#,
  );

  let result = run_single_file_tests(&file_path, None, false, Some(&workspace_std_path()));

  assert!(
    result.is_ok(),
    "expected single-file top-level @test discovery to remain unchanged when directive functions coexist with legacy attrs"
  );
  assert!(
    single_file_harness_binary_path(&file_path).exists(),
    "expected single-file harness binary to be built"
  );
}

#[test]
fn run_single_file_tests_filters_by_case_sensitive_substring() {
  let (_temp_dir, file_path) = write_single_test_file(
    r#"
import Test from "std::test";

@test
function passes(): void {}

@test
function Fails(): void {
    Test::fail();
}
"#,
  );

  let result = run_single_file_tests(&file_path, Some("passes"), false, Some(&workspace_std_path()));

  assert!(
    result.is_ok(),
    "expected case-sensitive single-file filter to select only the passing test"
  );
}

#[test]
fn run_std_tests_returns_ok_when_filter_matches_no_tests() {
  let output_dir = TempDir::new().expect("temporary std output dir");

  let result = run_std_tests(
    &workspace_std_path(),
    Some("missing-std-test-filter"),
    false,
    Some(output_dir.path()),
  );

  assert!(result.is_ok(), "expected empty std selection to succeed");
  assert!(
    !std_harness_binary_path(output_dir.path()).exists(),
    "expected no std harness binary when no tests are selected"
  );
}

#[test]
fn run_std_tests_executes_workspace_std_test_and_builds_harness() {
  let output_dir = TempDir::new().expect("temporary std output dir");

  let result = run_std_tests(
    &workspace_std_path(),
    Some("vector::tests::clearDropsStringElementsBeforeReuse"),
    false,
    Some(output_dir.path()),
  );

  assert!(result.is_ok(), "expected std test run to succeed");
  assert!(
    std_harness_binary_path(output_dir.path()).exists(),
    "expected std test run to build the harness binary"
  );
}

#[test]
fn run_std_tests_executes_vector_drop_exact_once_slice() {
  let output_dir = TempDir::new().expect("temporary std output dir");

  let result = run_std_tests(
    &workspace_std_path(),
    Some("vector::tests::clearAndDropRunElementDropExactlyOnce"),
    false,
    Some(output_dir.path()),
  );

  assert!(result.is_ok(), "expected vector drop exact-once std slice to succeed");
}

#[test]
fn run_std_tests_executes_hash_set_zero_sized_marker_slice() {
  let output_dir = TempDir::new().expect("temporary std output dir");

  let result = run_std_tests(
    &workspace_std_path(),
    Some("collections::tests::hashSetUsesZeroSizedMarkerPayload"),
    false,
    Some(output_dir.path()),
  );

  assert!(result.is_ok(), "expected hash set zero-sized marker std slice to succeed");
}

#[test]
fn run_std_tests_executes_hash_map_drop_exact_once_slice() {
  let output_dir = TempDir::new().expect("temporary std output dir");

  let result = run_std_tests(
    &workspace_std_path(),
    Some("collections::tests::hashMapReplacementRemoveAndDropAreExactOnce"),
    false,
    Some(output_dir.path()),
  );

  assert!(result.is_ok(), "expected hash map drop exact-once std slice to succeed");
}

#[test]
fn run_std_tests_executes_hash_set_drop_exact_once_slice() {
  let output_dir = TempDir::new().expect("temporary std output dir");

  let result = run_std_tests(
    &workspace_std_path(),
    Some("collections::tests::hashSetMembershipRemoveClearAndDropAreExactOnce"),
    false,
    Some(output_dir.path()),
  );

  assert!(result.is_ok(), "expected hash set drop exact-once std slice to succeed");
}

#[test]
fn run_std_tests_executes_fs_interior_nul_slice() {
  let output_dir = TempDir::new().expect("temporary std output dir");

  let result = run_std_tests(
    &workspace_std_path(),
    Some("fs::tests::writeStringAndReadToStringPreserveInteriorNulBytes"),
    false,
    Some(output_dir.path()),
  );

  assert!(result.is_ok(), "expected fs interior-nul std slice to succeed");
}

#[test]
fn run_std_tests_executes_string_snapshot_slice() {
  let output_dir = TempDir::new().expect("temporary std output dir");

  let result = run_std_tests(
    &workspace_std_path(),
    Some("string::tests::snapshotStdRunnerSmoke"),
    false,
    Some(output_dir.path()),
  );

  assert!(result.is_ok(), "expected string snapshot std slice to succeed");
}

#[test]
fn run_std_tests_executes_full_workspace_std_suite() {
  let output_dir = TempDir::new().expect("temporary std output dir");

  let result = run_std_tests(&workspace_std_path(), None, false, Some(output_dir.path()));

  assert!(result.is_ok(), "expected the full std suite to succeed");
}

#[test]
fn run_project_tests_returns_err_when_any_selected_test_fails() {
  let project = write_test_project(
    r#"
import Test from "std::test";

@test
function passes(): void {}

@test
function fails(): void {
    Test::fail();
}

@test
function laterPass(): void {}
"#,
  );

  let result = run_project_tests(project.path(), None, false);

  assert!(result.is_err(), "expected mixed pass/fail test run to return an error");
  assert!(
    harness_binary_path(project.path()).exists(),
    "expected test harness binary to be built"
  );
}

#[test]
fn run_project_tests_returns_ok_when_filter_matches_no_tests() {
  let project = write_test_project(
    r#"
@test
function passes(): void {}
"#,
  );

  let result = run_project_tests(project.path(), Some("missing"), false);

  assert!(result.is_ok(), "expected empty selection to succeed");
  assert!(
    !harness_binary_path(project.path()).exists(),
    "expected no harness binary when no tests are selected"
  );
}

#[test]
fn run_project_tests_returns_err_when_test_setup_analysis_fails() {
  let project = write_test_project(
    r#"
@test
function invalid(value: i32): void {}
"#,
  );

  let result = run_project_tests(project.path(), None, false);

  assert!(result.is_err(), "expected invalid test shape to fail setup");
  assert!(
    !harness_binary_path(project.path()).exists(),
    "expected no harness binary when setup fails before codegen"
  );
}

#[test]
fn run_project_tests_returns_err_when_assert_eq_uses_unsupported_equality() {
  let project = write_test_project(
    r#"
import Test from "std::test";

record Pair {
    public value: i32;
}

@test
function invalidEq(): void {
    let left: Pair = Pair { value: 1 };
    let right: Pair = Pair { value: 1 };
    Test::assertEq(left, right);
}
"#,
  );

  let result = run_project_tests(project.path(), None, false);

  assert!(result.is_err(), "expected unsupported equality overload to fail test setup");
  assert!(
    !harness_binary_path(project.path()).exists(),
    "expected no harness binary when setup fails before codegen"
  );
}

#[test]
fn run_project_tests_returns_err_when_builtin_eq_uses_unsupported_record_directly() {
  let project = write_test_project(
    r#"
import Test from "std::test";

record Wrapper {
    public value: i32;
}

@test
function invalidEq(): void {
    let left: Wrapper = Wrapper { value: 7 };
    let right: Wrapper = Wrapper { value: 7 };
    Test::assert(@eq<Wrapper>(&left, &right));
}
"#,
  );

  let result = run_project_tests(project.path(), None, false);

  assert!(
    result.is_err(),
    "expected unsupported builtin equality to fail before harness build"
  );
  assert!(
    !harness_binary_path(project.path()).exists(),
    "expected no harness binary when builtin equality is rejected during setup"
  );
}

#[test]
fn run_project_tests_allows_matching_snapshot_without_update_mode() {
  let project = write_test_project(
    r#"
import Test from "std::test";

@test
function matchesSnapshot(): void {
    Test::assertSnapshot("rendered", "hello snapshot\n");
}
"#,
  );

  let snapshot_path = snapshot_file_path(project.path(), "main::matchesSnapshot", "rendered");
  fs::create_dir_all(snapshot_path.parent().expect("snapshot dir")).expect("create snapshot dir");
  fs::write(&snapshot_path, "hello snapshot\n").expect("write snapshot file");

  let result = run_project_tests(project.path(), None, false);

  assert!(result.is_ok(), "expected matching snapshot to pass without update mode");
}

#[test]
fn run_project_tests_missing_snapshot_fails_without_update_mode() {
  let project = write_test_project(
    r#"
import Test from "std::test";

@test
function missingSnapshot(): void {
    Test::assertSnapshot("rendered", "hello snapshot\n");
}
"#,
  );

  let snapshot_path = snapshot_file_path(project.path(), "main::missingSnapshot", "rendered");

  let result = run_project_tests(project.path(), None, false);

  assert!(result.is_err(), "expected missing snapshot to fail without update mode");
  assert!(
    !snapshot_path.exists(),
    "expected missing snapshot run to avoid creating a baseline"
  );
}

#[test]
fn run_project_tests_mismatched_snapshot_fails_without_update_mode() {
  let project = write_test_project(
    r#"
import Test from "std::test";

@test
function mismatchedSnapshot(): void {
    Test::assertSnapshot("rendered", "new contents\n");
}
"#,
  );

  let snapshot_path = snapshot_file_path(project.path(), "main::mismatchedSnapshot", "rendered");
  fs::create_dir_all(snapshot_path.parent().expect("snapshot dir")).expect("create snapshot dir");
  fs::write(&snapshot_path, "old contents\n").expect("write snapshot file");

  let result = run_project_tests(project.path(), None, false);

  assert!(result.is_err(), "expected mismatched snapshot to fail without update mode");
  assert_eq!(
    fs::read_to_string(&snapshot_path).expect("read snapshot file"),
    "old contents\n"
  );
}

#[test]
fn run_project_tests_creates_snapshot_in_update_mode() {
  let project = write_test_project(
    r#"
import Test from "std::test";

@test
function writesSnapshot(): void {
    Test::assertSnapshot("rendered", "hello snapshot\n");
}
"#,
  );

  let snapshot_path = snapshot_file_path(project.path(), "main::writesSnapshot", "rendered");

  let result = run_project_tests(project.path(), None, true);

  assert!(result.is_ok(), "expected update mode to create a missing snapshot");
  assert_eq!(
    fs::read_to_string(&snapshot_path).expect("read snapshot file"),
    "hello snapshot\n"
  );
}

#[test]
fn run_single_file_tests_creates_snapshot_next_to_source_file() {
  let (_temp_dir, file_path) = write_single_test_file(
    r#"
import Test from "std::test";

@test
function writesSnapshot(): void {
    Test::assertSnapshot("rendered", "hello snapshot\n");
}
"#,
  );

  let snapshot_path = file_path
    .parent()
    .expect("single-file parent")
    .join("__snapshots__")
    .join(format!(
      "{}__{}.snap.txt",
      escape_snapshot_component("sample::writesSnapshot"),
      escape_snapshot_component("rendered")
    ));

  let result = run_single_file_tests(&file_path, None, true, Some(&workspace_std_path()));

  assert!(
    result.is_ok(),
    "expected single-file update mode to create the snapshot next to the source file"
  );
  assert_eq!(
    fs::read_to_string(&snapshot_path).expect("read single-file snapshot"),
    "hello snapshot\n"
  );
}

#[test]
fn run_project_tests_replaces_snapshot_in_update_mode() {
  let project = write_test_project(
    r#"
import Test from "std::test";

@test
function replacesSnapshot(): void {
    Test::assertSnapshot("rendered", "new contents\n");
}
"#,
  );

  let snapshot_path = snapshot_file_path(project.path(), "main::replacesSnapshot", "rendered");
  fs::create_dir_all(snapshot_path.parent().expect("snapshot dir")).expect("create snapshot dir");
  fs::write(&snapshot_path, "old contents\n").expect("write snapshot file");

  let result = run_project_tests(project.path(), None, true);

  assert!(result.is_ok(), "expected update mode to replace a mismatched snapshot");
  assert_eq!(
    fs::read_to_string(&snapshot_path).expect("read snapshot file"),
    "new contents\n"
  );
}

#[test]
fn run_project_tests_snapshots_utf8_file_contents() {
  let project = write_test_project(
    r#"
import Test from "std::test";

@test
function snapshotsFile(): void {
    Test::assertFileSnapshot("artifact", "FILE_PLACEHOLDER");
}
"#,
  );

  let output_path = project.path().join("fixture-output.txt");
  fs::write(&output_path, "file contents\n").expect("write fixture output");

  let source_path = project.path().join("src/main.ign");
  let source = fs::read_to_string(&source_path).expect("read source");
  fs::write(
    &source_path,
    source.replace("FILE_PLACEHOLDER", output_path.to_string_lossy().as_ref()),
  )
  .expect("rewrite source");

  let snapshot_path = snapshot_file_path(project.path(), "main::snapshotsFile", "artifact");

  let result = run_project_tests(project.path(), None, true);

  assert!(
    result.is_ok(),
    "expected file snapshot helper to write file contents in update mode"
  );
  assert_eq!(
    fs::read_to_string(&snapshot_path).expect("read snapshot file"),
    "file contents\n"
  );
}

#[test]
fn run_project_tests_keeps_same_snapshot_name_distinct_across_modules() {
  let project = write_test_project(
    r#"
import MARKER from "./math";
import Test from "std::test";

@test
function rootSnapshot(): void {
    if (MARKER == 0) {
        Test::fail();
    }

    Test::assertSnapshot("shared", "root contents\n");
}
"#,
  );

  write_project_module(
    project.path(),
    "math.ign",
    r#"
import Test from "std::test";

export const MARKER: i32 = 1;

@test
function moduleSnapshot(): void {
    Test::assertSnapshot("shared", "module contents\n");
}
"#,
  );

  let root_snapshot = snapshot_file_path(project.path(), "main::rootSnapshot", "shared");
  let module_snapshot = snapshot_file_path(project.path(), "math::moduleSnapshot", "shared");

  let result = run_project_tests(project.path(), None, true);

  assert!(result.is_ok(), "expected update mode to create both module snapshots");
  assert_eq!(
    fs::read_to_string(&root_snapshot).expect("read root snapshot"),
    "root contents\n"
  );
  assert_eq!(
    fs::read_to_string(&module_snapshot).expect("read module snapshot"),
    "module contents\n"
  );
  assert_ne!(
    root_snapshot, module_snapshot,
    "expected module snapshots to use distinct filenames"
  );
}
