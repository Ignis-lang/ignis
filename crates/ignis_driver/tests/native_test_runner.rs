mod common;

use std::fs;
use std::path::{Path, PathBuf};
use std::time::{Duration, Instant};

use ignis_driver::{
  run_project_tests, run_project_tests_with_options, run_single_file_tests, run_std_tests, TestRunOptions,
};
use tempfile::TempDir;

fn workspace_std_path() -> PathBuf {
  PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../std")
}

fn write_test_project(source: &str) -> TempDir {
  write_test_project_with_fixture_dirs(source, &[])
}

fn write_test_project_with_fixture_dirs(
  source: &str,
  fixture_dirs: &[&str],
) -> TempDir {
  let temp_dir = TempDir::new().expect("temporary project dir");
  let src_dir = temp_dir.path().join("src");

  fs::create_dir_all(&src_dir).expect("create src dir");
  fs::write(src_dir.join("main.ign"), source).expect("write main module");

  let fixture_section = if fixture_dirs.is_empty() {
    String::new()
  } else {
    let entries: Vec<String> = fixture_dirs.iter().map(|dir| format!("\"{}\"", dir)).collect();
    format!("\n[test]\nfixtures = [{}]\n", entries.join(", "))
  };

  fs::write(
    temp_dir.path().join("ignis.toml"),
    format!(
      "[package]\nname = \"native_test_runner_fixture\"\nversion = \"0.1.0\"\nauthors = []\ndescription = \"fixture\"\nkeywords = []\nlicense = \"MIT\"\nrepository = \"\"\n\n[ignis]\nstd = true\nstd_path = \"{}\"\n\n[build]\nbin = true\nsource_dir = \"src\"\nentry = \"main.ign\"\nout_dir = \"build\"\nopt_level = 0\ndebug = false\ntarget = \"c\"\ncc = \"cc\"\ncflags = []\nemit = []\n{}",
      workspace_std_path().display(),
      fixture_section
    ),
  )
  .expect("write ignis.toml");

  temp_dir
}

/// The `@test`-free project body used by fixture-only runs.
const NO_TESTS_MAIN: &str = "function main(): void {}\n";

/// A program that allocates through libc and never frees, so LSan flags it.
const LEAKING_FIXTURE_SOURCE: &str = concat!(
  "import LibC, CType from \"std::libc\";\n",
  "\n",
  "function main(): i32 {\n",
  "  let leaked: CType::CVoidPtr = LibC::Allocator::malloc(64);\n",
  "  if (leaked == null) {\n",
  "    return 1;\n",
  "  }\n",
  "\n",
  "  return 0;\n",
  "}\n",
);

fn write_fixture_file(
  project_root: &Path,
  relative_path: &str,
  source: &str,
) -> PathBuf {
  let path = project_root.join(relative_path);
  fs::create_dir_all(path.parent().expect("fixture parent")).expect("create fixture dir");
  fs::write(&path, source).expect("write fixture source");
  path
}

fn fixture_snapshot_path(
  project_root: &Path,
  relative_path: &str,
) -> PathBuf {
  let fixture_path = project_root.join(relative_path);
  let directory = fixture_path.parent().expect("fixture parent");
  let stem = fixture_path.file_stem().expect("fixture stem");

  directory.join("__snapshots__").join(stem).with_extension("snap")
}

fn write_fixture_snapshot(
  project_root: &Path,
  relative_path: &str,
  body: &str,
) -> PathBuf {
  let snapshot_path = fixture_snapshot_path(project_root, relative_path);
  fs::create_dir_all(snapshot_path.parent().expect("snapshot dir")).expect("create snapshot dir");
  fs::write(&snapshot_path, body).expect("write fixture snapshot");
  snapshot_path
}

fn fixture_options() -> TestRunOptions {
  TestRunOptions::default()
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
fn run_project_tests_links_std_generic_specializations_used_by_tests() {
  let project = write_test_project(
    r#"
import String from "std::string";
import Test from "std::test";
import Vector from "std::vector";

record DiagnosticLabel {
    public message: str;
}

@test
function stdGenericSpecializationsLink(): void {
    let mut labels: Vector<DiagnosticLabel> = Vector::new<DiagnosticLabel>();
    labels.push(DiagnosticLabel { message: "label" });

    Test::assertEq<u64>(labels.length(), 1);

    let source: String = String::create("x");
    let found: char = match (source.charAt(0)) {
        Option::SOME(value) -> value,
        Option::NONE -> @panic("missing char"),
    };
    let fallback: char = source.charAt(9).unwrapOr('?');

    Test::assertEq<char>(found, 'x');
    Test::assertEq<char>(fallback, '?');
}
"#,
  );

  let result = run_project_tests(project.path(), None, false);

  assert!(result.is_ok(), "expected test-used std generic specializations to link");
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
fn run_project_tests_discovers_recursive_project_test_files_without_main_imports() {
  let project = write_test_project(
    r#"
function main(): i32 {
    return 0;
}
"#,
  );

  write_project_module(
    project.path(),
    "nested/helpers_test.ign",
    r#"
import Test from "std::test";

@test
function helperSmoke(): void {
    Test::assert(true);
}
"#,
  );
  write_project_module(
    project.path(),
    "nested/deeper/tests.ign",
    r#"
import Test from "std::test";

@test
function deeperSmoke(): void {
    Test::assert(true);
}
"#,
  );

  let result = run_project_tests(project.path(), None, false);

  assert!(
    result.is_ok(),
    "expected recursive autodiscovery to compile and run nested test files"
  );
  assert!(
    harness_binary_path(project.path()).exists(),
    "expected test harness binary to be built"
  );
}

/// A filtered run must emit every project module, not just the import closure of the
/// selected tests. `b.ign` calls `C::helper` without importing `c.ign`, so the import
/// graph alone would leave `c.o` out of the link and the harness would fail to resolve
/// `C_helper`.
#[test]
fn run_project_tests_links_modules_reached_outside_the_import_graph_of_the_filter() {
  let project = write_test_project(
    r#"
import C from "./c";
import A from "./a";

function main(): i32 {
    return A::callsB() - C::helper();
}
"#,
  );

  write_project_module(
    project.path(),
    "c.ign",
    r#"
export namespace C {
  function helper(): i32 {
    return 7;
  }
}
"#,
  );
  write_project_module(
    project.path(),
    "b.ign",
    r#"
export namespace B {
  function callsC(): i32 {
    return C::helper();
  }
}
"#,
  );
  write_project_module(
    project.path(),
    "a.ign",
    r#"
import B from "./b";

export namespace A {
  function callsB(): i32 {
    return B::callsC();
  }
}
"#,
  );
  write_project_module(
    project.path(),
    "a_tests.ign",
    r#"
import Test from "std::test";
import A from "./a";

@test
function aReachesC(): void {
    Test::assert(A::callsB() == 7);
}
"#,
  );

  let result = run_project_tests(project.path(), Some("aReachesC"), false);

  assert!(result.is_ok(), "expected the filtered test run to link and pass");
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
fn run_std_tests_executes_fs_read_dir_non_empty_slice() {
  let output_dir = TempDir::new().expect("temporary std output dir");

  let result = run_std_tests(
    &workspace_std_path(),
    Some("fs::tests::readDirNextReturnsOkSomeAndThenOkNone"),
    false,
    Some(output_dir.path()),
  );

  assert!(result.is_ok(), "expected fs read-dir non-empty std slice to succeed");
}

#[test]
fn run_std_tests_executes_fs_read_dir_empty_slice() {
  let output_dir = TempDir::new().expect("temporary std output dir");

  let result = run_std_tests(
    &workspace_std_path(),
    Some("fs::tests::readDirNextReturnsOkNoneForExhaustedEmptyDirectory"),
    false,
    Some(output_dir.path()),
  );

  assert!(result.is_ok(), "expected fs read-dir empty std slice to succeed");
}

#[test]
fn run_std_tests_executes_fs_read_dir_invalidated_slice() {
  let output_dir = TempDir::new().expect("temporary std output dir");

  let result = run_std_tests(
    &workspace_std_path(),
    Some("fs::tests::readDirNextReturnsErrorAfterDirectoryStreamIsInvalidated"),
    false,
    Some(output_dir.path()),
  );

  assert!(result.is_ok(), "expected fs read-dir invalidated std slice to succeed");
}

#[test]
fn run_std_tests_executes_fs_bytes_roundtrip_slice() {
  let output_dir = TempDir::new().expect("temporary std output dir");

  let result = run_std_tests(
    &workspace_std_path(),
    Some("fs::tests::writeBytesAndReadToBytesPreserveEmptyAndBinaryPayloads"),
    false,
    Some(output_dir.path()),
  );

  assert!(result.is_ok(), "expected fs byte roundtrip std slice to succeed");
}

#[test]
fn run_std_tests_executes_fs_canonicalize_slice() {
  let output_dir = TempDir::new().expect("temporary std output dir");

  let result = run_std_tests(
    &workspace_std_path(),
    Some("fs::tests::canonicalizeRequiresExistingPathAndResolvesRealLocation"),
    false,
    Some(output_dir.path()),
  );

  assert!(result.is_ok(), "expected fs canonicalize std slice to succeed");
}

#[test]
fn run_std_tests_executes_fs_temp_dir_slice() {
  let output_dir = TempDir::new().expect("temporary std output dir");

  let result = run_std_tests(
    &workspace_std_path(),
    Some("fs::tests::tempDirReturnsExistingDirectoryRoot"),
    false,
    Some(output_dir.path()),
  );

  assert!(result.is_ok(), "expected fs temp-dir std slice to succeed");
}

#[test]
fn run_std_tests_executes_fs_walk_symlink_leaf_slice() {
  let output_dir = TempDir::new().expect("temporary std output dir");

  let result = run_std_tests(
    &workspace_std_path(),
    Some("fs::tests::walkTreatsSymlinkDirectoryAsLeaf"),
    false,
    Some(output_dir.path()),
  );

  assert!(result.is_ok(), "expected fs walk symlink-leaf std slice to succeed");
}

#[test]
fn run_std_tests_executes_fs_remove_dir_all_slice() {
  let output_dir = TempDir::new().expect("temporary std output dir");

  let result = run_std_tests(
    &workspace_std_path(),
    Some("fs::tests::removeDirAllFailsWhenMissingAndRemovesNestedTreesWithoutFollowingSymlinks"),
    false,
    Some(output_dir.path()),
  );

  assert!(result.is_ok(), "expected fs removeDirAll std slice to succeed");
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
fn run_std_tests_executes_string_utf8_char_slice() {
  let output_dir = TempDir::new().expect("temporary std output dir");

  let result = run_std_tests(
    &workspace_std_path(),
    Some("string::tests::charAtDecodesUtf8ScalarAtByteBoundary"),
    false,
    Some(output_dir.path()),
  );

  assert!(result.is_ok(), "expected std string utf8 charAt slice to succeed");
}

#[test]
fn run_std_tests_executes_string_utf8_push_slice() {
  let output_dir = TempDir::new().expect("temporary std output dir");

  let result = run_std_tests(
    &workspace_std_path(),
    Some("string::tests::pushCharEncodesScalarUtf8BytesAndPushByteStaysByteOriented"),
    false,
    Some(output_dir.path()),
  );

  assert!(result.is_ok(), "expected std string utf8 push slice to succeed");
}

#[test]
fn run_std_tests_executes_path_normalize_slice() {
  let output_dir = TempDir::new().expect("temporary std output dir");

  let result = run_std_tests(
    &workspace_std_path(),
    Some("path::tests::normalizeCollapsesDotDotAndRepeatedSeparators"),
    false,
    Some(output_dir.path()),
  );

  assert!(result.is_ok(), "expected path normalize std slice to succeed");
}

#[test]
fn run_std_tests_executes_path_is_relative_slice() {
  let output_dir = TempDir::new().expect("temporary std output dir");

  let result = run_std_tests(
    &workspace_std_path(),
    Some("path::tests::isRelativeNegatesIsAbsolute"),
    false,
    Some(output_dir.path()),
  );

  assert!(result.is_ok(), "expected path isRelative std slice to succeed");
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

#[test]
fn fixture_program_passes_against_a_matching_snapshot() {
  let project = write_test_project_with_fixture_dirs(NO_TESTS_MAIN, &["corpus/ok"]);

  write_fixture_file(
    project.path(),
    "corpus/ok/returns_42.ign",
    "function main(): i32 {\n  return 42;\n}\n",
  );
  write_fixture_snapshot(
    project.path(),
    "corpus/ok/returns_42.ign",
    "exit_code: 42\nstdout: (empty)\nstderr: (empty)",
  );

  let result = run_project_tests_with_options(project.path(), &fixture_options());

  assert!(result.is_ok(), "expected the matching fixture snapshot to pass");
}

#[test]
fn fixture_program_fails_against_a_mismatched_snapshot() {
  let project = write_test_project_with_fixture_dirs(NO_TESTS_MAIN, &["corpus/ok"]);

  write_fixture_file(
    project.path(),
    "corpus/ok/returns_42.ign",
    "function main(): i32 {\n  return 42;\n}\n",
  );
  let snapshot_path = write_fixture_snapshot(
    project.path(),
    "corpus/ok/returns_42.ign",
    "exit_code: 7\nstdout: (empty)\nstderr: (empty)",
  );

  let result = run_project_tests_with_options(project.path(), &fixture_options());

  assert!(result.is_err(), "expected a mismatched fixture snapshot to fail");
  assert_eq!(
    fs::read_to_string(&snapshot_path).expect("read fixture snapshot"),
    "exit_code: 7\nstdout: (empty)\nstderr: (empty)",
    "expected a failing run to leave the baseline untouched"
  );
}

#[test]
fn fixture_program_fails_when_its_snapshot_is_missing() {
  let project = write_test_project_with_fixture_dirs(NO_TESTS_MAIN, &["corpus/ok"]);

  write_fixture_file(
    project.path(),
    "corpus/ok/returns_42.ign",
    "function main(): i32 {\n  return 42;\n}\n",
  );
  let snapshot_path = fixture_snapshot_path(project.path(), "corpus/ok/returns_42.ign");

  let result = run_project_tests_with_options(project.path(), &fixture_options());

  assert!(result.is_err(), "expected a missing fixture snapshot to fail");
  assert!(!snapshot_path.exists(), "expected a failing run to avoid creating a baseline");
}

#[test]
fn fixture_program_snapshot_is_written_in_update_mode() {
  let project = write_test_project_with_fixture_dirs(NO_TESTS_MAIN, &["corpus/ok"]);

  write_fixture_file(
    project.path(),
    "corpus/ok/prints_and_exits.ign",
    "import Io from \"std::io\";\n\nfunction main(): i32 {\n  Io::println(\"hello\");\n  return 3;\n}\n",
  );
  let snapshot_path = fixture_snapshot_path(project.path(), "corpus/ok/prints_and_exits.ign");

  let options = TestRunOptions {
    update_snapshots: true,
    ..TestRunOptions::default()
  };
  let result = run_project_tests_with_options(project.path(), &options);

  assert!(result.is_ok(), "expected update mode to create the fixture baseline");
  assert_eq!(
    fs::read_to_string(&snapshot_path).expect("read fixture snapshot"),
    "exit_code: 3\nstdout: hello\nstderr: (empty)"
  );
}

#[test]
fn fixture_err_mode_snapshots_the_reported_diagnostics() {
  let project = write_test_project_with_fixture_dirs(NO_TESTS_MAIN, &["corpus/err"]);

  write_fixture_file(
    project.path(),
    "corpus/err/undefined_name.ign",
    "// e2e: err\nfunction main(): i32 {\n  return missingValue;\n}\n",
  );
  let snapshot_path = fixture_snapshot_path(project.path(), "corpus/err/undefined_name.ign");

  let options = TestRunOptions {
    update_snapshots: true,
    ..TestRunOptions::default()
  };

  assert!(
    run_project_tests_with_options(project.path(), &options).is_ok(),
    "expected update mode to record the diagnostics baseline"
  );

  let baseline = fs::read_to_string(&snapshot_path).expect("read diagnostics snapshot");
  assert!(
    !baseline.trim().is_empty(),
    "expected the diagnostics baseline to hold at least one error"
  );

  assert!(
    run_project_tests_with_options(project.path(), &fixture_options()).is_ok(),
    "expected the recorded diagnostics to compare equal on a second run"
  );
}

#[test]
fn fixture_err_mode_reports_a_parse_failure() {
  let project = write_test_project_with_fixture_dirs(NO_TESTS_MAIN, &["corpus/err"]);

  write_fixture_file(
    project.path(),
    "corpus/err/missing_semicolon.ign",
    "// e2e: err\nfunction main(): i32 {\n  return 42\n}\n",
  );
  let snapshot_path = fixture_snapshot_path(project.path(), "corpus/err/missing_semicolon.ign");

  let options = TestRunOptions {
    update_snapshots: true,
    ..TestRunOptions::default()
  };

  assert!(
    run_project_tests_with_options(project.path(), &options).is_ok(),
    "expected a parse failure to record a diagnostics baseline"
  );
  assert!(
    !fs::read_to_string(&snapshot_path)
      .expect("read diagnostics snapshot")
      .trim()
      .is_empty(),
    "expected the parse failure to produce at least one diagnostic"
  );
}

#[test]
fn fixture_err_mode_fails_when_the_program_compiles_cleanly() {
  let project = write_test_project_with_fixture_dirs(NO_TESTS_MAIN, &["corpus/err"]);

  write_fixture_file(
    project.path(),
    "corpus/err/compiles_fine.ign",
    "// e2e: err\nfunction main(): i32 {\n  return 0;\n}\n",
  );

  let options = TestRunOptions {
    update_snapshots: true,
    ..TestRunOptions::default()
  };
  let result = run_project_tests_with_options(project.path(), &options);

  assert!(result.is_err(), "expected an err fixture that compiles to fail");
}

#[test]
fn fixture_std_header_compiles_against_the_standard_library() {
  let project = write_test_project_with_fixture_dirs(NO_TESTS_MAIN, &["corpus/ok"]);

  write_fixture_file(
    project.path(),
    "corpus/ok/uses_std_string.ign",
    "// e2e: std\nimport String from \"std::string\";\nimport Io from \"std::io\";\n\nfunction main(): i32 {\n  let greeting: String = String::create(\"hi\");\n  Io::println(greeting.toStr());\n  return 0;\n}\n",
  );
  write_fixture_snapshot(
    project.path(),
    "corpus/ok/uses_std_string.ign",
    "exit_code: 0\nstdout: hi\nstderr: (empty)",
  );

  let result = run_project_tests_with_options(project.path(), &fixture_options());

  assert!(result.is_ok(), "expected the std fixture to compile, run and match");
}

#[test]
fn a_leaking_fixture_is_reported_as_a_failure() {
  let project = write_test_project_with_fixture_dirs(NO_TESTS_MAIN, &["corpus/ok"]);

  write_fixture_file(project.path(), "corpus/ok/leaks.ign", LEAKING_FIXTURE_SOURCE);
  write_fixture_snapshot(
    project.path(),
    "corpus/ok/leaks.ign",
    "exit_code: 0\nstdout: (empty)\nstderr: (empty)",
  );

  let result = run_project_tests_with_options(project.path(), &fixture_options());

  assert!(result.is_err(), "expected a leaking fixture to fail the run");
}

#[test]
fn the_allow_leak_header_keeps_a_leaking_fixture_passing() {
  let project = write_test_project_with_fixture_dirs(NO_TESTS_MAIN, &["corpus/ok"]);

  write_fixture_file(
    project.path(),
    "corpus/ok/leaks_on_purpose.ign",
    &format!("// e2e: allow-leak\n{}", LEAKING_FIXTURE_SOURCE),
  );
  write_fixture_snapshot(
    project.path(),
    "corpus/ok/leaks_on_purpose.ign",
    "exit_code: 0\nstdout: (empty)\nstderr: (empty)",
  );

  let result = run_project_tests_with_options(project.path(), &fixture_options());

  assert!(result.is_ok(), "expected an allow-leak fixture to pass despite leaking");
}

#[test]
fn a_filter_selects_fixtures_by_their_plan_entry_name() {
  let project = write_test_project_with_fixture_dirs(
    r#"
import Test from "std::test";

@test
function alwaysFails(): void {
    Test::fail();
}
"#,
    &["corpus/ok"],
  );

  write_fixture_file(
    project.path(),
    "corpus/ok/returns_42.ign",
    "function main(): i32 {\n  return 42;\n}\n",
  );
  write_fixture_snapshot(
    project.path(),
    "corpus/ok/returns_42.ign",
    "exit_code: 42\nstdout: (empty)\nstderr: (empty)",
  );

  let options = TestRunOptions {
    filter: Some("e2e::".to_string()),
    ..TestRunOptions::default()
  };
  let result = run_project_tests_with_options(project.path(), &options);

  assert!(result.is_ok(), "expected the e2e:: filter to select only the passing fixture");
}

#[test]
fn a_project_without_fixtures_runs_exactly_as_before() {
  let project = write_test_project(
    r#"
import Test from "std::test";

@test
function passes(): void {}
"#,
  );

  let result = run_project_tests_with_options(project.path(), &fixture_options());

  assert!(result.is_ok(), "expected a fixture-free project to keep passing");
  assert!(
    !project.path().join("build/fixtures").exists(),
    "expected no fixture work directories for a project without fixtures"
  );
}

#[test]
fn a_partition_runs_only_the_entries_it_owns() {
  let project = write_test_project_with_fixture_dirs(NO_TESTS_MAIN, &["corpus/ok"]);

  write_fixture_file(
    project.path(),
    "corpus/ok/a_passes.ign",
    "function main(): i32 {\n  return 0;\n}\n",
  );
  write_fixture_snapshot(
    project.path(),
    "corpus/ok/a_passes.ign",
    "exit_code: 0\nstdout: (empty)\nstderr: (empty)",
  );

  write_fixture_file(
    project.path(),
    "corpus/ok/b_fails.ign",
    "function main(): i32 {\n  return 1;\n}\n",
  );
  write_fixture_snapshot(
    project.path(),
    "corpus/ok/b_fails.ign",
    "exit_code: 0\nstdout: (empty)\nstderr: (empty)",
  );

  let first_shard = TestRunOptions {
    partition: Some((1, 2)),
    ..TestRunOptions::default()
  };
  let second_shard = TestRunOptions {
    partition: Some((2, 2)),
    ..TestRunOptions::default()
  };

  assert!(
    run_project_tests_with_options(project.path(), &first_shard).is_ok(),
    "expected shard 1/2 to hold only the passing fixture"
  );
  assert!(
    run_project_tests_with_options(project.path(), &second_shard).is_err(),
    "expected shard 2/2 to hold only the failing fixture"
  );
}

#[test]
fn an_invalid_partition_is_rejected_before_anything_runs() {
  let project = write_test_project_with_fixture_dirs(NO_TESTS_MAIN, &["corpus/ok"]);

  let options = TestRunOptions {
    partition: Some((3, 2)),
    ..TestRunOptions::default()
  };

  assert!(
    run_project_tests_with_options(project.path(), &options).is_err(),
    "expected an out-of-range partition to be rejected"
  );
}

#[test]
fn a_test_exceeding_the_timeout_is_killed_and_reported_as_a_failure() {
  let project = write_test_project(
    r#"
import LibC from "std::libc";

@test
function hangs(): void {
    LibC::Process::sleep(600);
}
"#,
  );

  let options = TestRunOptions {
    timeout: Some(Duration::from_millis(500)),
    ..TestRunOptions::default()
  };

  let started = Instant::now();
  let result = run_project_tests_with_options(project.path(), &options);

  assert!(result.is_err(), "expected a hanging test to fail the run");
  assert!(
    started.elapsed() < Duration::from_secs(300),
    "expected the runner to kill the hanging test instead of waiting for it"
  );
}

#[test]
fn a_fixture_exceeding_the_timeout_is_killed_and_reported_as_a_failure() {
  let project = write_test_project_with_fixture_dirs(NO_TESTS_MAIN, &["corpus/ok"]);

  write_fixture_file(
    project.path(),
    "corpus/ok/hangs.ign",
    "import LibC from \"std::libc\";\n\nfunction main(): i32 {\n  LibC::Process::sleep(600);\n  return 0;\n}\n",
  );
  write_fixture_snapshot(
    project.path(),
    "corpus/ok/hangs.ign",
    "exit_code: 0\nstdout: (empty)\nstderr: (empty)",
  );

  let options = TestRunOptions {
    timeout: Some(Duration::from_millis(500)),
    ..TestRunOptions::default()
  };

  let started = Instant::now();
  let result = run_project_tests_with_options(project.path(), &options);

  assert!(result.is_err(), "expected a hanging fixture to fail the run");
  assert!(
    started.elapsed() < Duration::from_secs(300),
    "expected the runner to kill the hanging fixture instead of waiting for it"
  );
}

#[test]
fn a_cli_fixture_directory_is_added_to_the_project_corpus() {
  let project = write_test_project(NO_TESTS_MAIN);

  write_fixture_file(
    project.path(),
    "extra/returns_9.ign",
    "function main(): i32 {\n  return 9;\n}\n",
  );
  let snapshot_path = fixture_snapshot_path(project.path(), "extra/returns_9.ign");

  let options = TestRunOptions {
    update_snapshots: true,
    fixture_dirs: vec![PathBuf::from("extra")],
    ..TestRunOptions::default()
  };
  let result = run_project_tests_with_options(project.path(), &options);

  assert!(result.is_ok(), "expected the CLI fixture directory to be scanned");
  assert_eq!(
    fs::read_to_string(&snapshot_path).expect("read fixture snapshot"),
    "exit_code: 9\nstdout: (empty)\nstderr: (empty)"
  );
}
