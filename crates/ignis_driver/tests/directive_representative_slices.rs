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
      "[package]\nname = \"directive_representative_fixture\"\nversion = \"0.1.0\"\nauthors = []\ndescription = \"fixture\"\nkeywords = []\nlicense = \"MIT\"\nrepository = \"\"\n\n[ignis]\nstd = true\nstd_path = \"{}\"\n\n[build]\nbin = true\nsource_dir = \"src\"\nentry = \"main.ign\"\nout_dir = \"build\"\nopt_level = 0\ndebug = false\ntarget = \"c\"\ncc = \"cc\"\ncflags = []\nemit = []\n",
      workspace_std_path().display()
    ),
  )
  .expect("write ignis.toml");

  temp_dir
}

fn harness_binary_path(project_root: &Path) -> PathBuf {
  project_root.join("build/bin/directive_representative_fixture-tests")
}

#[test]
fn serde_like_eq_slice_keeps_directives_compile_only_while_runtime_eq_contract_stays_handwritten() {
  let attempt = common::compile_project_single_file_with_workspace_std(
    r#"
import Compile from "std::compile";
import Eq from "std::hash";

@directive(target: "record", phase: expand, effect: emit, group: "serde")
function deriveEq(target: Compile::ItemRef, context: Compile::Context): void {
    return;
}

@deriveEq
@implements(Eq)
record UserId {
    public value: i32;

    equals(&self, other: &UserId): boolean {
        return self.value == other.value;
    }
}

function main(): i32 {
    let left: UserId = UserId { value: 7 };
    let right: UserId = UserId { value: 7 };

    if (left.equals(&right)) {
        return 0;
    }

    return 1;
}
"#,
    ignis_config::TargetBackend::C,
  )
  .expect("temporary project build setup should succeed");

  assert!(
    attempt.result.is_ok(),
    "expected the Eq-like representative slice to compile cleanly"
  );
  assert!(
    attempt.bin_path.exists(),
    "expected the representative Eq slice to build a runtime binary"
  );
}

#[test]
fn http_like_collect_finalize_slice_compiles_as_a_project_story() {
  let attempt = common::compile_project_single_file_with_workspace_std(
    r#"
      import Compile from "std::compile";

      @directive(target: "record", phase: collect, effect: collect, group: "routes")
      function route(target: Compile::ItemRef, collector: Compile::Collector): void {
        return;
      }

      @directive(target: "record", phase: finalize, effect: collect, group: "routes")
      function finalizeRoutes(target: Compile::ItemRef, collector: Compile::Collector): void {
        return;
      }

      @route
      @finalizeRoutes
      record UsersRoute {
        path: str;
      }

      function main(): i32 {
        return 0;
      }
    "#,
    ignis_config::TargetBackend::C,
  )
  .expect("temporary project build setup should succeed");

  assert!(attempt.result.is_ok(), "expected route collection slice to compile cleanly");
  assert!(
    attempt.bin_path.exists(),
    "expected the route collection slice to build a runtime binary"
  );
}

#[test]
fn test_like_directive_slice_keeps_top_level_test_discovery_as_the_runtime_source_of_truth() {
  let project = write_test_project(
    r#"
import Compile from "std::compile";
import Test from "std::test";

@directive(target: "function", phase: check, effect: diagnose)
function generatedTestMarker(target: Compile::ItemRef, context: Compile::Context): void {
    return;
}

@generatedTestMarker
function helper(): void {
    return;
}

@test
function smoke(): void {
    helper();
    Test::assert(true);
}
"#,
  );

  let result = run_project_tests(project.path(), None, false);

  assert!(
    result.is_ok(),
    "expected top-level @test to remain the runtime discovery contract when directives target functions"
  );
  assert!(
    harness_binary_path(project.path()).exists(),
    "expected the test-like slice to build the native harness"
  );
}
