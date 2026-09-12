//! Regression coverage for the compiler-identity build cache invalidation.
//!
//! Module and std stamps were previously keyed only on source content hashes
//! plus a declared `compiler_version` string. Rebuilding the compiler itself
//! (without bumping `CARGO_PKG_VERSION`, the common case during local
//! development) left every cached C/object/archive stamp looking "valid", so
//! `ignis build` silently reused stale output.
//!
//! These tests simulate "a different compiler binary ran last time" by
//! rewriting a stamp file's `compiler_identity=` line to a bogus value after
//! a first successful build, then rebuilding into the same output directory
//! and asserting the affected artifact is recompiled.
//!
//! A full std build is expensive (compiling and archiving ~60 C files
//! through a debug-profile driver), so every test here seeds its own output
//! directory from one shared std build (`common::seed_std_from_shared_snapshot`)
//! instead of triggering its own. Only `changed_compiler_identity_forces_std_archive_rebuild`
//! goes on to force a second, genuine std rebuild, since that rebuild is
//! exactly what it exists to prove happens.

mod common;

use std::path::Path;

use common::{compile_workspace_std_project_in, compile_workspace_std_project_in_with_force, seed_std_from_shared_snapshot};
use ignis_config::TargetBackend;
use tempfile::TempDir;

const MAIN_SOURCE: &str = r#"
function main(): i32 {
  return 0;
}
"#;

fn rewrite_stamp_compiler_identity(
  stamp_path: &Path,
  bogus_identity: &str,
) {
  let original = std::fs::read_to_string(stamp_path).expect("stamp file should exist after a successful build");

  let rewritten: String = original
    .lines()
    .map(|line| {
      if line.starts_with("compiler_identity=") {
        format!("compiler_identity={}", bogus_identity)
      } else {
        line.to_string()
      }
    })
    .collect::<Vec<_>>()
    .join("\n")
    + "\n";

  assert_ne!(
    original, rewritten,
    "expected the stamp to contain a `compiler_identity=` line to rewrite"
  );

  std::fs::write(stamp_path, rewritten).expect("failed to rewrite stamp file");
}

#[test]
fn changed_compiler_identity_forces_module_object_recompilation() {
  let temp_dir = TempDir::new().expect("failed to create temp dir");
  let source_path = temp_dir.path().join("main.ign");
  let output_dir = temp_dir.path().join("build");
  std::fs::create_dir_all(&output_dir).expect("failed to create build dir");
  std::fs::write(&source_path, MAIN_SOURCE).expect("failed to write source file");
  seed_std_from_shared_snapshot(&output_dir, TargetBackend::C);

  compile_workspace_std_project_in(&source_path, &output_dir, TargetBackend::C).expect("first build should succeed");

  let stamp_path = output_dir.join("user").join(".stamps").join("main.stamp");
  let c_path = output_dir.join("user").join("src").join("main.c");
  assert!(stamp_path.exists(), "expected a module stamp at {}", stamp_path.display());
  assert!(c_path.exists(), "expected emitted C at {}", c_path.display());

  // A no-op rebuild against the same (unmodified) stamp must be a cache hit:
  // the emitted C is left untouched.
  let c_mtime_before_noop = std::fs::metadata(&c_path).unwrap().modified().unwrap();
  compile_workspace_std_project_in(&source_path, &output_dir, TargetBackend::C).expect("no-op rebuild should succeed");
  let c_mtime_after_noop = std::fs::metadata(&c_path).unwrap().modified().unwrap();
  assert_eq!(
    c_mtime_before_noop, c_mtime_after_noop,
    "an unmodified stamp must be treated as a cache hit and must not re-emit C"
  );

  // Simulate a rebuilt compiler binary that kept the same declared
  // `compiler_version` but has a different identity: overwrite the stamp's
  // `compiler_identity` with a value that cannot match the identity computed
  // by the process actually running this test.
  rewrite_stamp_compiler_identity(&stamp_path, "bogus-different-compiler-identity-0.0.0");

  let c_content_before = std::fs::read_to_string(&c_path).expect("failed to read emitted C before rebuild");
  let obj_path = output_dir.join("user").join("obj").join("main.o");
  let obj_mtime_before = std::fs::metadata(&obj_path).unwrap().modified().unwrap();

  // Sleep past typical filesystem mtime granularity so a re-emitted object
  // file is guaranteed to carry a later timestamp.
  std::thread::sleep(std::time::Duration::from_millis(1100));

  compile_workspace_std_project_in(&source_path, &output_dir, TargetBackend::C)
    .expect("rebuild after a compiler-identity change should succeed");

  let obj_mtime_after = std::fs::metadata(&obj_path).unwrap().modified().unwrap();
  assert!(
    obj_mtime_after > obj_mtime_before,
    "the object file must be recompiled (newer mtime) once the stamp's compiler identity no longer matches"
  );

  // The stale identity must have invalidated the cache: the pipeline
  // recompiles the module, re-emits the C file, and writes a fresh stamp
  // whose identity no longer matches the bogus value.
  let rewritten_stamp = std::fs::read_to_string(&stamp_path).expect("stamp file should exist after the second build");
  assert!(
    !rewritten_stamp.contains("bogus-different-compiler-identity-0.0.0"),
    "the stamp must be rewritten with the current compiler's identity, not the bogus one"
  );

  // The C content itself may be byte-identical (same source, same
  // compiler), so the reliable re-emission signal is the object's mtime and
  // the stamp's rewritten identity, both asserted above.
  let c_content_after = std::fs::read_to_string(&c_path).expect("failed to read emitted C after rebuild");
  assert_eq!(
    c_content_before, c_content_after,
    "content should be stable across rebuilds of the same source with the same compiler"
  );
}

#[test]
fn changed_compiler_identity_forces_std_archive_rebuild() {
  let temp_dir = TempDir::new().expect("failed to create temp dir");
  let source_path = temp_dir.path().join("main.ign");
  let output_dir = temp_dir.path().join("build");
  std::fs::create_dir_all(&output_dir).expect("failed to create build dir");
  std::fs::write(&source_path, MAIN_SOURCE).expect("failed to write source file");
  seed_std_from_shared_snapshot(&output_dir, TargetBackend::C);

  compile_workspace_std_project_in(&source_path, &output_dir, TargetBackend::C).expect("first build should succeed");

  let std_stamp_path = output_dir.join("std").join(".stamp");
  let std_archive_path = output_dir.join("std").join("lib").join("libignis_std.a");
  assert!(std_stamp_path.exists(), "expected a std stamp at {}", std_stamp_path.display());
  assert!(
    std_archive_path.exists(),
    "expected a std archive at {}",
    std_archive_path.display()
  );

  rewrite_stamp_compiler_identity(&std_stamp_path, "bogus-different-compiler-identity-0.0.0");

  let archive_mtime_before = std::fs::metadata(&std_archive_path).unwrap().modified().unwrap();
  std::thread::sleep(std::time::Duration::from_millis(1100));

  compile_workspace_std_project_in(&source_path, &output_dir, TargetBackend::C)
    .expect("rebuild after a std compiler-identity change should succeed");

  let archive_mtime_after = std::fs::metadata(&std_archive_path).unwrap().modified().unwrap();
  assert!(
    archive_mtime_after > archive_mtime_before,
    "the std archive must be rebuilt (newer mtime) once its stamp's compiler identity no longer matches"
  );

  let rewritten_stamp =
    std::fs::read_to_string(&std_stamp_path).expect("std stamp file should exist after the second build");
  assert!(
    !rewritten_stamp.contains("bogus-different-compiler-identity-0.0.0"),
    "the std stamp must be rewritten with the current compiler's identity, not the bogus one"
  );
}

#[test]
fn force_rebuild_recompiles_a_module_with_an_otherwise_valid_cache() {
  let temp_dir = TempDir::new().expect("failed to create temp dir");
  let source_path = temp_dir.path().join("main.ign");
  let output_dir = temp_dir.path().join("build");
  std::fs::create_dir_all(&output_dir).expect("failed to create build dir");
  std::fs::write(&source_path, MAIN_SOURCE).expect("failed to write source file");
  seed_std_from_shared_snapshot(&output_dir, TargetBackend::C);

  compile_workspace_std_project_in(&source_path, &output_dir, TargetBackend::C).expect("first build should succeed");

  let obj_path = output_dir.join("user").join("obj").join("main.o");
  assert!(obj_path.exists(), "expected an object at {}", obj_path.display());
  let obj_mtime_before = std::fs::metadata(&obj_path).unwrap().modified().unwrap();

  std::thread::sleep(std::time::Duration::from_millis(1100));

  // Nothing changed -- source, stamp and compiler identity are all still
  // valid -- so only `--force` (`force_rebuild`) should cause a rebuild here.
  compile_workspace_std_project_in_with_force(&source_path, &output_dir, TargetBackend::C)
    .expect("forced rebuild should succeed");

  let obj_mtime_after = std::fs::metadata(&obj_path).unwrap().modified().unwrap();
  assert!(
    obj_mtime_after > obj_mtime_before,
    "--force must recompile the module even though its cache is otherwise valid"
  );
}
