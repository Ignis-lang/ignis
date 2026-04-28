mod common;

use std::fs;
use std::path::PathBuf;

use ignis_config::{IgnisSTDManifest, TargetBackend};

fn workspace_std_path() -> PathBuf {
  PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../std")
}

fn load_std_manifest() -> IgnisSTDManifest {
  let manifest_path = workspace_std_path().join("manifest.toml");
  let content = fs::read_to_string(&manifest_path).expect("read std manifest");
  toml::from_str(&content).expect("parse std manifest")
}

#[test]
fn std_compile_manifest_registers_compile_only_module_without_runtime_linking() {
  let manifest = load_std_manifest();

  assert_eq!(
    manifest.get_module_path("compile").map(|path| path.as_str()),
    Some("compile/mod.ign")
  );
  assert!(manifest.is_compile_only("compile"), "std::compile should be compile-time-only");
  assert!(
    !manifest.is_auto_load("compile"),
    "std::compile should remain opt-in and stay out of auto_load.modules"
  );
  assert!(
    manifest.get_linking_info("compile").is_none(),
    "std::compile should not expose runtime linking metadata"
  );
}

#[test]
fn std_compile_build_std_skips_runtime_codegen_outputs() {
  let attempt = common::build_std_with_target(TargetBackend::C).expect("temporary std build setup should succeed");

  assert!(
    attempt.result.is_ok(),
    "expected build_std to succeed with std::compile present"
  );
  assert!(
    !attempt.output_dir.join("std/include/std_compile.h").exists(),
    "compile-time-only std::compile should not emit a runtime header"
  );
  assert!(
    !attempt.output_dir.join("std/src/std_compile.c").exists(),
    "compile-time-only std::compile should not emit runtime C"
  );
  assert!(
    !attempt.output_dir.join("std/obj/std_compile.o").exists(),
    "compile-time-only std::compile should not emit a runtime object file"
  );
}
