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
fn std_serializer_manifest_registers_module_without_autoload() {
  let manifest = load_std_manifest();

  assert_eq!(
    manifest.get_module_path("serializer").map(|path| path.as_str()),
    Some("serializer/mod.ign")
  );
  assert!(
    !manifest.is_auto_load("serializer"),
    "serializer should remain opt-in and stay out of auto_load.modules"
  );
}

#[test]
fn std_serializer_build_std_succeeds() {
  let attempt = common::build_std_with_target(TargetBackend::C).expect("temporary std build setup should succeed");

  assert!(
    attempt.result.is_ok(),
    "expected build_std to succeed with serializer prerequisites present"
  );
  assert!(
    attempt.output_dir.join("std/lib/libignis_std.a").exists(),
    "expected build_std to emit the std archive"
  );
}
