use std::path::PathBuf;

use ignis_driver::run_std_tests;
use tempfile::TempDir;

fn workspace_std_path() -> PathBuf {
  PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../std")
}

#[test]
fn run_std_tests_executes_toml_lexer_slice() {
  let output_dir = TempDir::new().expect("temporary std output dir");

  let result = run_std_tests(&workspace_std_path(), Some("toml::tests"), false, Some(output_dir.path()));

  assert!(result.is_ok(), "expected toml std lexer slice to succeed");
}
