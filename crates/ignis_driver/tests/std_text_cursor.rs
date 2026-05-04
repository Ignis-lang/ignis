use std::path::PathBuf;

use ignis_driver::run_std_tests;
use tempfile::TempDir;

fn workspace_std_path() -> PathBuf {
  PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../std")
}

#[test]
fn run_std_tests_executes_string_utf8_cursor_slice() {
  let output_dir = TempDir::new().expect("temporary std output dir");

  let result = run_std_tests(
    &workspace_std_path(),
    Some("string::tests::utf8CursorSlicesByByteRange"),
    false,
    Some(output_dir.path()),
  );

  assert!(result.is_ok(), "expected string UTF-8 cursor slice to succeed");
}
