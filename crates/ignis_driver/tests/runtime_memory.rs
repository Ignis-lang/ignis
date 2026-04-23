use std::path::PathBuf;
use std::process::Command;

#[test]
fn runtime_memory_foundations_smoke() {
  let runtime_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../std/runtime");

  let output = Command::new("make")
    .arg("test")
    .current_dir(&runtime_dir)
    .output()
    .expect("runtime smoke test command should start");

  assert!(
    output.status.success(),
    "runtime smoke tests failed\nstdout:\n{}\nstderr:\n{}",
    String::from_utf8_lossy(&output.stdout),
    String::from_utf8_lossy(&output.stderr),
  );
}
