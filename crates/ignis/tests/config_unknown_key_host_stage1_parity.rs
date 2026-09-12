//! IGN-239: the host and the selfhost (stage1) compiler must warn identically
//! on an unknown `ignis.toml`/`std/manifest.toml` key. This builds the
//! selfhost compiler once with the host, then runs both compilers against the
//! same fixture project and diffs their stderr after stripping ANSI color
//! codes (the selfhost renderer always colors its output, the host's `colored`
//! crate does not when stdout/stderr are not a TTY — the same normalization
//! `scripts/selfhost_e2e_parity.py` already applies for diagnostic parity).

use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::OnceLock;

use tempfile::TempDir;

const SOURCE: &str = r#"
function main(): i32 {
    return 0;
}
"#;

fn workspace_root() -> PathBuf {
  PathBuf::from(env!("CARGO_MANIFEST_DIR"))
    .join("../..")
    .canonicalize()
    .expect("canonicalize workspace root")
}

fn selfhost_compiler() -> &'static Path {
  static COMPILER: OnceLock<PathBuf> = OnceLock::new();

  COMPILER.get_or_init(|| {
    let workspace_root = workspace_root();

    let build = Command::new(env!("CARGO_BIN_EXE_ignis"))
      .current_dir(&workspace_root)
      .arg("build")
      .arg("--project")
      .arg(".")
      .arg("ignis/main.ign")
      .output()
      .expect("build selfhost compiler");

    assert!(
      build.status.success(),
      "expected selfhost build to succeed\nstdout:\n{}\nstderr:\n{}",
      String::from_utf8_lossy(&build.stdout),
      String::from_utf8_lossy(&build.stderr)
    );

    workspace_root.join("build/selfhost/bin/ignis")
  })
}

fn strip_ansi(text: &str) -> String {
  let mut out = String::with_capacity(text.len());
  let mut chars = text.chars().peekable();

  while let Some(c) = chars.next() {
    if c == '\u{1b}' && chars.peek() == Some(&'[') {
      chars.next();
      for next in chars.by_ref() {
        if next.is_ascii_alphabetic() {
          break;
        }
      }
      continue;
    }
    out.push(c);
  }

  out
}

fn write_fixture_project(unknown_key_line: &str) -> TempDir {
  let temp_dir = TempDir::new().expect("temporary project dir");
  let src_dir = temp_dir.path().join("src");

  std::fs::create_dir_all(&src_dir).expect("create src dir");
  std::fs::write(src_dir.join("main.ign"), SOURCE).expect("write main module");

  // The selfhost resolver does not support `[ignis] std = false` the way the
  // host does (it always requires a resolvable std manifest), so this fixture
  // points at the real std tree instead of disabling it, keeping both
  // compilers on their success path.
  let std_path = workspace_root().join("std");

  std::fs::write(
    temp_dir.path().join("ignis.toml"),
    format!(
      "[package]\nname = \"config_unknown_key_parity_fixture\"\nversion = \"0.1.0\"\n\n[ignis]\nstd_path = \"{std_path}\"\n{unknown_key_line}\n\n[build]\nsource_dir = \"src\"\nentry = \"main.ign\"\n",
      std_path = std_path.display()
    ),
  )
  .expect("write ignis.toml");

  temp_dir
}

fn run_check(
  compiler: &Path,
  project_dir: &Path,
  use_project_flag: bool,
) -> std::process::Output {
  let mut command = Command::new(compiler);
  command.current_dir(project_dir);

  if use_project_flag {
    command.arg("check").arg("--project").arg(".");
  } else {
    command.arg("check");
  }

  command.output().expect("run ignis check")
}

#[test]
fn host_and_stage1_warn_identically_on_an_unknown_project_key() {
  let stage1 = selfhost_compiler();
  let project = write_fixture_project("runtime_path = \"../std\"");

  let host_output = run_check(Path::new(env!("CARGO_BIN_EXE_ignis")), project.path(), true);
  let stage1_output = run_check(stage1, project.path(), false);

  assert!(
    host_output.status.success(),
    "expected the host to succeed despite the unknown key\nstderr:\n{}",
    String::from_utf8_lossy(&host_output.stderr)
  );
  assert!(
    stage1_output.status.success(),
    "expected stage1 to succeed despite the unknown key\nstderr:\n{}",
    String::from_utf8_lossy(&stage1_output.stderr)
  );

  let host_line = strip_ansi(&String::from_utf8_lossy(&host_output.stderr))
    .lines()
    .find(|line| line.contains("unknown key"))
    .map(str::to_string)
    .expect("expected the host to warn about the unknown key");

  let stage1_line = strip_ansi(&String::from_utf8_lossy(&stage1_output.stderr))
    .lines()
    .find(|line| line.contains("unknown key"))
    .map(str::to_string)
    .expect("expected stage1 to warn about the unknown key");

  assert_eq!(
    host_line, stage1_line,
    "host and stage1 must warn with byte-identical text (after stripping ANSI color codes)"
  );
  assert_eq!(host_line, "Warning[C1008]: unknown key 'runtime_path' in [ignis]");
}
