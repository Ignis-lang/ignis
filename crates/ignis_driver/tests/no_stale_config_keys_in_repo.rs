//! IGN-239: every `ignis.toml`/`manifest.toml` tracked in the repository
//! should be free of unknown-key warnings. This asserts `check_unknown_keys`
//! finds nothing to warn about for each one, so a stale key (like the
//! `[link]` table IGN-239 review caught in
//! `test_cases/selfhost/build/test_discovery/std/manifest.toml`) does not
//! silently reappear.
//!
//! The file list comes from `git ls-files` rather than a filesystem walk: a
//! walk from the workspace root also reaches whatever a developer's own
//! tooling keeps alongside the checkout (worktrees, editor caches, and the
//! like), which can hold any number of unrelated `ignis.toml`/`manifest.toml`
//! files this test has no business asserting anything about. `git ls-files`
//! reports exactly the tracked set regardless of what else lives on disk. If
//! `git` itself is unavailable (no `.git`, no `git` binary), the test skips
//! rather than failing on an environment it cannot inspect.
//!
//! A file that is not valid TOML (some `test_cases/selfhost/resolver/config`
//! fixtures are deliberately malformed, to exercise the parser's own error
//! path) is skipped: `check_unknown_keys` never runs against text it cannot
//! parse into a `toml::Value` in production either.

use std::path::PathBuf;
use std::process::Command;

use ignis_driver::project::{
  check_unknown_keys, MANIFEST_SCHEMA, MANIFEST_UNKNOWN_KEY_CODE, PROJECT_SCHEMA, PROJECT_UNKNOWN_KEY_CODE,
};
use ignis_type::file::SourceMap;

fn workspace_root() -> PathBuf {
  PathBuf::from(env!("CARGO_MANIFEST_DIR"))
    .join("../..")
    .canonicalize()
    .expect("canonicalize workspace root")
}

/// Every `ignis.toml`/`manifest.toml` `git` tracks, as absolute paths.
///
/// Returns `None` when `git ls-files` cannot be run at all (missing binary,
/// not a git checkout), so the caller can skip cleanly instead of asserting
/// something about a repository state it cannot observe.
fn tracked_config_files() -> Option<Vec<PathBuf>> {
  let root = workspace_root();

  let output = Command::new("git")
    .current_dir(&root)
    .args(["ls-files", "--", "*ignis.toml", "*manifest.toml"])
    .output()
    .ok()?;

  if !output.status.success() {
    return None;
  }

  let listing = String::from_utf8(output.stdout).ok()?;

  Some(
    listing
      .lines()
      .filter(|line| !line.is_empty())
      .map(|relative| root.join(relative))
      .collect(),
  )
}

#[test]
fn every_tracked_config_file_is_free_of_unknown_keys() {
  let Some(files) = tracked_config_files() else {
    eprintln!("skipping: `git ls-files` is unavailable (no git binary, or not a git checkout)");
    return;
  };

  assert!(
    !files.is_empty(),
    "expected `git ls-files` to report at least one ignis.toml/manifest.toml"
  );

  let mut failures = Vec::new();

  for path in files {
    let Ok(content) = std::fs::read_to_string(&path) else {
      continue;
    };
    let Ok(value) = toml::from_str::<toml::Value>(&content) else {
      continue;
    };

    let is_manifest = path.file_name().and_then(|n| n.to_str()) == Some("manifest.toml");
    let (schema, code) = if is_manifest {
      (&MANIFEST_SCHEMA, MANIFEST_UNKNOWN_KEY_CODE)
    } else {
      (&PROJECT_SCHEMA, PROJECT_UNKNOWN_KEY_CODE)
    };

    let mut sm = SourceMap::new();
    let file = sm.add_file(path.clone(), content.clone());
    let mut diagnostics = Vec::new();
    check_unknown_keys(&value, schema, "", &content, file, code, &mut diagnostics);

    for diagnostic in diagnostics {
      failures.push(format!("{}: {}", path.display(), diagnostic.message));
    }
  }

  assert!(
    failures.is_empty(),
    "found unknown-key warnings in tracked config files:\n{}",
    failures.join("\n")
  );
}
