//! IGN-239: every `ignis.toml`/`manifest.toml` tracked in the repository
//! should be free of unknown-key warnings. This walks the workspace looking
//! for both filenames and asserts `check_unknown_keys` finds nothing to warn
//! about, so a stale key (like the `[link]` table IGN-239 review caught in
//! `test_cases/selfhost/build/test_discovery/std/manifest.toml`) does not
//! silently reappear.
//!
//! A file that is not valid TOML (some `test_cases/selfhost/resolver/config`
//! fixtures are deliberately malformed, to exercise the parser's own error
//! path) is skipped: `check_unknown_keys` never runs against text it cannot
//! parse into a `toml::Value` in production either.

use std::path::{Path, PathBuf};

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

fn walk(
  dir: &Path,
  out: &mut Vec<PathBuf>,
) {
  let Ok(entries) = std::fs::read_dir(dir) else {
    return;
  };

  for entry in entries.flatten() {
    let path = entry.path();

    if path.is_dir() {
      // `build/` and `target*/` hold generated artifacts and are gitignored;
      // walking them is both wasted work and liable to see half-written files.
      let name = path.file_name().and_then(|n| n.to_str()).unwrap_or("");
      if name == "build" || name.starts_with("target") || name == ".git" {
        continue;
      }
      walk(&path, out);
    } else if path.file_name().and_then(|n| n.to_str()) == Some("ignis.toml")
      || path.file_name().and_then(|n| n.to_str()) == Some("manifest.toml")
    {
      out.push(path);
    }
  }
}

#[test]
fn every_tracked_config_file_is_free_of_unknown_keys() {
  let root = workspace_root();
  let mut files = Vec::new();
  walk(&root, &mut files);

  assert!(!files.is_empty(), "expected to find at least one ignis.toml/manifest.toml");

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
