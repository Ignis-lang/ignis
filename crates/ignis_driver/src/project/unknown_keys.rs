//! Unknown-key detection for `ignis.toml` and `std/manifest.toml`.
//!
//! Both files are parsed with `serde`/`toml` without `deny_unknown_fields`, so
//! a stale or misspelled key is silently ignored instead of being reported.
//! This module walks the raw `toml::Value` tree against a small schema
//! describing every table and key each loader accepts, and turns every key
//! that is not in that schema into a warning-level `Diagnostic` rendered
//! through the normal diagnostics pipeline. It never rejects the file: an
//! unknown key is a warning, not an error, so `ignis check`/`build`/`test`
//! keep succeeding.
//!
//! The selfhost compiler (`ignis/config/project.ign`, `ignis/build/manifest.ign`)
//! mirrors this schema and message wording exactly, so a host build and a
//! stage1 selfhost build warn identically on the same project.

use std::path::Path;

use ignis_diagnostics::diagnostic_report::{Diagnostic, Severity};
use ignis_type::file::{FileId, SourceMap};
use ignis_type::span::Span;
use ignis_type::BytePosition;

/// Diagnostic code for an unknown key in `ignis.toml`.
pub const PROJECT_UNKNOWN_KEY_CODE: &str = "C1008";
/// Diagnostic code for an unknown key in `std/manifest.toml`.
pub const MANIFEST_UNKNOWN_KEY_CODE: &str = "B1004";

/// Describes what a TOML table is allowed to contain.
pub enum KeySchema {
  /// A table with a fixed set of allowed keys, each with its own nested schema.
  Struct(&'static [(&'static str, KeySchema)]),
  /// A table whose keys are caller-chosen names (e.g. module names, aliases);
  /// every value is checked against the same nested schema.
  Map(&'static KeySchema),
  /// A scalar, array, or otherwise unstructured value: nothing to check.
  Leaf,
}

/// Schema for `ignis.toml`.
pub const PROJECT_SCHEMA: KeySchema = KeySchema::Struct(&[
  (
    "package",
    KeySchema::Struct(&[
      ("name", KeySchema::Leaf),
      ("version", KeySchema::Leaf),
      ("authors", KeySchema::Leaf),
      ("description", KeySchema::Leaf),
      ("keywords", KeySchema::Leaf),
      ("license", KeySchema::Leaf),
      ("repository", KeySchema::Leaf),
    ]),
  ),
  (
    "ignis",
    KeySchema::Struct(&[("std", KeySchema::Leaf), ("std_path", KeySchema::Leaf)]),
  ),
  (
    "build",
    KeySchema::Struct(&[
      ("bin", KeySchema::Leaf),
      ("source_dir", KeySchema::Leaf),
      ("entry", KeySchema::Leaf),
      ("out_dir", KeySchema::Leaf),
      ("opt_level", KeySchema::Leaf),
      ("debug", KeySchema::Leaf),
      ("target", KeySchema::Leaf),
      ("target_triple", KeySchema::Leaf),
      ("known_features", KeySchema::Leaf),
      ("default_features", KeySchema::Leaf),
      ("cc", KeySchema::Leaf),
      ("cflags", KeySchema::Leaf),
      ("emit", KeySchema::Leaf),
    ]),
  ),
  ("test", KeySchema::Struct(&[("fixtures", KeySchema::Leaf)])),
  ("aliases", KeySchema::Map(&KeySchema::Leaf)),
]);

/// Schema for `std/manifest.toml`.
pub const MANIFEST_SCHEMA: KeySchema = KeySchema::Struct(&[
  ("toolchain", KeySchema::Struct(&[("include_dirs", KeySchema::Leaf)])),
  ("modules", KeySchema::Map(&KeySchema::Leaf)),
  (
    "linking",
    KeySchema::Map(&KeySchema::Struct(&[
      ("header", KeySchema::Leaf),
      ("headers", KeySchema::Leaf),
      ("header_quoted", KeySchema::Leaf),
      ("object", KeySchema::Leaf),
      ("archive", KeySchema::Leaf),
      ("lib", KeySchema::Leaf),
    ])),
  ),
  ("auto_load", KeySchema::Struct(&[("modules", KeySchema::Leaf)])),
  ("compile_only", KeySchema::Struct(&[("modules", KeySchema::Leaf)])),
]);

/// Levenshtein edit distance between two strings, compared codepoint by codepoint.
fn edit_distance(
  a: &str,
  b: &str,
) -> usize {
  let a: Vec<char> = a.chars().collect();
  let b: Vec<char> = b.chars().collect();
  let (n, m) = (a.len(), b.len());

  let mut row: Vec<usize> = (0..=m).collect();

  for i in 1..=n {
    let mut prev = row[0];
    row[0] = i;

    for j in 1..=m {
      let cur = row[j];
      row[j] = if a[i - 1] == b[j - 1] {
        prev
      } else {
        1 + prev.min(row[j]).min(row[j - 1])
      };
      prev = cur;
    }
  }

  row[m]
}

/// Finds the closest known key to `key` within edit distance 2, if any.
///
/// Ties break on the order keys are declared in the schema.
fn suggest(
  key: &str,
  known: &[(&'static str, KeySchema)],
) -> Option<&'static str> {
  let mut best: Option<(&'static str, usize)> = None;

  for (candidate, _) in known {
    let distance = edit_distance(key, candidate);

    if distance <= 2 {
      match best {
        Some((_, best_distance)) if best_distance <= distance => {},
        _ => best = Some((candidate, distance)),
      }
    }
  }

  best.map(|(name, _)| name)
}

/// Locates the byte span of `key` inside `table_path`'s section of `source`.
///
/// `table_path` is the dot-joined path of the enclosing table (empty for the
/// root). `is_table` is true when the unknown entry is itself a table (so it
/// is searched for as a `[table_path.key]` header instead of a `key = value`
/// line). This is a best-effort text search used only to give the warning a
/// source location; an unmatched key falls back to the start of the file.
fn locate_key(
  source: &str,
  table_path: &str,
  key: &str,
  is_table: bool,
) -> (u32, u32) {
  let lines: Vec<&str> = source.split('\n').collect();
  let mut offset: u32 = 0;
  let mut in_section = table_path.is_empty();

  for line in &lines {
    let trimmed = line.trim();

    if trimmed.starts_with('[') {
      let header = trimmed.trim_start_matches('[').trim_end_matches(']').trim();
      in_section = !table_path.is_empty() && (header == table_path || header.starts_with(&format!("{table_path}.")));
    }

    if is_table {
      let target_header = format!("[{}.{}]", table_path, key);
      let matches = if table_path.is_empty() {
        trimmed == format!("[{}]", key) || trimmed.starts_with(&format!("[{}.", key))
      } else {
        trimmed == target_header
      };

      if matches && let Some(pos) = line.find(key) {
        return (offset + pos as u32, offset + pos as u32 + key.len() as u32);
      }
    } else if in_section {
      // A bare key (`key = ...`) spans just the key text; a quoted key
      // (`"key" = ...`, needed for a key TOML's bare-key syntax can't spell,
      // such as a non-ASCII one) spans the whole quoted token including its
      // quotes, matching the selfhost's own lexer-token-based key span.
      let bare = format!("{key} ");
      let bare_eq = format!("{key}=");
      let quoted = format!("\"{key}\"");

      if (trimmed.starts_with(&bare) || trimmed.starts_with(&bare_eq))
        && let Some(pos) = line.find(key)
      {
        return (offset + pos as u32, offset + pos as u32 + key.len() as u32);
      }

      if trimmed.starts_with(&quoted)
        && let Some(pos) = line.find(&quoted)
      {
        return (offset + pos as u32, offset + pos as u32 + quoted.len() as u32);
      }
    }

    offset += line.len() as u32 + 1;
  }

  (0, 0)
}

fn table_label(table_path: &str) -> String {
  if table_path.is_empty() {
    "top-level".to_string()
  } else {
    table_path.to_string()
  }
}

/// Builds the "unknown key" warning message shared by both compilers.
fn unknown_key_message(
  table_path: &str,
  key: &str,
  suggestion: Option<&str>,
) -> String {
  match suggestion {
    Some(suggestion) => format!(
      "unknown key '{key}' in [{}]; did you mean '{suggestion}'?",
      table_label(table_path)
    ),
    None => format!("unknown key '{key}' in [{}]", table_label(table_path)),
  }
}

/// Walks `value` against `schema`, collecting one warning `Diagnostic` per
/// unknown key or table, and recursing into every known nested table.
pub fn check_unknown_keys(
  value: &toml::Value,
  schema: &KeySchema,
  table_path: &str,
  source: &str,
  file: FileId,
  code: &'static str,
  out: &mut Vec<Diagnostic>,
) {
  let table = match value.as_table() {
    Some(table) => table,
    None => return,
  };

  match schema {
    KeySchema::Struct(known) => {
      for (key, val) in table {
        match known.iter().find(|(name, _)| name == key) {
          Some((_, nested)) => {
            let nested_path = if table_path.is_empty() {
              key.clone()
            } else {
              format!("{table_path}.{key}")
            };
            check_unknown_keys(val, nested, &nested_path, source, file, code, out);
          },
          None => {
            let is_table = val.is_table();
            let suggestion = suggest(key, known);
            let message = unknown_key_message(table_path, key, suggestion);
            let (start, end) = locate_key(source, table_path, key, is_table);
            out.push(Diagnostic::new(
              Severity::Warning,
              message,
              code.to_string(),
              Span::new(file, BytePosition(start), BytePosition(end)),
            ));
          },
        }
      }
    },
    KeySchema::Map(nested) => {
      for (key, val) in table {
        let nested_path = if table_path.is_empty() {
          key.clone()
        } else {
          format!("{table_path}.{key}")
        };
        check_unknown_keys(val, nested, &nested_path, source, file, code, out);
      }
    },
    KeySchema::Leaf => {},
  }
}

/// Warns on stderr for every key or table `std/manifest.toml` does not
/// recognize. Mirrors `warn_unknown_keys` in `project::find` for `ignis.toml`;
/// see there for why this only warns and never fails the load.
pub fn warn_unknown_manifest_keys(
  content: &str,
  manifest_path: &Path,
) {
  let Ok(value) = toml::from_str::<toml::Value>(content) else {
    return;
  };

  let mut sm = SourceMap::new();
  let file = sm.add_file(manifest_path.to_path_buf(), content.to_string());

  let mut diagnostics = Vec::new();
  check_unknown_keys(
    &value,
    &MANIFEST_SCHEMA,
    "",
    content,
    file,
    MANIFEST_UNKNOWN_KEY_CODE,
    &mut diagnostics,
  );

  // See the matching sort in `project::find::warn_unknown_keys`: it keeps
  // the host's `BTreeMap`-ordered walk in file order, matching the
  // selfhost's line-scanning loader.
  diagnostics.sort_by_key(|diagnostic| diagnostic.primary_span.start);

  ignis_diagnostics::render_batch_to_stderr(&diagnostics, &sm);
}

#[cfg(test)]
mod tests {
  use super::*;

  fn diagnostics_for(
    source: &str,
    schema: &KeySchema,
    code: &'static str,
  ) -> Vec<Diagnostic> {
    let value: toml::Value = toml::from_str(source).unwrap();
    let mut out = Vec::new();
    check_unknown_keys(&value, schema, "", source, FileId::default(), code, &mut out);
    out
  }

  #[test]
  fn detects_unknown_key_with_suggestion() {
    let source = r#"
[package]
name = "test"
version = "0.1.0"

[ignis]
std_pth = "../std"
"#;
    let diags = diagnostics_for(source, &PROJECT_SCHEMA, PROJECT_UNKNOWN_KEY_CODE);
    assert_eq!(diags.len(), 1);
    assert_eq!(diags[0].error_code, PROJECT_UNKNOWN_KEY_CODE);
    assert_eq!(diags[0].severity, Severity::Warning);
    assert_eq!(diags[0].message, "unknown key 'std_pth' in [ignis]; did you mean 'std_path'?");
  }

  #[test]
  fn detects_unknown_key_without_suggestion() {
    let source = r#"
[package]
name = "test"
version = "0.1.0"

[build]
totally_unrelated_garbage = 1
"#;
    let diags = diagnostics_for(source, &PROJECT_SCHEMA, PROJECT_UNKNOWN_KEY_CODE);
    assert_eq!(diags.len(), 1);
    assert_eq!(diags[0].message, "unknown key 'totally_unrelated_garbage' in [build]");
  }

  #[test]
  fn detects_unknown_top_level_table() {
    let source = r#"
[package]
name = "test"
version = "0.1.0"

[buld]
opt_level = 2
"#;
    let diags = diagnostics_for(source, &PROJECT_SCHEMA, PROJECT_UNKNOWN_KEY_CODE);
    assert_eq!(diags.len(), 1);
    assert_eq!(diags[0].message, "unknown key 'buld' in [top-level]; did you mean 'build'?");
  }

  #[test]
  fn accepts_every_known_key() {
    let source = r#"
[package]
name = "test"
version = "0.1.0"
authors = ["me"]
description = "d"
keywords = ["k"]
license = "MIT"
repository = "https://example.com"

[ignis]
std = true
std_path = "../std"

[build]
bin = true
source_dir = "src"
entry = "main.ign"
out_dir = "build"
opt_level = 2
debug = true
target = "c"
target_triple = "x86_64-unknown-linux-gnu"
known_features = ["a"]
default_features = ["a"]
cc = "gcc"
cflags = ["-Wall"]
emit = ["c"]

[test]
fixtures = ["fixtures"]

[aliases]
mylib = "libs/mylib"
"#;
    let diags = diagnostics_for(source, &PROJECT_SCHEMA, PROJECT_UNKNOWN_KEY_CODE);
    assert!(diags.is_empty(), "unexpected diagnostics: {diags:?}");
  }

  #[test]
  fn manifest_detects_unknown_key_in_linking_subtable() {
    let source = r#"
[modules]
io = "io/mod.ign"

[linking.io]
header = "runtime/io.h"
objct = "runtime/libio.o"
"#;
    let diags = diagnostics_for(source, &MANIFEST_SCHEMA, MANIFEST_UNKNOWN_KEY_CODE);
    assert_eq!(diags.len(), 1);
    assert_eq!(diags[0].message, "unknown key 'objct' in [linking.io]; did you mean 'object'?");
  }

  #[test]
  fn manifest_accepts_every_known_key() {
    let source = r#"
[toolchain]
include_dirs = ["."]

[modules]
io = "io/mod.ign"

[linking.io]
header = "runtime/io.h"
headers = []
header_quoted = false
object = "runtime/libio.o"
archive = "runtime/libio.a"
lib = "m"

[auto_load]
modules = ["string"]

[compile_only]
modules = ["compile"]
"#;
    let diags = diagnostics_for(source, &MANIFEST_SCHEMA, MANIFEST_UNKNOWN_KEY_CODE);
    assert!(diags.is_empty(), "unexpected diagnostics: {diags:?}");
  }

  /// IGN-239 review #7: edit distance compares codepoints, not bytes, so a
  /// non-ASCII key within codepoint distance 2 of a known key still gets a
  /// suggestion (a byte-based distance would place a multi-byte substitution
  /// too far away). Also exercises the quoted-key span (TOML's bare-key
  /// syntax cannot spell a non-ASCII key), which `locate_key` finds and
  /// spans by the whole quoted token, matching the selfhost's key span.
  #[test]
  fn non_ascii_key_gets_a_suggestion_and_a_quoted_span() {
    let source = "[package]\nname = \"test\"\nversion = \"0.1.0\"\n\n[build]\n\"\u{e7}\u{e7}\" = \"clang\"\n";
    let diags = diagnostics_for(source, &PROJECT_SCHEMA, PROJECT_UNKNOWN_KEY_CODE);

    assert_eq!(diags.len(), 1);
    assert_eq!(diags[0].message, "unknown key '\u{e7}\u{e7}' in [build]; did you mean 'cc'?");
    // The span covers the whole quoted token `"çç"` (4 codepoints), not just
    // the 2 codepoints of the bare key text.
    assert_eq!(diags[0].primary_span.end.0 - diags[0].primary_span.start.0, 6);
  }

  /// IGN-239 review #1: `toml::Value`'s table is a `BTreeMap`, so it visits
  /// keys alphabetically ("build" before "package" before "zzz_unknown"),
  /// not in file order. Production sorts by `primary_span.start` after
  /// calling `check_unknown_keys` (see `warn_unknown_keys`); this pins that
  /// exact sort against the raw, unsorted walk order regressing.
  #[test]
  fn warnings_sort_into_file_order_despite_the_btreemap_walk() {
    let source = r#"
[package]
name = "test"
version = "0.1.0"

[zzz_unknown_table]
x = 1

[ignis]
std = true
alpha_unknown_in_ignis = 1

[build]
source_dir = "src"
entry = "main.ign"
beta_unknown_in_build = 1
"#;
    let mut diags = diagnostics_for(source, &PROJECT_SCHEMA, PROJECT_UNKNOWN_KEY_CODE);
    diags.sort_by_key(|d| d.primary_span.start);

    assert_eq!(diags.len(), 3);
    assert_eq!(diags[0].message, "unknown key 'zzz_unknown_table' in [top-level]");
    assert_eq!(diags[1].message, "unknown key 'alpha_unknown_in_ignis' in [ignis]");
    assert_eq!(diags[2].message, "unknown key 'beta_unknown_in_build' in [build]");
  }
}
