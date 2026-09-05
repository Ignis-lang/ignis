//! Program fixture cases for `ignis test`.
//!
//! A fixture is a standalone `.ign` file under a directory listed in the
//! project's `[test] fixtures`. The runner compiles it on its own, runs it (or
//! expects it to fail to compile), and compares the observed result against a
//! sibling `__snapshots__/<stem>.snap` baseline written in the same byte format
//! `std::test::Test::assertSnapshot` uses.
//!
//! Fixtures exist so the end-to-end corpus lives in Ignis source instead of
//! inline Rust literals, which lets the self-hosted compiler run the same
//! corpus with the same expectations.

use std::path::{Path, PathBuf};

/// Directory holding the baselines for the fixtures next to it.
const SNAPSHOT_DIR_NAME: &str = "__snapshots__";

/// Extension of a fixture baseline file.
const SNAPSHOT_EXTENSION: &str = "snap";

/// Prefix every fixture plan entry carries.
const FIXTURE_NAME_PREFIX: &str = "e2e";

/// Marker introducing a fixture header option.
const HEADER_MARKER: &str = "// e2e:";

/// How a fixture is compiled and what its baseline records.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum FixtureMode {
  /// Compile, link and run the program; the baseline holds exit code and streams.
  Program {
    /// Force the standard library on even when the project disabled it.
    force_std: bool,
    /// Skip leak checking for a fixture that leaks on purpose.
    allow_leak: bool,
  },
  /// Compile only and expect failure; the baseline holds the reported diagnostics.
  Diagnostics,
  /// Compile only and expect success with warnings; the baseline holds the reported warnings.
  Warnings,
}

/// One discovered fixture file and the mode its header selected.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct FixtureCase {
  /// Plan entry name, for example `e2e::arithmetic::add`.
  pub name: String,

  /// Absolute path to the fixture source.
  pub path: PathBuf,

  /// Parsed header, or the reason the header could not be understood.
  pub mode: Result<FixtureMode, String>,
}

impl FixtureCase {
  /// Absolute path of the baseline this fixture compares against.
  pub fn snapshot_path(&self) -> PathBuf {
    let directory = self.path.parent().unwrap_or_else(|| Path::new("."));
    let stem = self.path.file_stem().unwrap_or_default();

    directory
      .join(SNAPSHOT_DIR_NAME)
      .join(stem)
      .with_extension(SNAPSHOT_EXTENSION)
  }
}

/// Collects every fixture under `fixture_dirs`, in directory order and then by path.
///
/// A directory that does not exist contributes nothing: a project may declare a
/// corpus that only some checkouts contain, and that must not fail the run.
pub(crate) fn discover_fixture_cases(fixture_dirs: &[PathBuf]) -> Vec<FixtureCase> {
  let mut cases = Vec::new();

  for fixture_dir in fixture_dirs {
    let mut sources = Vec::new();
    collect_fixture_sources(fixture_dir, &mut sources);
    sources.sort();

    for source_path in sources {
      let Some(name) = fixture_case_name(fixture_dir, &source_path) else {
        continue;
      };

      let mode = match std::fs::read_to_string(&source_path) {
        Ok(source) => parse_fixture_header(&source),
        Err(error) => Err(format!("Failed to read fixture '{}': {}", source_path.display(), error)),
      };

      cases.push(FixtureCase {
        name,
        path: source_path,
        mode,
      });
    }
  }

  cases
}

/// Recursively gathers `.ign` files, skipping snapshot directories.
fn collect_fixture_sources(
  current_dir: &Path,
  discovered: &mut Vec<PathBuf>,
) {
  let Ok(entries) = std::fs::read_dir(current_dir) else {
    return;
  };

  let mut entry_paths: Vec<PathBuf> = entries
    .filter_map(|entry| entry.ok().map(|entry| entry.path()))
    .collect();
  entry_paths.sort();

  for entry_path in entry_paths {
    if entry_path.is_dir() {
      if entry_path.file_name().and_then(|name| name.to_str()) == Some(SNAPSHOT_DIR_NAME) {
        continue;
      }

      collect_fixture_sources(&entry_path, discovered);
      continue;
    }

    if entry_path.extension().and_then(|extension| extension.to_str()) == Some("ign") {
      discovered.push(entry_path);
    }
  }
}

/// Builds the plan entry name from the fixture's path relative to its directory.
fn fixture_case_name(
  fixture_dir: &Path,
  source_path: &Path,
) -> Option<String> {
  let relative = source_path.strip_prefix(fixture_dir).ok()?.with_extension("");
  let mut segments = vec![FIXTURE_NAME_PREFIX.to_string()];

  for component in relative.components() {
    segments.push(component.as_os_str().to_str()?.to_string());
  }

  Some(segments.join("::"))
}

/// Reads the fixture mode from the leading `// e2e: <option>` comment lines.
///
/// Scanning stops at the first line that is neither blank nor a comment, so a
/// fixture cannot change its mode from the middle of the file.
pub(crate) fn parse_fixture_header(source: &str) -> Result<FixtureMode, String> {
  let mut force_std = false;
  let mut allow_leak = false;
  let mut diagnostics = false;
  let mut warnings = false;

  for line in source.lines() {
    let trimmed = line.trim();

    if trimmed.is_empty() {
      continue;
    }

    if !trimmed.starts_with("//") {
      break;
    }

    let Some(options) = trimmed.strip_prefix(HEADER_MARKER) else {
      continue;
    };

    for option in options.split(',') {
      match option.trim() {
        "" => continue,
        "std" => force_std = true,
        "allow-leak" => allow_leak = true,
        "err" => diagnostics = true,
        "warn" => warnings = true,
        unknown => return Err(format!("Unknown fixture header option '{}'", unknown)),
      }
    }
  }

  if diagnostics && warnings {
    return Err("Fixture header option 'warn' cannot be combined with 'std', 'allow-leak' or 'err'".to_string());
  }

  if diagnostics {
    if force_std || allow_leak {
      return Err("Fixture header option 'err' cannot be combined with 'std' or 'allow-leak'".to_string());
    }

    return Ok(FixtureMode::Diagnostics);
  }

  if warnings {
    if force_std || allow_leak {
      return Err("Fixture header option 'warn' cannot be combined with 'std' or 'allow-leak'".to_string());
    }

    return Ok(FixtureMode::Warnings);
  }

  Ok(FixtureMode::Program { force_std, allow_leak })
}

/// Renders the baseline body of a program fixture.
///
/// The layout matches the one the migrated `e2e_ok` insta snapshots used, so a
/// migrated case keeps the exact expectation it had as a Rust test.
pub(crate) fn format_program_snapshot(
  exit_code: i32,
  stdout: &str,
  stderr: &str,
) -> String {
  format!(
    "exit_code: {}\nstdout: {}\nstderr: {}",
    exit_code,
    display_stream(stdout),
    display_stream(stderr)
  )
}

fn display_stream(stream: &str) -> &str {
  if stream.is_empty() {
    "(empty)"
  } else {
    stream.trim_end()
  }
}

/// Splits stderr into the program's own output and the LeakSanitizer report.
///
/// The report starts at the `==<pid>==ERROR: LeakSanitizer:` line and runs to
/// the end of the stream.
pub(crate) fn split_leak_report(stderr: &str) -> (String, String) {
  const LEAK_HEADER: &str = "ERROR: LeakSanitizer:";

  if !stderr.contains(LEAK_HEADER) {
    return (stderr.to_string(), String::new());
  }

  let mut program_lines = Vec::new();
  let mut leak_lines = Vec::new();
  let mut in_leak_report = false;

  for line in stderr.lines() {
    if !in_leak_report && line.starts_with("==") && line.contains(LEAK_HEADER) {
      in_leak_report = true;
    }

    if in_leak_report {
      leak_lines.push(line);
    } else {
      program_lines.push(line);
    }
  }

  (program_lines.join("\n"), leak_lines.join("\n"))
}

/// Renders a single-hunk unified diff between a baseline and an observed body.
///
/// Snapshot bodies are small, so one hunk covering the whole file reads better
/// than context windows and stays deterministic.
pub(crate) fn unified_diff(
  snapshot_path: &Path,
  expected: &str,
  actual: &str,
) -> String {
  let expected_lines: Vec<&str> = expected.lines().collect();
  let actual_lines: Vec<&str> = actual.lines().collect();

  let mut diff = vec![
    format!("--- expected: {}", snapshot_path.display()),
    "+++ actual".to_string(),
    format!("@@ -1,{} +1,{} @@", expected_lines.len().max(1), actual_lines.len().max(1)),
  ];

  for edit in diff_lines(&expected_lines, &actual_lines) {
    diff.push(edit);
  }

  diff.join("\n")
}

/// Line-level diff driven by a longest-common-subsequence table.
fn diff_lines(
  expected: &[&str],
  actual: &[&str],
) -> Vec<String> {
  // The table is quadratic in the line counts, which is fine for snapshot
  // bodies but not for an accidental multi-megabyte output.
  const MAX_DIFF_LINES: usize = 2000;

  if expected.len() > MAX_DIFF_LINES || actual.len() > MAX_DIFF_LINES {
    let mut edits: Vec<String> = expected.iter().map(|line| format!("-{}", line)).collect();
    edits.extend(actual.iter().map(|line| format!("+{}", line)));
    return edits;
  }

  let mut common = vec![vec![0usize; actual.len() + 1]; expected.len() + 1];

  for expected_index in (0..expected.len()).rev() {
    for actual_index in (0..actual.len()).rev() {
      common[expected_index][actual_index] = if expected[expected_index] == actual[actual_index] {
        common[expected_index + 1][actual_index + 1] + 1
      } else {
        common[expected_index + 1][actual_index].max(common[expected_index][actual_index + 1])
      };
    }
  }

  let mut edits = Vec::new();
  let mut expected_index = 0;
  let mut actual_index = 0;

  while expected_index < expected.len() && actual_index < actual.len() {
    if expected[expected_index] == actual[actual_index] {
      edits.push(format!(" {}", expected[expected_index]));
      expected_index += 1;
      actual_index += 1;
    } else if common[expected_index + 1][actual_index] >= common[expected_index][actual_index + 1] {
      edits.push(format!("-{}", expected[expected_index]));
      expected_index += 1;
    } else {
      edits.push(format!("+{}", actual[actual_index]));
      actual_index += 1;
    }
  }

  while expected_index < expected.len() {
    edits.push(format!("-{}", expected[expected_index]));
    expected_index += 1;
  }

  while actual_index < actual.len() {
    edits.push(format!("+{}", actual[actual_index]));
    actual_index += 1;
  }

  edits
}

/// Compares an observed body against the fixture's baseline.
///
/// With `update_snapshots` on, a missing or differing baseline is rewritten and
/// the case passes, which mirrors what `Test::assertSnapshot` does for `@test`
/// snapshots under `--update-snapshots`.
pub(crate) fn compare_snapshot(
  snapshot_path: &Path,
  actual: &str,
  update_snapshots: bool,
) -> Result<(), String> {
  if !snapshot_path.exists() {
    if update_snapshots {
      return write_snapshot(snapshot_path, actual);
    }

    return Err(format!("snapshot missing: {}\nobserved:\n{}", snapshot_path.display(), actual));
  }

  let expected = std::fs::read_to_string(snapshot_path)
    .map_err(|error| format!("Failed to read snapshot '{}': {}", snapshot_path.display(), error))?;

  if expected == actual {
    return Ok(());
  }

  if update_snapshots {
    return write_snapshot(snapshot_path, actual);
  }

  Err(format!(
    "snapshot mismatch: {}\n{}",
    snapshot_path.display(),
    unified_diff(snapshot_path, &expected, actual)
  ))
}

fn write_snapshot(
  snapshot_path: &Path,
  actual: &str,
) -> Result<(), String> {
  if let Some(parent) = snapshot_path.parent() {
    std::fs::create_dir_all(parent)
      .map_err(|error| format!("Failed to create snapshot directory '{}': {}", parent.display(), error))?;
  }

  std::fs::write(snapshot_path, actual)
    .map_err(|error| format!("Failed to write snapshot '{}': {}", snapshot_path.display(), error))
}

/// Creates every snapshot directory the selected fixtures may write into.
///
/// Two fixtures in one directory would otherwise race on the same lazily
/// created directory when the pool runs them concurrently.
pub(crate) fn prepare_fixture_snapshot_directories(cases: &[FixtureCase]) -> Result<(), String> {
  let mut prepared = std::collections::HashSet::new();

  for case in cases {
    let snapshot_path = case.snapshot_path();
    let Some(snapshot_dir) = snapshot_path.parent() else {
      continue;
    };

    if !prepared.insert(snapshot_dir.to_path_buf()) {
      continue;
    }

    std::fs::create_dir_all(snapshot_dir)
      .map_err(|error| format!("Failed to create snapshot directory '{}': {}", snapshot_dir.display(), error))?;
  }

  Ok(())
}

#[cfg(test)]
mod tests {
  use super::*;

  fn write_fixture(
    root: &Path,
    relative_path: &str,
    source: &str,
  ) {
    let path = root.join(relative_path);
    std::fs::create_dir_all(path.parent().expect("fixture parent")).expect("create fixture dir");
    std::fs::write(path, source).expect("write fixture");
  }

  #[test]
  fn fixtures_are_discovered_recursively_in_path_order_per_directory() {
    let temp_dir = tempfile::TempDir::new().expect("temporary fixture root");
    let root = temp_dir.path();

    write_fixture(root, "ok/zebra.ign", "function main(): void {}");
    write_fixture(root, "ok/alpha.ign", "function main(): void {}");
    write_fixture(root, "ok/nested/deeper.ign", "function main(): void {}");
    write_fixture(root, "ok/notes.txt", "ignored");
    write_fixture(root, "ok/__snapshots__/alpha.snap", "ignored");
    write_fixture(root, "err/broken.ign", "// e2e: err\nfunction main(): i32 {}");

    let cases = discover_fixture_cases(&[root.join("ok"), root.join("err")]);
    let names: Vec<&str> = cases.iter().map(|case| case.name.as_str()).collect();

    assert_eq!(names, vec!["e2e::alpha", "e2e::nested::deeper", "e2e::zebra", "e2e::broken"]);
    assert_eq!(cases[3].mode, Ok(FixtureMode::Diagnostics));
  }

  #[test]
  fn a_missing_fixture_directory_contributes_nothing() {
    let temp_dir = tempfile::TempDir::new().expect("temporary fixture root");

    assert!(discover_fixture_cases(&[temp_dir.path().join("absent")]).is_empty());
  }

  #[test]
  fn a_baseline_sits_in_a_snapshot_directory_next_to_its_fixture() {
    let case = FixtureCase {
      name: "e2e::nested::deeper".to_string(),
      path: PathBuf::from("/corpus/ok/nested/deeper.ign"),
      mode: Ok(FixtureMode::Program {
        force_std: false,
        allow_leak: false,
      }),
    };

    assert_eq!(
      case.snapshot_path(),
      PathBuf::from("/corpus/ok/nested/__snapshots__/deeper.snap")
    );
  }

  #[test]
  fn a_fixture_without_a_header_is_a_plain_program() {
    let mode = parse_fixture_header("function main(): i32 { return 0; }").expect("header");

    assert_eq!(
      mode,
      FixtureMode::Program {
        force_std: false,
        allow_leak: false,
      }
    );
  }

  #[test]
  fn header_options_accumulate_across_leading_comment_lines() {
    let mode = parse_fixture_header("// e2e: std\n// e2e: allow-leak\nfunction main(): void {}").expect("header");

    assert_eq!(
      mode,
      FixtureMode::Program {
        force_std: true,
        allow_leak: true,
      }
    );
  }

  #[test]
  fn the_err_option_selects_diagnostics_mode() {
    let mode = parse_fixture_header("// e2e: err\nfunction main(): i32 {}").expect("header");

    assert_eq!(mode, FixtureMode::Diagnostics);
  }

  #[test]
  fn the_warn_option_selects_warnings_mode() {
    let mode = parse_fixture_header("// e2e: warn\nfunction main(): void {}").expect("header");

    assert_eq!(mode, FixtureMode::Warnings);
  }

  #[test]
  fn the_warn_option_cannot_combine_with_std_or_allow_leak() {
    let error = parse_fixture_header("// e2e: warn, std\nfunction main(): void {}").expect_err("conflict");

    assert!(error.contains("warn"), "expected 'warn' in the error, got: {}", error);
  }

  #[test]
  fn the_warn_option_cannot_combine_with_err() {
    let error = parse_fixture_header("// e2e: warn, err\nfunction main(): i32 {}").expect_err("conflict");

    assert!(error.contains("warn"), "expected 'warn' in the error, got: {}", error);
  }

  #[test]
  fn a_header_after_the_first_code_line_is_ignored() {
    let mode = parse_fixture_header("function main(): void {}\n// e2e: err").expect("header");

    assert_eq!(
      mode,
      FixtureMode::Program {
        force_std: false,
        allow_leak: false,
      }
    );
  }

  #[test]
  fn an_unknown_header_option_is_rejected() {
    let error = parse_fixture_header("// e2e: nope\n").expect_err("unknown option");

    assert!(error.contains("nope"), "expected the option in the error, got: {}", error);
  }

  #[test]
  fn empty_streams_render_as_the_empty_marker() {
    assert_eq!(
      format_program_snapshot(0, "", ""),
      "exit_code: 0\nstdout: (empty)\nstderr: (empty)"
    );
  }

  #[test]
  fn trailing_newlines_are_trimmed_from_rendered_streams() {
    assert_eq!(
      format_program_snapshot(3, "dropped\ndropped\n", ""),
      "exit_code: 3\nstdout: dropped\ndropped\nstderr: (empty)"
    );
  }

  #[test]
  fn a_leak_report_is_split_away_from_program_output() {
    let (program, leak) = split_leak_report("hello\n==12==ERROR: LeakSanitizer: detected memory leaks\ndetail");

    assert_eq!(program, "hello");
    assert_eq!(leak, "==12==ERROR: LeakSanitizer: detected memory leaks\ndetail");
  }

  #[test]
  fn a_diff_marks_only_the_changed_line() {
    let diff = unified_diff(
      Path::new("/tmp/case.snap"),
      "exit_code: 0\nstdout: a\nstderr: (empty)",
      "exit_code: 0\nstdout: b\nstderr: (empty)",
    );

    assert_eq!(
      diff,
      "--- expected: /tmp/case.snap\n+++ actual\n@@ -1,3 +1,3 @@\n exit_code: 0\n-stdout: a\n+stdout: b\n stderr: (empty)"
    );
  }
}
