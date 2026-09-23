#!/usr/bin/env python3
"""Exercises scripts/selfhost_syntax_parity.py (gate G6) without a real compiler.

The host parser unit tests are the source of about two hundred G6 cases, and
the Rust files they live in go away when the host is frozen. Their snippets are
therefore committed under `test_cases/parser/host_unit_tests/`, and these tests
pin the two operations that keep that directory honest: materializing it from
the Rust sources, and detecting drift between the two. Both run against a
throwaway repository root holding a synthetic Rust test file, so nothing here
touches the real repository.

Usage: python3 scripts/tests/test_selfhost_syntax_parity.py
"""

import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

SCRIPT_DIR = Path(__file__).resolve().parent.parent
PARITY_PY = SCRIPT_DIR / "selfhost_syntax_parity.py"

sys.path.insert(0, str(SCRIPT_DIR))

from selfhost_syntax_parity import (  # noqa: E402
  ORIGIN_PARSER_TEST,
  ORIGIN_REPOSITORY,
  PARSER_TEST_DIR,
  collect_repository_cases,
  materialize_parser_tests,
  parser_test_drift,
)

SYNTHETIC_EXPRESSION_TESTS = r'''
fn parse_expr(source: &str) -> Node {
  todo()
}

#[test]
fn parses_addition() {
  parse_expr("1 + 2");
}

#[test]
fn parses_two_literals() {
  parse_expr("true");
  parse_expr(r#"
"text"
"#);
}
'''

EXPECTED_FILES = {
  "expression__parses_addition.ign": b"function test(): void { 1 + 2; }",
  "expression__parses_two_literals.ign": b"function test(): void { true; }",
  "expression__parses_two_literals__2.ign": b'function test(): void { \n"text"\n; }',
}


class ParserTestMaterializationTests(unittest.TestCase):
  def setUp(self) -> None:
    self._tmp = tempfile.TemporaryDirectory()
    self.addCleanup(self._tmp.cleanup)
    self.repository_root = Path(self._tmp.name)
    self.rust_file = self.repository_root / "crates/ignis_parser/src/parser/expression.rs"
    self.rust_file.parent.mkdir(parents=True)
    self.rust_file.write_text(SYNTHETIC_EXPRESSION_TESTS, encoding="utf-8")
    self.target_dir = self.repository_root / PARSER_TEST_DIR

  def committed_files(self) -> dict[str, bytes]:
    return {path.name: path.read_bytes() for path in self.target_dir.iterdir()}

  def test_materializes_one_wrapped_byte_exact_file_per_snippet(self) -> None:
    drift = materialize_parser_tests(self.repository_root, self.target_dir)

    self.assertEqual(sorted(drift.missing), sorted(EXPECTED_FILES))
    self.assertEqual(self.committed_files(), EXPECTED_FILES)

  def test_a_second_materialization_changes_nothing(self) -> None:
    materialize_parser_tests(self.repository_root, self.target_dir)
    before = {path.name: path.stat().st_mtime_ns for path in self.target_dir.iterdir()}

    drift = materialize_parser_tests(self.repository_root, self.target_dir)

    self.assertTrue(drift.is_empty(), drift)
    self.assertEqual({path.name: path.stat().st_mtime_ns for path in self.target_dir.iterdir()}, before)
    self.assertTrue(parser_test_drift(self.repository_root, self.target_dir).is_empty())

  def test_detects_missing_extra_and_differing_snippets(self) -> None:
    materialize_parser_tests(self.repository_root, self.target_dir)
    (self.target_dir / "expression__parses_addition.ign").unlink()
    (self.target_dir / "expression__retired.ign").write_bytes(b"function test(): void { }")
    (self.target_dir / "expression__parses_two_literals.ign").write_bytes(b"function test(): void { false; }")

    drift = parser_test_drift(self.repository_root, self.target_dir)

    self.assertEqual(drift.missing, ["expression__parses_addition.ign"])
    self.assertEqual(drift.extra, ["expression__retired.ign"])
    self.assertEqual(drift.differing, ["expression__parses_two_literals.ign"])

  def test_materializing_repairs_every_kind_of_drift(self) -> None:
    materialize_parser_tests(self.repository_root, self.target_dir)
    (self.target_dir / "expression__parses_addition.ign").unlink()
    (self.target_dir / "expression__retired.ign").write_bytes(b"function test(): void { }")
    (self.target_dir / "expression__parses_two_literals.ign").write_bytes(b"function test(): void { false; }")

    materialize_parser_tests(self.repository_root, self.target_dir)

    self.assertEqual(self.committed_files(), EXPECTED_FILES)

  def test_a_changed_rust_test_is_drift(self) -> None:
    materialize_parser_tests(self.repository_root, self.target_dir)
    self.rust_file.write_text(SYNTHETIC_EXPRESSION_TESTS.replace("1 + 2", "1 - 2"), encoding="utf-8")

    drift = parser_test_drift(self.repository_root, self.target_dir)

    self.assertEqual(drift.differing, ["expression__parses_addition.ign"])
    self.assertEqual(drift.missing, [])
    self.assertEqual(drift.extra, [])

  def test_materialized_snippets_join_the_corpus_as_parser_test_cases(self) -> None:
    materialize_parser_tests(self.repository_root, self.target_dir)
    ordinary = self.repository_root / "test_cases/e2e/ok/plain.ign"
    ordinary.parent.mkdir(parents=True)
    ordinary.write_text("function main(): void { }", encoding="utf-8")

    origins = {case.name: case.origin for case in collect_repository_cases(self.repository_root)}

    self.assertEqual(origins["test_cases_e2e_ok_plain"], ORIGIN_REPOSITORY)
    self.assertEqual(origins["test_cases_parser_host_unit_tests_expression_parses_addition"], ORIGIN_PARSER_TEST)
    self.assertEqual(len(origins), len(EXPECTED_FILES) + 1)


class RepositorySnippetTests(unittest.TestCase):
  """The committed snippets in this repository, checked the way CI checks them."""

  def test_the_committed_snippets_match_the_rust_sources(self) -> None:
    completed = subprocess.run(
      [sys.executable, str(PARITY_PY), "--check-parser-tests"],
      capture_output=True,
      text=True,
      check=False,
    )

    self.assertEqual(completed.returncode, 0, completed.stderr)


if __name__ == "__main__":
  unittest.main()
