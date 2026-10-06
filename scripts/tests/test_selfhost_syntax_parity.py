#!/usr/bin/env python3
"""Exercises scripts/selfhost_syntax_parity.py (gate G6) without a real compiler.

About two hundred G6 cases are the former host parser unit-test snippets,
committed under `test_cases/parser/host_unit_tests/`; the corpus test pins that
they are read as ordinary committed files.

The baseline tests cover the gate itself: comparing against committed parse
verdicts, writing baselines and checking their coverage. They drive a fake
compiler, a shell script that prints exactly the lines the harness parses, so
no real compiler runs.

Usage: python3 scripts/tests/test_selfhost_syntax_parity.py
"""

import contextlib
import io
import os
import stat
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

SCRIPT_DIR = Path(__file__).resolve().parent.parent
PARITY_PY = SCRIPT_DIR / "selfhost_syntax_parity.py"

sys.path.insert(0, str(SCRIPT_DIR))

from selfhost_syntax_parity import (  # noqa: E402
  CLASS_BASELINE_MALFORMED,
  CLASS_BASELINE_MISSING,
  CLASS_PASS,
  CLASS_SELFHOST_ACCEPTS,
  CLASS_SELFHOST_CRASH,
  CLASS_SELFHOST_REJECTS,
  ORIGIN_PARSER_TEST,
  ORIGIN_REPOSITORY,
  PARSER_TEST_DIR,
  Case,
  Settings,
  collect_repository_cases,
  evaluate,
  format_baseline,
  run_case,
  run_coverage_check,
  write_baselines,
)

class CorpusTests(unittest.TestCase):
  def test_committed_parser_snippets_join_the_corpus_as_parser_test_cases(self) -> None:
    with tempfile.TemporaryDirectory() as directory:
      repository_root = Path(directory)
      snippet = repository_root / PARSER_TEST_DIR / "expression__parses_addition.ign"
      snippet.parent.mkdir(parents=True)
      snippet.write_bytes(b"function test(): void { 1 + 2; }")
      ordinary = repository_root / "test_cases/e2e/ok/plain.ign"
      ordinary.parent.mkdir(parents=True)
      ordinary.write_text("function main(): void { }", encoding="utf-8")

      origins = {case.name: case.origin for case in collect_repository_cases(repository_root)}

    self.assertEqual(
      origins,
      {
        "test_cases_e2e_ok_plain": ORIGIN_REPOSITORY,
        "test_cases_parser_host_unit_tests_expression_parses_addition": ORIGIN_PARSER_TEST,
      },
    )


# A fake selfhost: like the real one it prints the phase report the harness
# reads only under `--verbose`, on stderr. It rejects a source that mentions
# SYNTAX_ERROR or SELFHOST_REJECTS and prints nothing at all (no parse
# verdict) for one that mentions SELFHOST_CRASH.
FAKE_SELFHOST = """#!/bin/sh
case_file="$1"

case " $* " in
  *" --verbose "*) ;;
  *) exit 0 ;;
esac

if grep -q SELFHOST_CRASH "$case_file"; then
  exit 0
fi

if grep -q -e SYNTAX_ERROR -e SELFHOST_REJECTS "$case_file"; then
  echo "Error[I0021]: expected an expression" >&2
  echo "  --> $case_file:1:1" >&2
  echo "  parse: fail (1 errors, 0 warnings)" >&2
  exit 1
fi

echo "  lex  : ok (0 errors, 0 warnings)" >&2
echo "  parse: ok (0 errors, 0 warnings)" >&2
"""

def write_executable(
  path: Path,
  content: str,
) -> Path:
  path.write_text(content, encoding="utf-8")
  path.chmod(path.stat().st_mode | stat.S_IXUSR | stat.S_IXGRP | stat.S_IXOTH)

  return path


def make_case(
  name: str,
  source: str,
) -> Case:
  return Case(name=name, origin=ORIGIN_REPOSITORY, source=source, location=f"test_cases/{name}.ign")


GOOD = make_case("good", "function main(): void { }")
BAD = make_case("bad", "function main(): void { SYNTAX_ERROR }")


class BaselineHarness(unittest.TestCase):
  """A throwaway repository root with fake compilers and an empty baseline directory."""

  def setUp(self) -> None:
    self._tmp = tempfile.TemporaryDirectory()
    self.addCleanup(self._tmp.cleanup)
    self.root = Path(self._tmp.name)
    self.baseline_dir = self.root / "test_cases/__parse_verdicts__"
    (self.root / "std").mkdir()
    (self.root / "bin").mkdir()
    self.selfhost = write_executable(self.root / "bin/selfhost", FAKE_SELFHOST)

  def settings(self) -> Settings:
    return Settings(
      compiler=self.selfhost,
      std_path=self.root / "std",
      repository_root=self.root,
      baseline_dir=self.baseline_dir,
      work_dir=self.root / "work",
    )

  def write_baseline(
    self,
    case: Case,
    content: str,
  ) -> Path:
    self.baseline_dir.mkdir(parents=True, exist_ok=True)
    path = self.baseline_dir / f"{case.name}.txt"
    path.write_text(content, encoding="utf-8")

    return path

  def write_orphan(self) -> Path:
    return self.write_baseline(make_case("retired", ""), format_baseline(True))

  def quietly(
    self,
    function,
    *arguments,
    **keywords,
  ):
    """Call `function` with stdout and stderr captured; return (result, stdout, stderr)."""
    stdout = io.StringIO()
    stderr = io.StringIO()

    with contextlib.redirect_stdout(stdout), contextlib.redirect_stderr(stderr):
      result = function(*arguments, **keywords)

    return result, stdout.getvalue(), stderr.getvalue()


class BaselineCompareTests(BaselineHarness):
  def test_matching_verdicts_pass(self) -> None:
    self.write_baseline(GOOD, format_baseline(True))
    self.write_baseline(BAD, format_baseline(False))

    results, counts, stale, gate = evaluate(self.settings(), [GOOD, BAD], jobs=2, filtered=False)

    self.assertEqual([result.classification for result in results], [CLASS_PASS, CLASS_PASS])
    self.assertEqual(counts[CLASS_PASS], 2)
    self.assertEqual(stale, [])
    self.assertEqual(gate["status"], "pass")
    self.assertEqual(gate["gate"], "G6")
    self.assertEqual(gate["details"]["compared_against"], "baseline")
    self.assertNotIn("host_cross_check", gate["details"])

  def test_the_gate_id_is_configurable(self) -> None:
    self.write_baseline(GOOD, format_baseline(True))
    settings = self.settings()
    settings.gate_id = "G6-STAGE1"

    _, _, _, gate = evaluate(settings, [GOOD], jobs=1, filtered=False)

    self.assertEqual(gate["gate"], "G6-STAGE1")

  def test_a_selfhost_rejecting_an_accepted_baseline_fails(self) -> None:
    case = make_case("rejected_by_selfhost", "function main(): void { SELFHOST_REJECTS }")
    self.write_baseline(case, format_baseline(True))

    result = run_case(case, self.settings())

    self.assertEqual(result.classification, CLASS_SELFHOST_REJECTS)
    self.assertIn("I0021", result.reason)

  def test_a_selfhost_accepting_a_rejected_baseline_fails(self) -> None:
    self.write_baseline(GOOD, format_baseline(False))

    result = run_case(GOOD, self.settings())

    self.assertEqual(result.classification, CLASS_SELFHOST_ACCEPTS)

  def test_a_case_without_a_baseline_fails_without_running_the_selfhost(self) -> None:
    results, counts, _, gate = evaluate(self.settings(), [GOOD], jobs=1, filtered=False)

    self.assertEqual(results[0].classification, CLASS_BASELINE_MISSING)
    self.assertIsNone(results[0].selfhost)
    self.assertEqual(counts[CLASS_BASELINE_MISSING], 1)
    self.assertEqual(gate["status"], "fail")

  def test_a_malformed_baseline_fails(self) -> None:
    malformed = {
      "no_trailing_newline": "parse-verdict v1\naccepted",
      "wrong_header": "parse-verdict v2\naccepted\n",
      "unknown_verdict": "parse-verdict v1\nmaybe\n",
      "empty": "",
    }

    for name, content in malformed.items():
      with self.subTest(name=name):
        case = make_case(name, "function main(): void { }")
        self.write_baseline(case, content)

        results, _, _, gate = evaluate(self.settings(), [case], jobs=1, filtered=False)

        self.assertEqual(results[0].classification, CLASS_BASELINE_MALFORMED)
        self.assertEqual(gate["status"], "fail")

  def test_a_selfhost_without_a_verdict_is_a_crash(self) -> None:
    case = make_case("crashes", "SELFHOST_CRASH")
    self.write_baseline(case, format_baseline(True))

    result = run_case(case, self.settings())

    self.assertEqual(result.classification, CLASS_SELFHOST_CRASH)

  def test_an_orphaned_baseline_fails_the_gate(self) -> None:
    self.write_baseline(GOOD, format_baseline(True))
    self.write_orphan()

    results, _, stale, gate = evaluate(self.settings(), [GOOD], jobs=1, filtered=False)

    self.assertEqual(results[0].classification, CLASS_PASS)
    self.assertEqual(stale, ["test_cases/__parse_verdicts__/retired.txt"])
    self.assertEqual(gate["status"], "fail")
    self.assertEqual(gate["details"]["stale_baselines"], stale)

  def test_an_orphaned_baseline_is_not_computed_under_a_filter(self) -> None:
    self.write_baseline(GOOD, format_baseline(True))
    self.write_orphan()

    _, _, stale, gate = evaluate(self.settings(), [GOOD], jobs=1, filtered=True)

    self.assertEqual(stale, [])
    self.assertEqual(gate["status"], "pass")


class WriteBaselineTests(BaselineHarness):
  def test_writes_one_verdict_per_case_and_prunes_orphans(self) -> None:
    self.write_orphan()

    status, stdout, _ = self.quietly(write_baselines, self.settings(), [GOOD, BAD], 2, filtered=False)

    self.assertEqual(status, 0)
    self.assertEqual((self.baseline_dir / "good.txt").read_text(encoding="utf-8"), "parse-verdict v1\naccepted\n")
    self.assertEqual((self.baseline_dir / "bad.txt").read_text(encoding="utf-8"), "parse-verdict v1\nrejected\n")
    self.assertFalse((self.baseline_dir / "retired.txt").exists())
    self.assertIn("wrote 2 baselines", stdout)
    self.assertIn("removed 1 orphaned", stdout)

  def test_writes_nothing_when_any_case_has_no_verdict(self) -> None:
    existing = self.write_baseline(GOOD, format_baseline(False))
    crash = make_case("crashes", "SELFHOST_CRASH")

    status, _, stderr = self.quietly(write_baselines, self.settings(), [GOOD, crash], 2, filtered=False)

    self.assertEqual(status, 1)
    self.assertEqual(existing.read_text(encoding="utf-8"), format_baseline(False))
    self.assertFalse((self.baseline_dir / "crashes.txt").exists())
    self.assertIn("crashes", stderr)

  def test_a_filtered_write_never_prunes(self) -> None:
    orphan = self.write_orphan()

    status, stdout, _ = self.quietly(write_baselines, self.settings(), [GOOD], 1, filtered=True)

    self.assertEqual(status, 0)
    self.assertTrue(orphan.exists())
    self.assertTrue((self.baseline_dir / "good.txt").exists())
    self.assertIn("kept every other baseline", stdout)


class CoverageCheckTests(BaselineHarness):
  def check(
    self,
    cases: list[Case],
  ) -> tuple[int, str]:
    status, _, stderr = self.quietly(run_coverage_check, self.settings(), cases)

    return status, stderr

  def test_passes_when_every_case_has_a_valid_baseline(self) -> None:
    self.write_baseline(GOOD, format_baseline(True))
    self.write_baseline(BAD, format_baseline(False))

    status, _ = self.check([GOOD, BAD])

    self.assertEqual(status, 0)

  def test_fails_on_a_missing_baseline(self) -> None:
    self.write_baseline(GOOD, format_baseline(True))

    status, stderr = self.check([GOOD, BAD])

    self.assertEqual(status, 1)
    self.assertIn("missing baseline: test_cases/__parse_verdicts__/bad.txt", stderr)

  def test_fails_on_an_orphaned_baseline(self) -> None:
    self.write_baseline(GOOD, format_baseline(True))
    self.write_orphan()

    status, stderr = self.check([GOOD])

    self.assertEqual(status, 1)
    self.assertIn("orphaned baseline: test_cases/__parse_verdicts__/retired.txt", stderr)

  def test_fails_on_a_malformed_baseline(self) -> None:
    self.write_baseline(GOOD, "parse-verdict v1\nACCEPTED\n")

    status, stderr = self.check([GOOD])

    self.assertEqual(status, 1)
    self.assertIn("malformed baseline: test_cases/__parse_verdicts__/good.txt", stderr)

  def test_fails_on_two_sources_sharing_a_case_name(self) -> None:
    twin = Case(name="good", origin=ORIGIN_REPOSITORY, source="", location="test_cases/go-od.ign")
    self.write_baseline(GOOD, format_baseline(True))

    status, stderr = self.check([GOOD, twin])

    self.assertEqual(status, 1)
    self.assertIn("duplicate case name: good: test_cases/good.ign, test_cases/go-od.ign", stderr)


class ArgumentTests(unittest.TestCase):
  """Invalid combinations are rejected by argparse before any case runs."""

  def run_script(
    self,
    *arguments: str,
  ) -> subprocess.CompletedProcess:
    return subprocess.run(
      [sys.executable, str(PARITY_PY), *arguments],
      capture_output=True,
      text=True,
      check=False,
      env={**os.environ, "PYTHONDONTWRITEBYTECODE": "1"},
    )

  def test_check_coverage_refuses_a_filter(self) -> None:
    completed = self.run_script("--check-coverage", "--filter", "good")

    self.assertEqual(completed.returncode, 2)
    self.assertIn("--filter does not apply", completed.stderr)

  def test_conflicting_arguments_exit_with_an_argparse_error(self) -> None:
    conflicts = {
      "two modes": ["--write-baselines", "--check-coverage"],
      "compare without a compiler": [],
      "write without a compiler": ["--write-baselines"],
    }

    for label, arguments in conflicts.items():
      with self.subTest(label=label):
        completed = self.run_script(*arguments)

        self.assertEqual(completed.returncode, 2, completed.stderr)
        self.assertIn("error:", completed.stderr)

  def test_the_host_and_parser_scrape_options_are_gone(self) -> None:
    removed = {
      "--host": ["--compiler", "c", "--host", "h"],
      "--host-compare": ["--compiler", "c", "--host-compare"],
      "--check-parser-tests": ["--check-parser-tests"],
      "--materialize-parser-tests": ["--materialize-parser-tests"],
    }

    for option, arguments in removed.items():
      with self.subTest(option=option):
        completed = self.run_script(*arguments)

        self.assertEqual(completed.returncode, 2, completed.stderr)
        self.assertIn(f"unrecognized arguments: {option}", completed.stderr)


if __name__ == "__main__":
  unittest.main()
