#!/usr/bin/env python3
"""Exercises scripts/selfhost_doc_parity.py without a real compiler.

The pure parts are tested directly: which targets the script documents, how an
output is compared with its baseline, and the report it prints. The end-to-end
tests drive fake compilers, small Python scripts that answer `doc` the way the
real one does, printing a package or writing it through `--output`, against a
throwaway baseline directory.

Usage: python3 scripts/tests/test_selfhost_doc_parity.py
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
DOC_PARITY_PY = SCRIPT_DIR / "selfhost_doc_parity.py"

sys.path.insert(0, str(SCRIPT_DIR))

from selfhost_doc_parity import (  # noqa: E402
  BASELINE_DIR,
  CI_TARGET,
  FIXTURES,
  REPO_ROOT,
  RunResult,
  Target,
  baseline_name,
  compare,
  diff_text,
  fixture_targets,
  format_report,
  main,
  run_doc,
  std_targets,
)

# Prints `{"entry": "<argument>"}` plus the suffix it was built with, or writes
# it to the file `--output` names without a trailing newline, as `ignis doc`
# does. An argument naming FAIL_ON makes it exit 1 with nothing written.
FAKE_COMPILER = """#!/usr/bin/env python3
import sys

arguments = sys.argv[1:]

if arguments[1] == "{fail_on}":
  print("error: cannot document", file=sys.stderr)
  sys.exit(1)

payload = '{{"entry": "' + arguments[1] + '"}}{suffix}'

if "--output" in arguments:
  with open(arguments[arguments.index("--output") + 1], "w") as handle:
    handle.write(payload)
else:
  print(payload)
"""


def write_fake_compiler(directory: Path, name: str, suffix: str = "", fail_on: str = "") -> str:
  path = directory / name
  path.write_text(FAKE_COMPILER.format(suffix=suffix, fail_on=fail_on), encoding="utf-8")
  path.chmod(path.stat().st_mode | stat.S_IXUSR)
  return str(path)


def run_main(*argv: str) -> tuple[int, str]:
  with contextlib.redirect_stdout(io.StringIO()) as output, contextlib.redirect_stderr(io.StringIO()):
    code = main(list(argv))

  return code, output.getvalue()


class TargetTests(unittest.TestCase):
  def test_the_workflow_target_comes_first_and_writes_through_output(self):
    targets = std_targets(REPO_ROOT)

    self.assertEqual(targets[0].argument, CI_TARGET)
    self.assertTrue(targets[0].output_file)

  def test_the_published_entry_is_the_only_std_target(self):
    self.assertEqual([target.argument for target in std_targets(REPO_ROOT)], [CI_TARGET])

  def test_fixtures_are_written_under_their_module_name(self):
    with tempfile.TemporaryDirectory() as directory:
      workdir = Path(directory)
      targets = fixture_targets(workdir)

      self.assertEqual([target.argument for target in targets], [f"{name}.ign" for name in FIXTURES])

      for name, source in FIXTURES.items():
        self.assertEqual((workdir / f"{name}.ign").read_text(encoding="utf-8"), source)

  def test_the_former_host_test_fixtures_are_all_present(self):
    self.assertEqual(
      sorted(FIXTURES),
      ["adds", "counter", "described", "helper", "math", "outcome", "written"],
    )

  def test_every_target_has_its_own_baseline_name(self):
    with tempfile.TemporaryDirectory() as directory:
      targets = std_targets(REPO_ROOT) + fixture_targets(Path(directory))

    names = [baseline_name(target) for target in targets]

    self.assertEqual(len(names), len(set(names)))
    self.assertEqual(names[0], "output__std_io_mod.json")
    self.assertEqual(names[1:], [f"fixture_{name}.json" for name in FIXTURES])

  def test_the_committed_baselines_cover_exactly_the_targets(self):
    with tempfile.TemporaryDirectory() as directory:
      targets = std_targets(REPO_ROOT) + fixture_targets(Path(directory))

    committed = sorted(path.name for path in (REPO_ROOT / BASELINE_DIR).glob("*.json"))

    self.assertEqual(committed, sorted(baseline_name(target) for target in targets))


class CompareTests(unittest.TestCase):
  def test_same_bytes_and_a_zero_exit_code_are_identical(self):
    result = RunResult(exit_code=0, payload=b"{}\n", stderr="")

    self.assertTrue(compare("t", b"{}\n", result, 10).identical)

  def test_a_non_zero_exit_code_is_reported_with_the_selfhost_stderr(self):
    selfhost = RunResult(exit_code=1, payload=b"", stderr="error: boom\n")

    comparison = compare("t", b"{}\n", selfhost, 10)

    self.assertFalse(comparison.identical)
    self.assertIn("exit code: selfhost 1, baseline recorded 0", comparison.detail)
    self.assertIn("error: boom", comparison.detail)

  def test_a_missing_baseline_is_different(self):
    comparison = compare("t", None, RunResult(exit_code=0, payload=b"{}\n", stderr=""), 10)

    self.assertFalse(comparison.identical)
    self.assertIn("no baseline", comparison.detail)

  def test_a_changed_line_shows_in_the_diff(self):
    selfhost = RunResult(exit_code=0, payload=b'{\n  "a": 2\n}\n', stderr="")

    comparison = compare("t", b'{\n  "a": 1\n}\n', selfhost, 10)

    self.assertFalse(comparison.identical)
    self.assertIn('-  "a": 1', comparison.detail)
    self.assertIn('+  "a": 2', comparison.detail)

  def test_a_long_diff_is_cut(self):
    baseline = "".join(f"{index}\n" for index in range(50)).encode()
    selfhost = "".join(f"x{index}\n" for index in range(50)).encode()

    text = diff_text(baseline, selfhost, 5)

    self.assertEqual(len(text.splitlines()), 6)
    self.assertIn("more diff lines", text.splitlines()[-1])

  def test_a_trailing_newline_alone_is_reported_by_length(self):
    self.assertEqual(
      diff_text(b"{}", b"{}\n", 10),
      "same lines, different bytes: baseline 2 bytes, selfhost 3 bytes",
    )

  def test_the_report_lists_every_target_and_a_summary(self):
    same = RunResult(exit_code=0, payload=b"a\n", stderr="")
    other = RunResult(exit_code=0, payload=b"b\n", stderr="")
    report = format_report([compare("one", b"a\n", same, 10), compare("two", b"a\n", other, 10)])
    lines = report.splitlines()

    self.assertEqual(lines[0], "identical: one")
    self.assertEqual(lines[1], "different: two")
    self.assertTrue(lines[2].startswith("    "))
    self.assertEqual(lines[-1], "summary: 1 identical, 1 different")


class EndToEndTests(unittest.TestCase):
  def setUp(self):
    self._tmp = tempfile.TemporaryDirectory()
    self.addCleanup(self._tmp.cleanup)
    self.workdir = Path(self._tmp.name)
    self.baselines = self.workdir / "baselines"

  def test_run_doc_reads_the_output_file_for_an_output_target(self):
    compiler = write_fake_compiler(self.workdir, "fake")
    target = Target(label="t", cwd=self.workdir, argument="a.ign", output_file=True)

    result = run_doc(compiler, target, self.workdir, self.workdir / "out.json")

    self.assertEqual(result.exit_code, 0)
    self.assertEqual(result.payload, b'{"entry": "a.ign"}')

  def test_written_baselines_hold_each_output_byte_for_byte(self):
    compiler = write_fake_compiler(self.workdir, "fake")

    code, _ = run_main("--compiler", compiler, "--baselines", str(self.baselines), "--write-baselines")

    self.assertEqual(code, 0)
    self.assertEqual((self.baselines / "output__std_io_mod.json").read_bytes(), b'{"entry": "std/io/mod.ign"}')
    self.assertEqual((self.baselines / "fixture_adds.json").read_bytes(), b'{"entry": "adds.ign"}\n')

  def test_a_compiler_matching_the_baselines_exits_zero(self):
    compiler = write_fake_compiler(self.workdir, "fake")
    run_main("--compiler", compiler, "--baselines", str(self.baselines), "--write-baselines")

    code, output = run_main("--compiler", compiler, "--baselines", str(self.baselines))

    self.assertEqual(code, 0, output)
    self.assertNotIn("different:", output)
    self.assertIn(f"identical: fixture {next(iter(FIXTURES))}", output)

  def test_a_differing_compiler_exits_one_with_a_diff(self):
    recorded = write_fake_compiler(self.workdir, "recorded")
    selfhost = write_fake_compiler(self.workdir, "selfhost", suffix=" ")
    run_main("--compiler", recorded, "--baselines", str(self.baselines), "--write-baselines")

    code, output = run_main("--compiler", selfhost, "--baselines", str(self.baselines), "--jobs", "2")

    self.assertEqual(code, 1)
    self.assertIn(f"different: {CI_TARGET} (--output)", output)
    self.assertIn("+++ selfhost", output)
    self.assertIn("--- baseline", output)

  def test_a_missing_baseline_fails(self):
    compiler = write_fake_compiler(self.workdir, "fake")
    run_main("--compiler", compiler, "--baselines", str(self.baselines), "--write-baselines")
    (self.baselines / "fixture_adds.json").unlink()

    code, output = run_main("--compiler", compiler, "--baselines", str(self.baselines))

    self.assertEqual(code, 1)
    self.assertIn("different: fixture adds", output)
    self.assertIn("no baseline", output)

  def test_an_orphaned_baseline_fails(self):
    compiler = write_fake_compiler(self.workdir, "fake")
    run_main("--compiler", compiler, "--baselines", str(self.baselines), "--write-baselines")
    (self.baselines / "fixture_retired.json").write_bytes(b"{}\n")

    code, output = run_main("--compiler", compiler, "--baselines", str(self.baselines))

    self.assertEqual(code, 1)
    self.assertIn("different: orphaned baseline fixture_retired.json", output)

  def test_writing_prunes_orphaned_baselines(self):
    compiler = write_fake_compiler(self.workdir, "fake")
    self.baselines.mkdir()
    (self.baselines / "fixture_retired.json").write_bytes(b"{}\n")

    code, _ = run_main("--compiler", compiler, "--baselines", str(self.baselines), "--write-baselines")

    self.assertEqual(code, 0)
    self.assertFalse((self.baselines / "fixture_retired.json").exists())

  def test_a_failing_target_writes_nothing(self):
    compiler = write_fake_compiler(self.workdir, "fake", fail_on="helper.ign")
    self.baselines.mkdir()
    (self.baselines / "fixture_adds.json").write_bytes(b"old\n")

    code, _ = run_main("--compiler", compiler, "--baselines", str(self.baselines), "--write-baselines")

    self.assertEqual(code, 1)
    self.assertEqual((self.baselines / "fixture_adds.json").read_bytes(), b"old\n")
    self.assertEqual(sorted(path.name for path in self.baselines.iterdir()), ["fixture_adds.json"])

  def test_the_host_option_is_gone(self):
    completed = subprocess.run(
      [sys.executable, str(DOC_PARITY_PY), "--compiler", "ignis", "--host", "ignis"],
      capture_output=True,
      text=True,
      check=False,
    )

    self.assertEqual(completed.returncode, 2, completed.stderr)
    self.assertIn("unrecognized arguments: --host", completed.stderr)


if __name__ == "__main__":
  os.chdir(REPO_ROOT)
  unittest.main()
