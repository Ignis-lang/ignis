#!/usr/bin/env python3
"""Exercises scripts/selfhost_doc_parity.py without a real compiler.

The pure parts are tested directly: which targets the script documents, how
two outputs are compared, and the report it prints. The end-to-end tests drive
fake compilers, small Python scripts that answer `doc` the way the real one
does, printing a package or writing it through `--output`.

Usage: python3 scripts/tests/test_selfhost_doc_parity.py
"""

import contextlib
import io
import os
import stat
import sys
import tempfile
import unittest
from pathlib import Path

SCRIPT_DIR = Path(__file__).resolve().parent.parent

sys.path.insert(0, str(SCRIPT_DIR))

from selfhost_doc_parity import (  # noqa: E402
  CI_TARGET,
  FIXTURES,
  REPO_ROOT,
  RunResult,
  Target,
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
# does.
FAKE_COMPILER = """#!/usr/bin/env python3
import sys

arguments = sys.argv[1:]
payload = '{{"entry": "' + arguments[1] + '"}}{suffix}'

if "--output" in arguments:
  with open(arguments[arguments.index("--output") + 1], "w") as handle:
    handle.write(payload)
else:
  print(payload)
"""


def write_fake_compiler(directory: Path, name: str, suffix: str) -> str:
  path = directory / name
  path.write_text(FAKE_COMPILER.format(suffix=suffix), encoding="utf-8")
  path.chmod(path.stat().st_mode | stat.S_IXUSR)
  return str(path)


class TargetTests(unittest.TestCase):
  def test_the_workflow_target_comes_first_and_writes_through_output(self):
    targets = std_targets(REPO_ROOT)

    self.assertEqual(targets[0].argument, CI_TARGET)
    self.assertTrue(targets[0].output_file)

  def test_every_std_module_entry_is_a_stdout_target(self):
    targets = std_targets(REPO_ROOT)
    expected = sorted(path.relative_to(REPO_ROOT).as_posix() for path in (REPO_ROOT / "std").glob("*/mod.ign"))
    stdout_arguments = [target.argument for target in targets if not target.output_file]

    self.assertEqual(stdout_arguments, expected)
    self.assertIn(CI_TARGET, stdout_arguments)

  def test_fixtures_are_written_under_their_module_name(self):
    with tempfile.TemporaryDirectory() as directory:
      workdir = Path(directory)
      targets = fixture_targets(workdir)

      self.assertEqual([target.argument for target in targets], [f"{name}.ign" for name in FIXTURES])

      for name, source in FIXTURES.items():
        self.assertEqual((workdir / f"{name}.ign").read_text(encoding="utf-8"), source)

  def test_the_host_test_fixtures_are_all_present(self):
    self.assertEqual(
      sorted(FIXTURES),
      ["adds", "counter", "described", "helper", "math", "outcome", "written"],
    )


class CompareTests(unittest.TestCase):
  def test_same_bytes_and_exit_code_are_identical(self):
    result = RunResult(exit_code=0, payload=b"{}\n", stderr="")

    self.assertTrue(compare("t", result, result, 10).identical)

  def test_a_different_exit_code_is_reported_with_the_selfhost_stderr(self):
    host = RunResult(exit_code=0, payload=b"{}\n", stderr="")
    selfhost = RunResult(exit_code=1, payload=b"", stderr="error: boom\n")

    comparison = compare("t", host, selfhost, 10)

    self.assertFalse(comparison.identical)
    self.assertIn("exit code: host 0, selfhost 1", comparison.detail)
    self.assertIn("error: boom", comparison.detail)

  def test_a_changed_line_shows_in_the_diff(self):
    host = RunResult(exit_code=0, payload=b'{\n  "a": 1\n}\n', stderr="")
    selfhost = RunResult(exit_code=0, payload=b'{\n  "a": 2\n}\n', stderr="")

    comparison = compare("t", host, selfhost, 10)

    self.assertFalse(comparison.identical)
    self.assertIn('-  "a": 1', comparison.detail)
    self.assertIn('+  "a": 2', comparison.detail)

  def test_a_long_diff_is_cut(self):
    host = "".join(f"{index}\n" for index in range(50)).encode()
    selfhost = "".join(f"x{index}\n" for index in range(50)).encode()

    text = diff_text(host, selfhost, 5)

    self.assertEqual(len(text.splitlines()), 6)
    self.assertIn("more diff lines", text.splitlines()[-1])

  def test_a_trailing_newline_alone_is_reported_by_length(self):
    self.assertEqual(
      diff_text(b"{}", b"{}\n", 10),
      "same lines, different bytes: host 2 bytes, selfhost 3 bytes",
    )

  def test_the_report_lists_every_target_and_a_summary(self):
    host = RunResult(exit_code=0, payload=b"a\n", stderr="")
    selfhost = RunResult(exit_code=0, payload=b"b\n", stderr="")
    report = format_report([compare("one", host, host, 10), compare("two", host, selfhost, 10)])
    lines = report.splitlines()

    self.assertEqual(lines[0], "identical: one")
    self.assertEqual(lines[1], "different: two")
    self.assertTrue(lines[2].startswith("    "))
    self.assertEqual(lines[-1], "summary: 1 identical, 1 different")


class EndToEndTests(unittest.TestCase):
  def test_run_doc_reads_the_output_file_for_an_output_target(self):
    with tempfile.TemporaryDirectory() as directory:
      workdir = Path(directory)
      compiler = write_fake_compiler(workdir, "fake", "")
      target = Target(label="t", cwd=workdir, argument="a.ign", output_file=True)

      result = run_doc(compiler, target, workdir, workdir / "out.json")

      self.assertEqual(result.exit_code, 0)
      self.assertEqual(result.payload, b'{"entry": "a.ign"}')

  def test_identical_compilers_exit_zero(self):
    with tempfile.TemporaryDirectory() as directory:
      workdir = Path(directory)
      host = write_fake_compiler(workdir, "host", "")
      selfhost = write_fake_compiler(workdir, "selfhost", "")

      with contextlib.redirect_stdout(io.StringIO()) as output:
        code = main(["--compiler", selfhost, "--host", host])

      self.assertEqual(code, 0)
      self.assertNotIn("different:", output.getvalue())
      self.assertIn(f"identical: fixture {next(iter(FIXTURES))}", output.getvalue())

  def test_a_differing_compiler_exits_one_with_a_diff(self):
    with tempfile.TemporaryDirectory() as directory:
      workdir = Path(directory)
      host = write_fake_compiler(workdir, "host", "")
      selfhost = write_fake_compiler(workdir, "selfhost", " ")

      with contextlib.redirect_stdout(io.StringIO()) as output:
        code = main(["--compiler", selfhost, "--host", host, "--jobs", "2"])

      self.assertEqual(code, 1)
      self.assertIn(f"different: {CI_TARGET} (--output)", output.getvalue())
      self.assertIn("+++ selfhost", output.getvalue())


if __name__ == "__main__":
  os.chdir(REPO_ROOT)
  unittest.main()
