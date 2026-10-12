#!/usr/bin/env python3
"""Tests for the pure parts of scripts/check_cross_backend_abi.py.

The end-to-end check needs a compiler and QBE and runs in the QBE workflow;
these cover how it registers the probe module and how it judges one build.
"""

import importlib.util
import os
import subprocess
import unittest

SCRIPTS_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
SPEC = importlib.util.spec_from_file_location("check_cross_backend_abi", os.path.join(SCRIPTS_DIR, "check_cross_backend_abi.py"))
CHECKER = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(CHECKER)


def completed(code, stdout="", stderr=""):
    return subprocess.CompletedProcess([], code, stdout, stderr)


class ManifestTests(unittest.TestCase):
    def test_the_module_goes_first_under_modules(self):
        manifest = "[toolchain]\ninclude_dirs = [\".\"]\n[modules]\nio = \"io/mod.ign\"\n"

        self.assertEqual(
            CHECKER.add_manifest_module(manifest),
            "[toolchain]\ninclude_dirs = [\".\"]\n[modules]\nabiprobe = \"abiprobe/mod.ign\"\nio = \"io/mod.ign\"\n",
        )

    def test_a_manifest_without_modules_is_an_error(self):
        with self.assertRaises(ValueError):
            CHECKER.add_manifest_module("[toolchain]\n")

    def test_the_repository_manifest_has_a_modules_section(self):
        path = os.path.join(CHECKER.PROJECT_ROOT, "std", "manifest.toml")
        with open(path, encoding="utf-8") as handle:
            self.assertIn(CHECKER.MANIFEST_LINE, CHECKER.add_manifest_module(handle.read()))


class OutcomeTests(unittest.TestCase):
    def test_an_agreeing_build_has_no_problem(self):
        self.assertIsNone(CHECKER.outcome_problem("x", completed(0), completed(0, "all shapes agree\n")))

    def test_a_failed_build_is_reported_with_its_output(self):
        problem = CHECKER.outcome_problem("std archive, QBE program", completed(1, "", "Error[B4004]: nope\n"), completed(1))

        self.assertTrue(problem.startswith("std archive, QBE program: the build failed"))
        self.assertIn("Error[B4004]: nope", problem)

    def test_a_disagreeing_shape_is_reported_with_its_name(self):
        problem = CHECKER.outcome_problem("x", completed(0), completed(1, "sumArray\n"))

        self.assertIn("exit 1, expected 0", problem)
        self.assertIn("sumArray", problem)

    def test_a_crash_is_a_problem(self):
        self.assertIsNotNone(CHECKER.outcome_problem("x", completed(0), completed(-11)))


if __name__ == "__main__":
    unittest.main()
