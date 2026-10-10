#!/usr/bin/env python3
"""Tests for scripts/check_platform_constants.py.

Each test writes a small platform file under <project>/build/platform-constants-tests/
and runs the checker on it through its command line, with the real C compiler
and the repository's std/manifest.toml.
"""

import os
import shutil
import subprocess
import sys
import unittest
import uuid

SCRIPTS_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
PROJECT_ROOT = os.path.dirname(SCRIPTS_DIR)
CHECKER = os.path.join(SCRIPTS_DIR, "check_platform_constants.py")
TEMP_ROOT = os.path.join(PROJECT_ROOT, "build", "platform-constants-tests")

LITERAL = '@configFlag(@platform("linux") && @arch("x86_64"))'
FALLBACK = '@configFlag(!(@platform("linux") && @arch("x86_64")))'


def platform_file(literal, fallback=None, externs=None):
    """A platform module with the three blocks the checker reads.

    `literal` maps each name to `(type, value)`; the other two blocks default to
    the same names in the same order.
    """
    names = list(literal)
    fallback = names if fallback is None else fallback
    externs = names if externs is None else externs
    lines = ['import CType from "./primitives";', "", f"{FALLBACK}", "extern __platform_headers {"]
    for name in externs:
        lines.append(f"  const {name}: {literal.get(name, ('CType::CInt', 0))[0]};")
    lines += ["}", "", LITERAL, "export namespace Platform {"]
    for name, (ty, value) in literal.items():
        lines.append(f"  const {name}: {ty} = {value};")
    lines += ["}", "", FALLBACK, "export namespace Platform {"]
    for name in fallback:
        lines.append(f"  const {name}: {literal.get(name, ('CType::CInt', 0))[0]} = __platform_headers::{name};")
    lines.append("}")
    return "\n".join(lines) + "\n"


class CheckPlatformConstantsTests(unittest.TestCase):
    def setUp(self):
        self.root = os.path.join(TEMP_ROOT, uuid.uuid4().hex)
        os.makedirs(self.root)

    def tearDown(self):
        shutil.rmtree(self.root, ignore_errors=True)

    def run_checker(self, source):
        path = os.path.join(self.root, "platform.ign")
        with open(path, "w") as handle:
            handle.write(source)
        return subprocess.run(
            [sys.executable, CHECKER, "--platform-file", path, "--any-host"],
            capture_output=True,
            text=True,
        )

    def test_matching_values_pass(self):
        result = self.run_checker(platform_file({"SEEK_END": ("CType::CInt", 2), "EOF": ("CType::CInt", -1)}))

        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout.strip(), "platform constants: 2 constants match the C headers")
        self.assertEqual(result.stderr, "")

    def test_a_wrong_value_names_both_values(self):
        result = self.run_checker(platform_file({"SEEK_END": ("CType::CInt", 7)}))

        self.assertEqual(result.returncode, 1)
        self.assertEqual(result.stderr.strip(), "platform constants: SEEK_END is 7 in std/libc/platform.ign, 2 in C")
        self.assertEqual(result.stdout, "")

    def test_hexadecimal_values_compare_by_value(self):
        result = self.run_checker(platform_file({"O_CREAT": ("CType::CInt", "0x40")}))

        self.assertEqual(result.returncode, 0, result.stderr)

    def test_a_name_the_headers_do_not_define_fails(self):
        result = self.run_checker(platform_file({"IGNIS_NOT_A_MACRO": ("CType::CInt", 1)}))

        self.assertEqual(result.returncode, 1)
        self.assertIn("did not compile", result.stderr)
        self.assertIn("IGNIS_NOT_A_MACRO", result.stderr)

    def test_a_constant_missing_from_the_fallback_table_fails(self):
        source = platform_file({"SEEK_SET": ("CType::CInt", 0), "SEEK_END": ("CType::CInt", 2)}, fallback=["SEEK_SET"])
        result = self.run_checker(source)

        self.assertEqual(result.returncode, 1)
        self.assertIn("header-reading Platform does not list the literal table's constants", result.stderr)
        self.assertIn("missing: ['SEEK_END']", result.stderr)

    def test_a_constant_missing_from_the_extern_block_fails(self):
        source = platform_file({"SEEK_SET": ("CType::CInt", 0), "SEEK_END": ("CType::CInt", 2)}, externs=["SEEK_END"])
        result = self.run_checker(source)

        self.assertEqual(result.returncode, 1)
        self.assertIn("extern __platform_headers does not list the literal table's constants", result.stderr)
        self.assertIn("missing: ['SEEK_SET']", result.stderr)

    def test_a_missing_block_is_a_usage_error(self):
        result = self.run_checker("export namespace Platform {}\n")

        self.assertEqual(result.returncode, 2)
        self.assertIn("missing block", result.stderr)

    def test_the_committed_platform_file_matches_this_host(self):
        result = subprocess.run([sys.executable, CHECKER, "--any-host"], capture_output=True, text=True)

        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertRegex(result.stdout, r"^platform constants: \d+ constants match the C headers\n$")


if __name__ == "__main__":
    unittest.main()
