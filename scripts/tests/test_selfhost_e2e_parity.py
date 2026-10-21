#!/usr/bin/env python3
"""Exercises how scripts/selfhost_e2e_parity.py lays a case out.

The harness itself is no longer a promotion gate (G2 was removed once G3's
`e2e::` fixtures covered the same corpus), but G7's harness reuses its case
discovery, so the layout contract still has to hold. A fixture whose helper
modules sit in a directory named after it imports them
relative to itself. The generated project has to carry that directory, or the
case fails to resolve its imports no matter which compiler runs it. Nothing
here runs a compiler.

Usage: python3 scripts/tests/test_selfhost_e2e_parity.py
"""

import shutil
import sys
import tempfile
import unittest
from pathlib import Path

SCRIPT_DIR = Path(__file__).resolve().parent.parent

sys.path.insert(0, str(SCRIPT_DIR))

import selfhost_e2e_parity as parity  # noqa: E402


class MaterialiseCaseTests(unittest.TestCase):
  def setUp(self):
    self.root = Path(tempfile.mkdtemp(prefix="e2e-parity-"))
    self.corpus = self.root / "corpus"
    self.work = self.root / "work"
    self.corpus.mkdir()

  def tearDown(self):
    shutil.rmtree(self.root, ignore_errors=True)

  def case(self, name):
    path = self.corpus / f"{name}.ign"
    return parity.Case(name=name, helper="", source=path.read_text(encoding="utf-8"), path=path)

  def test_a_single_file_case_becomes_src_main(self):
    (self.corpus / "alone.ign").write_text("function main(): i32 {\n  return 0;\n}\n", encoding="utf-8")

    case_dir = parity.materialise_case(self.case("alone"), Path("/std"), self.work)

    self.assertEqual((case_dir / "src" / "main.ign").read_text(encoding="utf-8"), "function main(): i32 {\n  return 0;\n}\n")
    self.assertEqual(sorted(p.name for p in (case_dir / "src").iterdir()), ["main.ign"])

  def test_the_helper_directory_is_copied_beside_main(self):
    (self.corpus / "split.ign").write_text('import f from "./split/inner/helper";\n', encoding="utf-8")
    (self.corpus / "split" / "inner").mkdir(parents=True)
    (self.corpus / "split" / "inner" / "helper.ign").write_text("export function f(): i32 {\n  return 1;\n}\n", encoding="utf-8")

    case_dir = parity.materialise_case(self.case("split"), Path("/std"), self.work)

    copied = case_dir / "src" / "split" / "inner" / "helper.ign"
    self.assertTrue(copied.is_file())
    self.assertEqual(copied.read_text(encoding="utf-8"), "export function f(): i32 {\n  return 1;\n}\n")

  def test_materialising_twice_replaces_the_previous_layout(self):
    (self.corpus / "split.ign").write_text("function main(): i32 {\n  return 0;\n}\n", encoding="utf-8")
    (self.corpus / "split").mkdir()
    (self.corpus / "split" / "helper.ign").write_text("// first\n", encoding="utf-8")

    parity.materialise_case(self.case("split"), Path("/std"), self.work)
    (self.corpus / "split" / "helper.ign").write_text("// second\n", encoding="utf-8")
    case_dir = parity.materialise_case(self.case("split"), Path("/std"), self.work)

    self.assertEqual((case_dir / "src" / "split" / "helper.ign").read_text(encoding="utf-8"), "// second\n")


if __name__ == "__main__":
  unittest.main()
