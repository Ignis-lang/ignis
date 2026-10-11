#!/usr/bin/env python3
"""Tests for the pure parts of scripts/check_symbol_stability.py.

The script's end-to-end run needs a compiler and runs in CI against stage1;
these cover how it reads `nm` output and what it reports as missing.
"""

import importlib.util
import os
import unittest

SCRIPTS_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
SPEC = importlib.util.spec_from_file_location("check_symbol_stability", os.path.join(SCRIPTS_DIR, "check_symbol_stability.py"))
CHECKER = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(CHECKER)


class DefinedSymbolsTests(unittest.TestCase):
    def test_keeps_defined_names_of_every_section(self):
        listing = "\n".join([
            "0000000000000000 t _IN3std6vector6VectorI3i32E4pushE",
            "0000000000000010 T main",
            "0000000000000000 d _IN3std7process7runtime12ProcessSlots4ARGCE",
            "0000000000000000 b counter_12",
        ])

        self.assertEqual(
            CHECKER.defined_symbols(listing),
            {"_IN3std6vector6VectorI3i32E4pushE", "main", "_IN3std7process7runtime12ProcessSlots4ARGCE", "counter_12"},
        )

    def test_drops_undefined_names_and_local_labels(self):
        listing = "\n".join([
            "                 U malloc",
            "                 w __gmon_start__",
            "0000000000000000 r .LC0",
            "0000000000000020 t helper",
        ])

        self.assertEqual(CHECKER.defined_symbols(listing), {"helper"})


class MissingSymbolsTests(unittest.TestCase):
    def test_a_shared_definition_renamed_by_its_id_is_missing(self):
        first = {"main", "push_1960", "_IN3std6answerE"}
        second = {"main", "push_1977", "_IN3std6answerE", "padding_12"}

        self.assertEqual(CHECKER.missing_symbols(first, second), ["push_1960"])

    def test_extra_definitions_in_the_second_program_are_fine(self):
        self.assertEqual(CHECKER.missing_symbols({"a", "b"}, {"a", "b", "c"}), [])


if __name__ == "__main__":
    unittest.main()
