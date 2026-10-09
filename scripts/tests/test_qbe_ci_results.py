#!/usr/bin/env python3
"""Tests for scripts/qbe_ci_results.py, the QBE CI aggregation checker.

Every test builds its fixture tree under <project>/build/qbe-ci-worker/, never
under /tmp: the checker is only ever allowed to write inside the project's
build directory, so the tests exercise it in the same rooted layout CI uses.
"""

import os
import shutil
import subprocess
import sys
import unittest
import uuid

SCRIPTS_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
PROJECT_ROOT = os.path.dirname(SCRIPTS_DIR)
CHECKER = os.path.join(SCRIPTS_DIR, "qbe_ci_results.py")
TEMP_ROOT = os.path.join(PROJECT_ROOT, "build", "qbe-ci-worker")

SHARDS = 4

# discover() sorts complete relative paths after collection. A file sharing
# a directory's stem sorts before that directory's children.
CATALOG = [
    "arithmetic_add.ign",
    "callback_fold.ign",
    "closure_cross_module.ign",
    "closure_cross_module/helper.ign",
    "closure_cross_module/main.ign",
    "match_nested.ign",
    "record_field_default.ign",
    "string_concat.ign",
]

SUPPORTED = [
    "arithmetic_add.ign",
    "callback_fold.ign",
    "closure_cross_module.ign",
    "closure_cross_module/helper.ign",
    "closure_cross_module/main.ign",
    "record_field_default.ign",
]

SKIP_REASONS = {
    "match_nested.ign": "not supported by the qbe target yet: Match",
    "string_concat.ign": "not supported by the qbe target yet: Concat",
}


def catalog_index(name):
    return CATALOG.index(name)


def committed_classification(name):
    if name in SUPPORTED:
        return ("supported", None)
    return ("skipped", SKIP_REASONS[name])


class QbeCiResultsTest(unittest.TestCase):
    def setUp(self):
        os.makedirs(TEMP_ROOT, exist_ok=True)
        self.base = os.path.join(TEMP_ROOT, "test-" + uuid.uuid4().hex)
        os.makedirs(self.base)
        self.addCleanup(shutil.rmtree, self.base, ignore_errors=True)

    # -- fixture construction -------------------------------------------------

    def write_corpus(self):
        corpus_dir = os.path.join(self.base, "test_cases", "e2e", "ok")
        for name in CATALOG + ["__snapshots__/ignored.ign"]:
            path = os.path.join(corpus_dir, name)
            os.makedirs(os.path.dirname(path), exist_ok=True)
            with open(path, "w", encoding="utf-8") as handle:
                handle.write("function main(): i32 {\n    return 0;\n}\n")
        return corpus_dir

    def write_committed_manifests(self):
        supported_path = os.path.join(self.base, "test_cases", "qbe_supported.txt")
        with open(supported_path, "w", encoding="utf-8") as handle:
            handle.write("".join(name + "\n" for name in SUPPORTED))
        skip_path = os.path.join(self.base, "test_cases", "qbe_skip.tsv")
        with open(skip_path, "w", encoding="utf-8") as handle:
            for name, reason in SKIP_REASONS.items():
                handle.write(name + "\t" + reason + "\n")
        return supported_path, skip_path

    def shard_assignment(self, shard):
        return [
            name
            for name in CATALOG
            if catalog_index(name) % SHARDS == shard
        ]

    def write_valid_shard(self, report_root, mode, shard, classification=None):
        """Write one mode/shard report directory that satisfies the contract."""
        shard_dir = os.path.join(report_root, mode, str(shard))
        os.makedirs(shard_dir, exist_ok=True)
        assigned = self.shard_assignment(shard)
        supported = []
        skipped = []
        for name in assigned:
            kind, reason = (classification or committed_classification)(name)
            if kind == "supported":
                supported.append(name)
            else:
                skipped.append((name, reason))
        with open(os.path.join(shard_dir, "all.txt"), "w", encoding="utf-8") as handle:
            handle.write("".join(name + "\n" for name in CATALOG))
        with open(os.path.join(shard_dir, "supported.txt"), "w", encoding="utf-8") as handle:
            handle.write("".join(name + "\n" for name in supported))
        with open(os.path.join(shard_dir, "skip.tsv"), "w", encoding="utf-8") as handle:
            for name, reason in skipped:
                handle.write(name + "\t" + reason + "\n")
        complete = [
            str(shard),
            str(SHARDS),
            str(len(CATALOG)),
            str(len(assigned)),
            str(len(supported)),
            str(len(skipped)),
        ]
        with open(os.path.join(shard_dir, "COMPLETE"), "w", encoding="utf-8") as handle:
            handle.write("".join(value + "\n" for value in complete))

    def write_valid_reports(self, report_root):
        for mode in ("inventory", "lane"):
            for shard in range(SHARDS):
                self.write_valid_shard(report_root, mode, shard)

    def run_checker(self, report_root, extra_args=None, transform_manifests=None):
        corpus_dir = self.write_corpus()
        supported_path, skip_path = self.write_committed_manifests()
        if transform_manifests:
            transform_manifests(supported_path, skip_path)
        command = [
            sys.executable,
            CHECKER,
            "--report-root",
            report_root,
            "--corpus-dir",
            corpus_dir,
            "--supported-manifest",
            supported_path,
            "--skip-manifest",
            skip_path,
            "--shards",
            str(SHARDS),
            "--summary-dir",
            os.path.join(self.base, "build", "qbe-results", "summary"),
        ]
        if extra_args:
            command.extend(extra_args)
        return subprocess.run(command, capture_output=True, text=True)

    def assert_fails_with(self, result, fragment):
        self.assertEqual(result.returncode, 1, result.stderr)
        self.assertIn(fragment, result.stderr)

    # -- acceptance -----------------------------------------------------------

    def test_valid_full_run_passes(self):
        report_root = os.path.join(self.base, "build", "qbe-results", "merged")
        self.write_valid_reports(report_root)
        result = self.run_checker(report_root)
        self.assertEqual(result.returncode, 0, result.stderr)

    def test_summary_and_merged_reports_are_written_under_build(self):
        report_root = os.path.join(self.base, "build", "qbe-results", "merged")
        self.write_valid_reports(report_root)
        summary_dir = os.path.join(self.base, "build", "qbe-results", "summary")
        result = self.run_checker(report_root)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertTrue(os.path.isfile(os.path.join(summary_dir, "summary.json")))
        for merged in (
            "inventory-supported.txt",
            "inventory-skip.tsv",
            "lane-supported.txt",
            "lane-skip.tsv",
        ):
            self.assertTrue(os.path.isfile(os.path.join(summary_dir, merged)), merged)

    def test_missing_committed_case_fails(self):
        report_root = os.path.join(self.base, "build", "qbe-results", "merged")
        self.write_valid_reports(report_root)

        def remove_case(supported_path, skip_path):
            with open(supported_path, "w", encoding="utf-8") as handle:
                handle.write("".join(name + "\n" for name in SUPPORTED[1:]))

        result = self.run_checker(report_root, transform_manifests=remove_case)
        self.assert_fails_with(result, "committed inventories do not match the corpus")

    def test_unknown_committed_case_fails(self):
        report_root = os.path.join(self.base, "build", "qbe-results", "merged")
        self.write_valid_reports(report_root)

        def add_case(supported_path, skip_path):
            with open(supported_path, "a", encoding="utf-8") as handle:
                handle.write("unknown.ign\n")

        result = self.run_checker(report_root, transform_manifests=add_case)
        self.assert_fails_with(result, "committed inventories do not match the corpus")

    # -- missing pieces -------------------------------------------------------

    def test_missing_shard_directory_fails(self):
        report_root = os.path.join(self.base, "build", "qbe-results", "merged")
        self.write_valid_reports(report_root)
        shutil.rmtree(os.path.join(report_root, "lane", "2"))
        result = self.run_checker(report_root)
        self.assert_fails_with(result, "lane/2")

    def test_missing_complete_fails(self):
        report_root = os.path.join(self.base, "build", "qbe-results", "merged")
        self.write_valid_reports(report_root)
        os.remove(os.path.join(report_root, "inventory", "1", "COMPLETE"))
        result = self.run_checker(report_root)
        self.assert_fails_with(result, "inventory/1")

    def test_missing_report_file_fails(self):
        report_root = os.path.join(self.base, "build", "qbe-results", "merged")
        self.write_valid_reports(report_root)
        os.remove(os.path.join(report_root, "lane", "3", "skip.tsv"))
        result = self.run_checker(report_root)
        self.assert_fails_with(result, "lane/3")

    def test_missing_case_fails(self):
        report_root = os.path.join(self.base, "build", "qbe-results", "merged")
        self.write_valid_reports(report_root)
        victim = "record_field_default.ign"
        shard = catalog_index(victim) % SHARDS
        shard_dir = os.path.join(report_root, "lane", str(shard))
        with open(os.path.join(shard_dir, "supported.txt"), "w", encoding="utf-8") as handle:
            handle.write(
                "".join(
                    name + "\n"
                    for name in self.shard_assignment(shard)
                    if name != victim and committed_classification(name)[0] == "supported"
                )
            )
        result = self.run_checker(report_root)
        self.assert_fails_with(result, victim)

    # -- malformed or stale COMPLETE -------------------------------------------

    def test_malformed_complete_fails(self):
        report_root = os.path.join(self.base, "build", "qbe-results", "merged")
        self.write_valid_reports(report_root)
        for bad in ("1 2 3 4 5 6\n", "-1\n4\n7\n3\n2\n1\n", "0\n4\n7\n3\n2\n1\nextra\n"):
            with open(
                os.path.join(report_root, "inventory", "0", "COMPLETE"),
                "w",
                encoding="utf-8",
            ) as handle:
                handle.write(bad)
            result = self.run_checker(report_root)
            self.assert_fails_with(result, "COMPLETE")

    def test_stale_complete_counter_fails(self):
        report_root = os.path.join(self.base, "build", "qbe-results", "merged")
        self.write_valid_reports(report_root)
        # The report files still show one supported case for shard 0, but the
        # COMPLETE a crashed or stale shard left behind claims one fewer pass.
        complete_path = os.path.join(report_root, "lane", "0", "COMPLETE")
        with open(complete_path, encoding="utf-8") as handle:
            values = handle.read().splitlines()
        values[4] = str(int(values[4]) - 1)
        with open(complete_path, "w", encoding="utf-8") as handle:
            handle.write("".join(value + "\n" for value in values))
        result = self.run_checker(report_root)
        self.assert_fails_with(result, "passed")

    def test_wrong_shard_count_in_complete_fails(self):
        report_root = os.path.join(self.base, "build", "qbe-results", "merged")
        self.write_valid_reports(report_root)
        complete_path = os.path.join(report_root, "inventory", "2", "COMPLETE")
        with open(complete_path, encoding="utf-8") as handle:
            values = handle.read().splitlines()
        values[1] = "3"
        with open(complete_path, "w", encoding="utf-8") as handle:
            handle.write("".join(value + "\n" for value in values))
        result = self.run_checker(report_root)
        self.assert_fails_with(result, "shard count")

    def test_wrong_catalog_total_in_complete_fails(self):
        report_root = os.path.join(self.base, "build", "qbe-results", "merged")
        self.write_valid_reports(report_root)
        complete_path = os.path.join(report_root, "lane", "1", "COMPLETE")
        with open(complete_path, encoding="utf-8") as handle:
            values = handle.read().splitlines()
        values[2] = str(len(CATALOG) + 1)
        with open(complete_path, "w", encoding="utf-8") as handle:
            handle.write("".join(value + "\n" for value in values))
        result = self.run_checker(report_root)
        self.assert_fails_with(result, "total")

    # -- partition and duplicates ----------------------------------------------

    def test_duplicate_case_across_shards_fails(self):
        report_root = os.path.join(self.base, "build", "qbe-results", "merged")
        self.write_valid_reports(report_root)
        victim = "arithmetic_add.ign"
        owner = catalog_index(victim) % SHARDS
        thief = (owner + 1) % SHARDS
        for shard in (owner, thief):
            shard_dir = os.path.join(report_root, "lane", str(shard))
            with open(os.path.join(shard_dir, "supported.txt"), "a", encoding="utf-8") as handle:
                handle.write(victim + "\n")
        result = self.run_checker(report_root)
        self.assert_fails_with(result, victim)

    def test_case_in_wrong_shard_fails(self):
        report_root = os.path.join(self.base, "build", "qbe-results", "merged")
        self.write_valid_reports(report_root)
        victim = "match_nested.ign"
        owner = catalog_index(victim) % SHARDS
        wrong = (owner + 1) % SHARDS
        # Owner drops it, wrong shard picks it up; both shards keep their
        # counters consistent so only the mod-4 assignment can be at fault.
        for shard, kind in ((owner, "skipped"), (wrong, "skipped")):
            shard_dir = os.path.join(report_root, "lane", str(shard))
            assignment = self.shard_assignment(shard)
            if shard == owner:
                assignment = [name for name in assignment if name != victim]
            else:
                assignment = assignment + [victim]
            supported = [name for name in assignment if committed_classification(name)[0] == "supported"]
            skipped = [(name, committed_classification(name)[1]) for name in assignment if committed_classification(name)[0] != "supported"]
            with open(os.path.join(shard_dir, "supported.txt"), "w", encoding="utf-8") as handle:
                handle.write("".join(name + "\n" for name in supported))
            with open(os.path.join(shard_dir, "skip.tsv"), "w", encoding="utf-8") as handle:
                for name, reason in skipped:
                    handle.write(name + "\t" + reason + "\n")
            complete = [
                str(shard),
                str(SHARDS),
                str(len(CATALOG)),
                str(len(assignment)),
                str(len(supported)),
                str(len(skipped)),
            ]
            with open(os.path.join(shard_dir, "COMPLETE"), "w", encoding="utf-8") as handle:
                handle.write("".join(value + "\n" for value in complete))
        result = self.run_checker(report_root)
        self.assert_fails_with(result, victim)

    # -- catalog ---------------------------------------------------------------

    def test_all_txt_diverges_from_corpus_fails(self):
        report_root = os.path.join(self.base, "build", "qbe-results", "merged")
        self.write_valid_reports(report_root)
        all_path = os.path.join(report_root, "inventory", "0", "all.txt")
        with open(all_path, encoding="utf-8") as handle:
            lines = handle.read().splitlines()
        lines.remove("callback_fold.ign")
        with open(all_path, "w", encoding="utf-8") as handle:
            handle.write("".join(line + "\n" for line in lines))
        result = self.run_checker(report_root)
        self.assert_fails_with(result, "all.txt")

    def test_all_txt_with_wrong_order_fails(self):
        report_root = os.path.join(self.base, "build", "qbe-results", "merged")
        self.write_valid_reports(report_root)
        all_path = os.path.join(report_root, "inventory", "3", "all.txt")
        with open(all_path, encoding="utf-8") as handle:
            lines = handle.read().splitlines()
        lines[0], lines[1] = lines[1], lines[0]
        with open(all_path, "w", encoding="utf-8") as handle:
            handle.write("".join(line + "\n" for line in lines))
        result = self.run_checker(report_root)
        self.assert_fails_with(result, "all.txt")

    def test_snapshot_cases_are_excluded_from_the_catalog(self):
        # The corpus above contains __snapshots__/ignored.ign; a shard that
        # reports it as supported must fail because it is not in the catalog.
        report_root = os.path.join(self.base, "build", "qbe-results", "merged")
        self.write_valid_reports(report_root)
        with open(
            os.path.join(report_root, "lane", "0", "supported.txt"), "a", encoding="utf-8"
        ) as handle:
            handle.write("__snapshots__/ignored.ign\n")
        result = self.run_checker(report_root)
        self.assert_fails_with(result, "__snapshots__/ignored.ign")

    # -- committed inventory agreement -----------------------------------------

    def test_classification_mismatch_with_committed_manifests_fails(self):
        report_root = os.path.join(self.base, "build", "qbe-results", "merged")
        self.write_valid_reports(report_root)

        def swapped(name):
            if name == "record_field_default.ign":
                return ("skipped", "not supported by the qbe target yet: FieldDefault")
            return committed_classification(name)

        for shard in range(SHARDS):
            self.write_valid_shard(report_root, "lane", shard, classification=swapped)
        result = self.run_checker(report_root)
        self.assert_fails_with(result, "record_field_default.ign")

    def test_skip_reason_mismatch_fails(self):
        report_root = os.path.join(self.base, "build", "qbe-results", "merged")
        self.write_valid_reports(report_root)

        def renamed(name):
            if name == "string_concat.ign":
                return ("skipped", "some other reason")
            return committed_classification(name)

        for shard in range(SHARDS):
            self.write_valid_shard(report_root, "inventory", shard, classification=renamed)
        result = self.run_checker(report_root)
        self.assert_fails_with(result, "string_concat.ign")

    # -- global progress --------------------------------------------------------

    def test_zero_global_passed_fails(self):
        report_root = os.path.join(self.base, "build", "qbe-results", "merged")
        self.write_valid_reports(report_root)

        def all_skipped(name):
            return ("skipped", SKIP_REASONS.get(name, "not supported by the qbe target yet: Everything"))

        for shard in range(SHARDS):
            self.write_valid_shard(report_root, "lane", shard, classification=all_skipped)
        result = self.run_checker(report_root)
        self.assert_fails_with(result, "passed")


if __name__ == "__main__":
    unittest.main()
