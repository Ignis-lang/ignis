#!/usr/bin/env python3
"""Aggregate and validate the QBE CI shard reports.

Every matrix shard writes, per mode (inventory / lane), a report directory
holding `all.txt`, `supported.txt`, `skip.tsv` and a `COMPLETE` marker written
last. This script is the aggregate job's gate: it accepts the merged report
root, the fixture corpus and the committed inventory manifests, and fails
unless every shard completed and the four shards together cover the corpus
exactly, with the classification the committed manifests record.

It only reads the committed manifests — the CI inventory shards write their
reports under build/, never the tracked files. The merged reports and the
summary are written to `--summary-dir`, which CI keeps under build/ as well.
"""

import argparse
import json
import os
import re
import sys

MODES = ("inventory", "lane")
REPORT_FILES = ("all.txt", "supported.txt", "skip.tsv", "COMPLETE")
SNAPSHOT_DIR_NAME = "__snapshots__"


def fail_collector(errors):
    def fail(message):
        errors.append(message)

    return fail


def read_text(path):
    with open(path, encoding="utf-8") as handle:
        return handle.read()


def parse_lines(text):
    """Split into non-empty lines, tolerating one trailing newline."""
    return [line for line in text.splitlines() if line.strip() != ""]


def expected_catalog(corpus_dir, fail):
    """The ordered fixture catalog: per-directory byte-sorted pre-order walk
    over the corpus, skipping __snapshots__ directories and non-.ign files —
    the same walk the Ignis runner's FixtureTests::discover performs."""
    entries = []

    def walk(directory):
        try:
            names = sorted(os.listdir(directory))
        except OSError as error:
            fail(f"cannot read corpus directory {directory}: {error}")
            return
        for name in names:
            path = os.path.join(directory, name)
            if os.path.isdir(path):
                if name == SNAPSHOT_DIR_NAME:
                    continue
                walk(path)
            elif name.endswith(".ign"):
                entries.append(os.path.relpath(path, corpus_dir).replace(os.sep, "/"))

    walk(corpus_dir)
    return entries


def parse_committed_manifests(supported_manifest, skip_manifest, fail):
    supported = parse_lines(read_text(supported_manifest))
    supported_set = set(supported)
    if len(supported_set) != len(supported):
        fail(f"{supported_manifest} lists a fixture more than once")
    skips = {}
    for line in parse_lines(read_text(skip_manifest)):
        columns = line.split("\t", 1)
        if len(columns) != 2 or columns[0] == "" or columns[1].strip() == "":
            fail(f"{skip_manifest} entry is not <name>\\t<non-empty reason>: {line!r}")
            continue
        name, reason = columns
        if name in skips:
            fail(f"{skip_manifest} lists {name} more than once")
        skips[name] = reason
    overlap = supported_set & set(skips)
    if overlap:
        fail(f"{supported_manifest} and {skip_manifest} both list: {', '.join(sorted(overlap))}")
    return supported_set, skips


def parse_complete(text, shard, mode_dir, fail):
    lines = text.splitlines()
    if len(lines) != 6 or any(not re.fullmatch(r"[0-9]+", line) for line in lines):
        fail(
            f"{mode_dir}/COMPLETE must hold six unsigned decimal values on six lines, got: {text!r}"
        )
        return None
    return [int(line) for line in lines]


def load_shard_report(report_root, mode, shard, fail):
    mode_dir = os.path.join(report_root, mode, str(shard))
    if not os.path.isdir(mode_dir):
        fail(f"missing report directory {mode_dir}")
        return None
    missing = [name for name in REPORT_FILES if not os.path.isfile(os.path.join(mode_dir, name))]
    if missing:
        fail(f"missing report file(s) in {mode_dir}: {', '.join(missing)}")
        return None
    try:
        complete = parse_complete(read_text(os.path.join(mode_dir, "COMPLETE")), shard, mode_dir, fail)
        return {
            "dir": mode_dir,
            "all": parse_lines(read_text(os.path.join(mode_dir, "all.txt"))),
            "supported": parse_lines(read_text(os.path.join(mode_dir, "supported.txt"))),
            "skips": parse_skip_tsv(read_text(os.path.join(mode_dir, "skip.tsv")), mode_dir, fail),
            "complete": complete,
        }
    except OSError as error:
        fail(f"cannot read report in {mode_dir}: {error}")
        return None


def parse_skip_tsv(text, mode_dir, fail):
    skips = {}
    for line in parse_lines(text):
        columns = line.split("\t", 1)
        if len(columns) != 2 or columns[1].strip() == "":
            fail(f"{mode_dir}/skip.tsv entry is not <name>\\t<reason>: {line!r}")
            continue
        name, reason = columns
        if name in skips:
            fail(f"{mode_dir}/skip.tsv lists {name} more than once")
        skips[name] = reason
    return skips


def validate_shard_counters(report, shard, shards, catalog, fail):
    complete = report["complete"]
    if complete is None:
        return 0
    shard_index, shard_count, total, selected, passed, skipped = complete
    mode_dir = report["dir"]
    if shard_index != shard:
        fail(f"{mode_dir}/COMPLETE records shard index {shard_index}, expected {shard}")
    if shard_count != shards:
        fail(f"{mode_dir}/COMPLETE records shard count {shard_count}, expected {shards}")
    if total != len(catalog):
        fail(f"{mode_dir}/COMPLETE records total {total}, expected {len(catalog)}")
    expected_selected = sum(1 for position in range(len(catalog)) if position % shards == shard)
    if selected != expected_selected:
        fail(f"{mode_dir}/COMPLETE records selected {selected}, expected {expected_selected}")
    if passed != len(report["supported"]):
        fail(
            f"{mode_dir}/COMPLETE records passed {passed}, but supported.txt holds "
            f"{len(report['supported'])} case(s)"
        )
    if skipped != len(report["skips"]):
        fail(
            f"{mode_dir}/COMPLETE records skipped {skipped}, but skip.tsv holds "
            f"{len(report['skips'])} case(s)"
        )
    if passed + skipped != selected:
        fail(
            f"{mode_dir}/COMPLETE records passed {passed} + skipped {skipped} != selected {selected}"
        )
    return passed


def validate_shard_case_lists(report, catalog, shards, fail):
    mode_dir = report["dir"]
    catalog_positions = {name: position for position, name in enumerate(catalog)}
    catalog_set = set(catalog)
    shard = report["complete"][0] if report["complete"] else None
    for name in report["supported"]:
        if name not in catalog_set:
            fail(f"{mode_dir}/supported.txt lists unknown case {name}")
    if len(set(report["supported"])) != len(report["supported"]):
        fail(f"{mode_dir}/supported.txt lists a case more than once")
    for name in report["skips"]:
        if name not in catalog_set:
            fail(f"{mode_dir}/skip.tsv lists unknown case {name}")
    both = set(report["supported"]) & set(report["skips"])
    if both:
        fail(f"{mode_dir} classifies {', '.join(sorted(both))} as both supported and skipped")
    if shard is None:
        return
    for name in list(report["supported"]) + list(report["skips"]):
        position = catalog_positions.get(name)
        if position is not None and position % shards != shard:
            fail(f"{mode_dir} ran case {name} (catalog position {position}), which belongs to shard {position % shards}")


def validate_mode(
    report_root, mode, shards, catalog, catalog_set, committed_supported, committed_skips, fail
):
    total_passed = 0
    observations = {}  # case name -> list of (shard, kind, reason)
    for shard in range(shards):
        report = load_shard_report(report_root, mode, shard, fail)
        if report is None:
            continue
        if report["all"] != catalog:
            fail(f"{mode}/{shard}/all.txt does not match the ordered corpus catalog")
        total_passed += validate_shard_counters(report, shard, shards, catalog, fail)
        validate_shard_case_lists(report, catalog, shards, fail)
        for name in report["supported"]:
            observations.setdefault(name, []).append((shard, "supported", None))
        for name, reason in report["skips"].items():
            observations.setdefault(name, []).append((shard, "skipped", reason))

    for name, entries in sorted(observations.items()):
        if len(entries) > 1:
            described = "; ".join(
                f"{mode}/{shard} {kind}" for shard, kind, _ in entries
            )
            fail(f"case {name} appears in more than one shard/classification: {described}")
    for name in catalog_set:
        if name not in observations:
            fail(f"case {name} is missing from every {mode} shard report")

    if total_passed <= 0:
        fail(f"no {mode} shard passed a single case; at least one global pass is required")

    for name in sorted(catalog_set):
        entries = observations.get(name, [])
        if name in committed_supported:
            if not any(kind == "supported" for _, kind, _ in entries):
                fail(
                    f"case {name}: committed {SUPPORTED_BASENAME} marks it supported, "
                    f"but the {mode} reports classify it differently"
                )
        elif name in committed_skips:
            reported = [reason for _, kind, reason in entries if kind == "skipped"]
            if not reported:
                fail(
                    f"case {name}: committed {SKIP_BASENAME} skips it, "
                    f"but no {mode} shard reported it skipped"
                )
            elif reported[0] != committed_skips[name]:
                fail(
                    f"case {name}: skip reason {reported[0]!r} does not match "
                    f"the committed {committed_skips[name]!r}"
                )
    return total_passed


SUPPORTED_BASENAME = "qbe_supported.txt"
SKIP_BASENAME = "qbe_skip.tsv"


def write_merged_mode(report_root, mode, shards, catalog, summary_dir):
    supported_order = {name: position for position, name in enumerate(catalog)}
    supported = []
    skips = []
    for shard in range(shards):
        shard_dir = os.path.join(report_root, mode, str(shard))
        if not os.path.isdir(shard_dir):
            continue
        for name in parse_lines(read_text(os.path.join(shard_dir, "supported.txt"))):
            supported.append(name)
        for name, reason in parse_skip_tsv(
            read_text(os.path.join(shard_dir, "skip.tsv")), shard_dir, lambda _message: None
        ).items():
            skips.append((name, reason))
    supported.sort(key=lambda name: supported_order.get(name, len(catalog)))
    skips.sort(key=lambda entry: supported_order.get(entry[0], len(catalog)))
    with open(os.path.join(summary_dir, f"{mode}-supported.txt"), "w", encoding="utf-8") as handle:
        handle.write("".join(name + "\n" for name in supported))
    with open(os.path.join(summary_dir, f"{mode}-skip.tsv"), "w", encoding="utf-8") as handle:
        for name, reason in skips:
            handle.write(name + "\t" + reason + "\n")
    return len(supported), len(skips)


def write_summary(report_root, summary_dir, shards, catalog):
    os.makedirs(summary_dir, exist_ok=True)
    mode_counts = {mode: write_merged_mode(report_root, mode, shards, catalog, summary_dir) for mode in MODES}
    summary = {
        "status": "pass",
        "shards": shards,
        "total": len(catalog),
        "modes": {
            mode: {"passed": counts[0], "skipped": counts[1]} for mode, counts in mode_counts.items()
        },
    }
    with open(os.path.join(summary_dir, "summary.json"), "w", encoding="utf-8") as handle:
        json.dump(summary, handle, indent=2)
        handle.write("\n")
    return summary


def count_mode_kinds(report_root, mode, shards):
    supported = 0
    skipped = 0
    for shard in range(shards):
        shard_dir = os.path.join(report_root, mode, str(shard))
        if not os.path.isdir(shard_dir):
            continue
        try:
            supported += len(parse_lines(read_text(os.path.join(shard_dir, "supported.txt"))))
            skipped += len(parse_skip_tsv(read_text(os.path.join(shard_dir, "skip.tsv")), shard_dir, lambda _message: None))
        except OSError:
            continue
    return supported, skipped


def parse_args(argv):
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--report-root", default="build/qbe-results", help="merged shard report root")
    parser.add_argument("--corpus-dir", default="test_cases/e2e/ok", help="the e2e fixture corpus")
    parser.add_argument("--supported-manifest", default="test_cases/qbe_supported.txt")
    parser.add_argument("--skip-manifest", default="test_cases/qbe_skip.tsv")
    parser.add_argument("--shards", type=int, default=4, help="shard count the matrix ran")
    parser.add_argument("--summary-dir", default="build/qbe-results/summary", help="where merged reports and the summary are written (must stay under build/)")
    return parser.parse_args(argv)


def main(argv=None):
    args = parse_args(argv if argv is not None else sys.argv[1:])
    errors = []
    fail = fail_collector(errors)

    catalog = expected_catalog(args.corpus_dir, fail)
    catalog_set = set(catalog)
    committed_supported, committed_skips = parse_committed_manifests(
        args.supported_manifest, args.skip_manifest, fail
    )

    committed_cases = committed_supported | set(committed_skips)
    if committed_cases != catalog_set:
        missing = sorted(catalog_set - committed_cases)
        unknown = sorted(committed_cases - catalog_set)
        fail(f"committed inventories do not match the corpus: missing={missing}, unknown={unknown}")

    mode_passes = {}
    mode_skips = {}
    if not errors:
        for mode in MODES:
            validate_mode(
                args.report_root,
                mode,
                args.shards,
                catalog,
                catalog_set,
                committed_supported,
                committed_skips,
                fail,
            )
            mode_passes[mode], mode_skips[mode] = count_mode_kinds(
                args.report_root, mode, args.shards
            )

    if errors:
        for error in errors:
            print(f"qbe-ci-results: error: {error}", file=sys.stderr)
        return 1

    write_summary(args.report_root, args.summary_dir, args.shards, catalog)
    for mode in MODES:
        print(
            f"qbe-ci-results: {mode}: {mode_passes[mode]} passed, {mode_skips[mode]} skipped, "
            f"{len(catalog)} total across {args.shards} shard(s)"
        )
    print(f"qbe-ci-results: summary written to {args.summary_dir}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
