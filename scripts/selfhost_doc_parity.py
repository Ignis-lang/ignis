#!/usr/bin/env python3
"""Compare a selfhost compiler's `ignis doc` output with committed baselines, byte for byte.

`ignis doc` prints an API package as pretty JSON, and the release and nightly
workflows publish the one `ignis doc std/io/mod.ign --std-path std --output
docs-pkg/api.json` writes. The baselines under `test_cases/doc/__doc_baselines__/`
hold the exact bytes the Rust host compiler wrote for every target before it was
retired, one file per target, and the compiler under test has to exit 0 and
write the same bytes:

- the workflows' own invocation, `std/io/mod.ign` written through `--output`.
  It is the only standard-library target: documenting any std module entry
  prints the same package of every std module (about 900 KB), differing only
  in its `entry` line, so another std entry would add bytes, not coverage;
- the programs the host's `ignis doc` tests documented, each written into a
  scratch directory and named relative to it, so the `entry` field and the
  module name do not depend on where the scratch directory is.

A difference prints a unified diff from the baseline to the output, cut at
`--max-diff-lines` lines, and the script exits 1. So does a target without a
baseline, and a baseline no target writes. Every target identical exits 0.

`--write-baselines` regenerates every baseline from `--compiler` and removes
the orphaned ones. Nothing is written unless every target exits 0. Review the
diff: it is a change to the published API package.

Usage:
  python3 scripts/selfhost_doc_parity.py --compiler build/bootstrap/stage2/ignis
  python3 scripts/selfhost_doc_parity.py --compiler build/bootstrap/stage2/ignis --write-baselines
"""

import argparse
import difflib
import re
import subprocess
import sys
import tempfile
from concurrent.futures import ThreadPoolExecutor
from dataclasses import dataclass
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent

# Where the committed outputs live, relative to the repository root. The files
# are `.json`, so the G6 walk over every `.ign` under `test_cases/` skips them.
BASELINE_DIR = "test_cases/doc/__doc_baselines__"

# The invocation `.github/workflows/nightly.yml` and `release.yml` publish.
CI_TARGET = "std/io/mod.ign"

# The programs the host's `ignis doc` tests documented, keyed by the file stem
# each one is written under, since the stem is the module name.
FIXTURES = {
  "adds": """
/// Adds two numbers.
export function add(a: i32, b: i32): i32 {
  return a + b;
}
""",
  "helper": """
/// Not exported.
function helper(): i32 {
  return 0;
}
""",
  "counter": """
/// A counter.
export record Counter {
  /// How far it has counted.
  public value: i32;

  get(&self): i32 {
    return self.value;
  }

  public static new(start: i32): Counter {
    return Counter { value: start };
  }
}
""",
  "outcome": """
/// A result of sorts.
export enum Outcome {
  DONE(i32),
  FAILED,
}
""",
  "math": """
namespace Math {
  /// Adds two numbers.
  function add(a: i32, b: i32): i32 {
    return a + b;
  }
}
""",
  "described": """
//! # The module
//!
//! What the file as a whole is for.

/// A function.
export function noop(): void {
  return;
}
""",
  "written": "export function noop(): void {\n  return;\n}\n",
}


@dataclass(frozen=True)
class Target:
  """One `ignis doc` invocation: its argument, the directory it runs in, and
  whether the package is written through `--output` instead of stdout."""

  label: str
  cwd: Path
  argument: str
  output_file: bool
  fixture: bool = False


@dataclass(frozen=True)
class RunResult:
  """What one compiler produced for one target."""

  exit_code: int
  payload: bytes
  stderr: str


@dataclass(frozen=True)
class Comparison:
  label: str
  identical: bool
  detail: str


def std_targets(repo_root: Path) -> list:
  """The one standard-library target: the entry the workflows publish,
  written through `--output` the way they write it."""
  return [Target(label=f"{CI_TARGET} (--output)", cwd=repo_root, argument=CI_TARGET, output_file=True)]


def fixture_targets(workdir: Path) -> list:
  """Writes every fixture into `workdir` and returns one target per file."""
  targets = []

  for name, source in FIXTURES.items():
    (workdir / f"{name}.ign").write_text(source, encoding="utf-8")
    targets.append(
      Target(label=f"fixture {name}", cwd=workdir, argument=f"{name}.ign", output_file=False, fixture=True)
    )

  return targets


def baseline_name(target: Target) -> str:
  """The baseline file for `target`: the argument flattened to one name,
  prefixed by `fixture_` or `output__` so no two targets share a file."""
  stem = re.sub(r"[^A-Za-z0-9]+", "_", target.argument.removesuffix(".ign")).strip("_")

  if target.fixture:
    return f"fixture_{stem}.json"

  return f"output__{stem}.json"


def run_doc(compiler: str, target: Target, std_path: Path, output_path: Path) -> RunResult:
  """Runs `compiler doc` for `target`. The payload is stdout, or the file
  `--output` wrote, which is empty when the compiler wrote none."""
  command = [compiler, "doc", target.argument, "--std-path", str(std_path)]

  if target.output_file:
    if output_path.exists():
      output_path.unlink()

    command += ["--output", str(output_path)]

  completed = subprocess.run(command, cwd=target.cwd, capture_output=True, check=False)
  payload = completed.stdout

  if target.output_file:
    payload = output_path.read_bytes() if output_path.exists() else b""

  return RunResult(
    exit_code=completed.returncode,
    payload=payload,
    stderr=completed.stderr.decode("utf-8", errors="replace"),
  )


def compare(label: str, baseline: bytes | None, selfhost: RunResult, max_diff_lines: int) -> Comparison:
  """Identical means a zero exit code and exactly the baseline's bytes."""
  if baseline is None:
    return Comparison(label=label, identical=False, detail="no baseline for this target")

  if selfhost.exit_code != 0:
    detail = f"exit code: selfhost {selfhost.exit_code}, baseline recorded 0"

    if selfhost.stderr.strip():
      detail += "\nselfhost stderr:\n" + selfhost.stderr.strip()

    return Comparison(label=label, identical=False, detail=detail)

  if baseline == selfhost.payload:
    return Comparison(label=label, identical=True, detail="")

  return Comparison(label=label, identical=False, detail=diff_text(baseline, selfhost.payload, max_diff_lines))


def diff_text(baseline: bytes, selfhost: bytes, max_diff_lines: int) -> str:
  """A unified diff from the baseline to the selfhost's output, cut after
  `max_diff_lines` lines. A byte difference no line shows, such as a trailing
  newline, is reported by length."""
  baseline_lines = baseline.decode("utf-8", errors="replace").splitlines(keepends=True)
  selfhost_lines = selfhost.decode("utf-8", errors="replace").splitlines(keepends=True)
  diff = list(difflib.unified_diff(baseline_lines, selfhost_lines, fromfile="baseline", tofile="selfhost"))

  if [line.rstrip("\n") for line in baseline_lines] == [line.rstrip("\n") for line in selfhost_lines]:
    return f"same lines, different bytes: baseline {len(baseline)} bytes, selfhost {len(selfhost)} bytes"

  shown = [line if line.endswith("\n") else line + "\n" for line in diff[:max_diff_lines]]

  if len(diff) > max_diff_lines:
    shown.append(f"... {len(diff) - max_diff_lines} more diff lines\n")

  return "".join(shown).rstrip("\n")


def format_report(comparisons: list) -> str:
  """One `identical`/`different` line per target, each difference followed by
  its detail, then a summary line."""
  lines = []

  for comparison in comparisons:
    verdict = "identical" if comparison.identical else "different"
    lines.append(f"{verdict}: {comparison.label}")

    if not comparison.identical:
      lines.extend(f"    {line}" for line in comparison.detail.splitlines())

  different = sum(1 for comparison in comparisons if not comparison.identical)
  lines.append(f"summary: {len(comparisons) - different} identical, {different} different")
  return "\n".join(lines)


def read_baseline(baseline_dir: Path, target: Target) -> bytes | None:
  path = baseline_dir / baseline_name(target)
  return path.read_bytes() if path.is_file() else None


def orphaned_baselines(baseline_dir: Path, targets: list) -> list:
  """Baseline files no target writes, by name."""
  if not baseline_dir.is_dir():
    return []

  expected = {baseline_name(target) for target in targets}
  return sorted(path.name for path in baseline_dir.glob("*.json") if path.name not in expected)


def write_baselines(baseline_dir: Path, targets: list, results: list) -> int:
  """Record every output, or nothing when any target failed."""
  failed = [(target, result) for target, result in zip(targets, results) if result.exit_code != 0]

  if failed:
    for target, result in failed:
      print(f"error: {target.label} exited {result.exit_code}", file=sys.stderr)

      if result.stderr.strip():
        print(result.stderr.strip(), file=sys.stderr)

    print(f"{len(failed)} of {len(targets)} targets failed; no baseline was written or removed", file=sys.stderr)
    return 1

  baseline_dir.mkdir(parents=True, exist_ok=True)
  orphans = orphaned_baselines(baseline_dir, targets)

  for target, result in zip(targets, results):
    (baseline_dir / baseline_name(target)).write_bytes(result.payload)

  for name in orphans:
    (baseline_dir / name).unlink()

  print(f"wrote {len(targets)} baselines under {baseline_dir}, removed {len(orphans)} orphaned")
  return 0


def parse_arguments(argv: list) -> argparse.Namespace:
  parser = argparse.ArgumentParser(
    description="Compare a selfhost compiler's `ignis doc` output with committed baselines."
  )
  parser.add_argument("--compiler", required=True, help="selfhost compiler binary")
  parser.add_argument(
    "--baselines",
    default=str(REPO_ROOT / BASELINE_DIR),
    help=f"committed outputs, one per target (default: {BASELINE_DIR})",
  )
  parser.add_argument(
    "--write-baselines",
    action="store_true",
    help="regenerate the baselines from --compiler and exit; review the diff, it changes the published API package",
  )
  parser.add_argument("--std-path", default=str(REPO_ROOT / "std"), help="standard library root (default: the repository's)")
  parser.add_argument("--jobs", type=int, default=4, help="targets documented at once (default: 4)")
  parser.add_argument("--max-diff-lines", type=int, default=40, help="diff lines shown per difference (default: 40)")
  return parser.parse_args(argv)


def resolve_binary(value: str) -> str:
  """A path with a separator is resolved against the working directory, since
  every target runs in a directory of its own; a bare name is left to PATH."""
  if "/" in value:
    return str(Path(value).resolve())

  return value


def main(argv: list) -> int:
  arguments = parse_arguments(argv)
  compiler = resolve_binary(arguments.compiler)
  std_path = Path(arguments.std_path).resolve()
  baseline_dir = Path(arguments.baselines).resolve()

  with tempfile.TemporaryDirectory(prefix="ignis-doc-parity-") as scratch_text:
    scratch = Path(scratch_text)
    fixture_dir = scratch / "fixtures"
    fixture_dir.mkdir()

    targets = std_targets(REPO_ROOT) + fixture_targets(fixture_dir)

    with ThreadPoolExecutor(max_workers=max(1, arguments.jobs)) as pool:
      results = list(
        pool.map(
          lambda indexed: run_doc(compiler, indexed[1], std_path, scratch / f"output-{indexed[0]}.json"),
          enumerate(targets),
        )
      )

  if arguments.write_baselines:
    return write_baselines(baseline_dir, targets, results)

  comparisons = [
    compare(target.label, read_baseline(baseline_dir, target), result, arguments.max_diff_lines)
    for target, result in zip(targets, results)
  ]
  comparisons += [
    Comparison(label=f"orphaned baseline {name}", identical=False, detail="no target writes this baseline")
    for name in orphaned_baselines(baseline_dir, targets)
  ]

  print(format_report(comparisons))
  return 0 if all(comparison.identical for comparison in comparisons) else 1


if __name__ == "__main__":
  sys.exit(main(sys.argv[1:]))
