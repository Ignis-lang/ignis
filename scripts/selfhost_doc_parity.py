#!/usr/bin/env python3
"""Compare `ignis doc` between a selfhost compiler and the host, byte for byte.

`ignis doc` prints an API package as pretty JSON, and the release and nightly
workflows publish the one `ignis doc std/io/mod.ign --std-path std --output
docs-pkg/api.json` writes. The selfhost port has to produce the same bytes, so
this script runs both compilers on the same targets and compares exactly what
each one wrote, exit code included:

- the workflows' own invocation, `std/io/mod.ign` written through `--output`;
- every standard-library module entry, `std/<name>/mod.ign`, printed to stdout;
- the programs the host's `crates/ignis/tests/doc_command.rs` documents, each
  written into a scratch directory and named relative to it, so the `entry`
  field and the module name are the same for both compilers.

A difference prints a unified diff of the two outputs, cut at `--max-diff-lines`
lines, and the script exits 1. Every target identical exits 0.

Usage:
  python3 scripts/selfhost_doc_parity.py --compiler build/bootstrap/stage2/ignis --host ignis
"""

import argparse
import difflib
import subprocess
import sys
import tempfile
from concurrent.futures import ThreadPoolExecutor
from dataclasses import dataclass
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent

# The invocation `.github/workflows/nightly.yml` and `release.yml` publish.
CI_TARGET = "std/io/mod.ign"

# The programs `crates/ignis/tests/doc_command.rs` documents, keyed by the file
# stem each one is written under, since the stem is the module name.
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
  """The workflows' target first, through `--output`, then every std module
  entry printed to stdout, sorted by path."""
  targets = [Target(label=f"{CI_TARGET} (--output)", cwd=repo_root, argument=CI_TARGET, output_file=True)]

  for entry in sorted((repo_root / "std").glob("*/mod.ign")):
    relative = entry.relative_to(repo_root).as_posix()
    targets.append(Target(label=relative, cwd=repo_root, argument=relative, output_file=False))

  return targets


def fixture_targets(workdir: Path) -> list:
  """Writes every fixture into `workdir` and returns one target per file."""
  targets = []

  for name, source in FIXTURES.items():
    (workdir / f"{name}.ign").write_text(source, encoding="utf-8")
    targets.append(Target(label=f"fixture {name}", cwd=workdir, argument=f"{name}.ign", output_file=False))

  return targets


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


def compare(label: str, host: RunResult, selfhost: RunResult, max_diff_lines: int) -> Comparison:
  """Identical means the same exit code and the same bytes."""
  if host.exit_code != selfhost.exit_code:
    detail = f"exit code: host {host.exit_code}, selfhost {selfhost.exit_code}"

    if selfhost.stderr.strip():
      detail += "\nselfhost stderr:\n" + selfhost.stderr.strip()

    return Comparison(label=label, identical=False, detail=detail)

  if host.payload == selfhost.payload:
    return Comparison(label=label, identical=True, detail="")

  return Comparison(label=label, identical=False, detail=diff_text(host.payload, selfhost.payload, max_diff_lines))


def diff_text(host: bytes, selfhost: bytes, max_diff_lines: int) -> str:
  """A unified diff from the host's output to the selfhost's, cut after
  `max_diff_lines` lines. A byte difference no line shows, such as a trailing
  newline, is reported by length."""
  host_lines = host.decode("utf-8", errors="replace").splitlines(keepends=True)
  selfhost_lines = selfhost.decode("utf-8", errors="replace").splitlines(keepends=True)
  diff = list(difflib.unified_diff(host_lines, selfhost_lines, fromfile="host", tofile="selfhost"))

  if [line.rstrip("\n") for line in host_lines] == [line.rstrip("\n") for line in selfhost_lines]:
    return f"same lines, different bytes: host {len(host)} bytes, selfhost {len(selfhost)} bytes"

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


def compare_target(
  target: Target,
  index: int,
  compiler: str,
  host: str,
  std_path: Path,
  scratch: Path,
  max_diff_lines: int,
) -> Comparison:
  host_result = run_doc(host, target, std_path, scratch / f"host-{index}.json")
  selfhost_result = run_doc(compiler, target, std_path, scratch / f"selfhost-{index}.json")
  return compare(target.label, host_result, selfhost_result, max_diff_lines)


def parse_arguments(argv: list) -> argparse.Namespace:
  parser = argparse.ArgumentParser(description="Compare `ignis doc` output between a selfhost compiler and the host.")
  parser.add_argument("--compiler", required=True, help="selfhost compiler binary")
  parser.add_argument("--host", required=True, help="host compiler binary")
  parser.add_argument("--std-path", default=str(REPO_ROOT / "std"), help="standard library root (default: the repository's)")
  parser.add_argument("--jobs", type=int, default=4, help="targets compared at once (default: 4)")
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
  host = resolve_binary(arguments.host)
  std_path = Path(arguments.std_path).resolve()

  with tempfile.TemporaryDirectory(prefix="ignis-doc-parity-") as scratch_text:
    scratch = Path(scratch_text)
    fixture_dir = scratch / "fixtures"
    fixture_dir.mkdir()

    targets = std_targets(REPO_ROOT) + fixture_targets(fixture_dir)

    with ThreadPoolExecutor(max_workers=max(1, arguments.jobs)) as pool:
      comparisons = list(
        pool.map(
          lambda indexed: compare_target(
            indexed[1],
            indexed[0],
            compiler,
            host,
            std_path,
            scratch,
            arguments.max_diff_lines,
          ),
          enumerate(targets),
        )
      )

  print(format_report(comparisons))
  return 0 if all(comparison.identical for comparison in comparisons) else 1


if __name__ == "__main__":
  sys.exit(main(sys.argv[1:]))
