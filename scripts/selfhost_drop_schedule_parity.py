#!/usr/bin/env python3
"""Compare the two compilers' `--dump-drop-schedule` output, case by case (gate G7).

Ownership bugs this quarter have all had the same shape: a binding that was or
was not dropped depending on the scrutinee, the construct, the pattern and the
arm. `--dump-drop-schedule` prints, per function, every owned value with its
declaration site and every scheduled drop site with its reason, in a fixed
order. Two compilers that agree on ownership therefore print the same bytes,
and this harness turns that into a gate: it runs the host and the selfhost over
the `ok` end-to-end corpus plus the selfhost compiler's own entry point, diffs
the two dumps per case, and reports the first differing lines.

The gate is **informational**. It does not feed the promotion `candidate`
verdict — a drop-schedule divergence is a lead to follow, not a release
blocker, and until the selfhost's ownership analysis is finished a non-zero
count is the expected state.

Case discovery is reused verbatim from `selfhost_e2e_parity.py`, so G7 and G2
always run over the same corpus.
"""

import argparse
import json
import subprocess
import sys
from concurrent.futures import ThreadPoolExecutor
from dataclasses import dataclass, field
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))

from selfhost_e2e_parity import CORPUS_OK, FIXTURE_DIR, extract_fixture_cases  # noqa: E402

GATE_ID = "G7"

DUMP_HEADER = "drop-schedule v1"

HOST_TIMEOUT_SECONDS = 180
SELFHOST_TIMEOUT_SECONDS = 300

CLASS_PASS = "pass"
CLASS_DIFFERS = "differs"
CLASS_HOST_NO_DUMP = "host-no-dump"
CLASS_SELFHOST_NO_DUMP = "selfhost-no-dump"
CLASS_TIMEOUT = "timeout"

CLASS_ORDER = (
  CLASS_PASS,
  CLASS_DIFFERS,
  CLASS_SELFHOST_NO_DUMP,
  CLASS_HOST_NO_DUMP,
  CLASS_TIMEOUT,
)

# How much of a divergence the report shows per case. The point is to name the
# first place the two disagree, not to paste two whole dumps.
REPORTED_DIFF_LINES = 6

# How many diverging cases get their own detail section.
REPORTED_CASES = 25


@dataclass
class DropCase:
  name: str
  path: Path
  # A project case is checked from its own root with no file argument, the way
  # `ignis check` compiles a project. `ignis/main.ign` only resolves its
  # `@compiler::*` imports that way, so the selfhost compiler itself is one.
  project_root: Path | None = None


@dataclass
class DropResult:
  case: DropCase
  classification: str
  reason: str = ""
  host_lines: int = 0
  selfhost_lines: int = 0
  first_difference: int | None = None
  diff_lines: list[str] = field(default_factory=list)


def collect_cases(
  repository_root: Path,
  extra_paths: list[Path],
  project_roots: list[Path],
  name_filter: str | None,
) -> list[DropCase]:
  """Every `ok` fixture, plus whatever entry points the caller added."""
  fixture_dir = repository_root / FIXTURE_DIR[CORPUS_OK]
  cases = [
    DropCase(case.name, fixture_dir / f"{case.name}.ign") for case in extract_fixture_cases(CORPUS_OK, repository_root)
  ]

  for path in extra_paths:
    resolved = path if path.is_absolute() else repository_root / path
    cases.append(DropCase(path.as_posix(), resolved))

  for root in project_roots:
    resolved = (root if root.is_absolute() else repository_root / root).resolve()
    manifest = resolved / "ignis.toml"
    name = resolved.relative_to(repository_root).as_posix() if resolved != repository_root else "."
    cases.append(DropCase(f"project:{name}", manifest, project_root=resolved))

  if name_filter:
    cases = [case for case in cases if name_filter in case.name]

  return cases


def run_dump(
  compiler: Path,
  case: DropCase,
  std_path: Path,
  repository_root: Path,
  timeout: int,
) -> tuple[list[str] | None, str]:
  """Run one compiler over one case and return the dump's lines.

  Both compilers print the dump on stdout after everything else they log, so
  the dump is whatever follows the last `drop-schedule v1` header line.
  """
  command = [
    str(compiler),
    "check",
    "--dump-drop-schedule",
    "--quiet",
    "--std-path",
    str(std_path),
  ]

  if case.project_root is None:
    command.append(str(case.path))

  try:
    completed = subprocess.run(
      command,
      cwd=case.project_root or repository_root,
      capture_output=True,
      text=True,
      timeout=timeout,
    )
  except subprocess.TimeoutExpired:
    return None, f"timed out after {timeout}s"
  except OSError as error:
    return None, f"could not run {compiler}: {error}"

  lines = completed.stdout.splitlines()

  if DUMP_HEADER not in lines:
    tail = "\n".join((completed.stderr or completed.stdout).splitlines()[-3:])
    return None, f"no dump on stdout (exit {completed.returncode}){': ' + tail if tail else ''}"

  start = len(lines) - 1 - lines[::-1].index(DUMP_HEADER)

  return lines[start:], ""


def compare(
  host_lines: list[str],
  selfhost_lines: list[str],
) -> tuple[int | None, list[str]]:
  """First differing line number (1-based) and a short excerpt around it."""
  for index in range(max(len(host_lines), len(selfhost_lines))):
    host_line = host_lines[index] if index < len(host_lines) else "<end of dump>"
    selfhost_line = selfhost_lines[index] if index < len(selfhost_lines) else "<end of dump>"

    if host_line == selfhost_line:
      continue

    excerpt = []
    for offset in range(min(REPORTED_DIFF_LINES, max(len(host_lines), len(selfhost_lines)) - index)):
      position = index + offset
      host_at = host_lines[position] if position < len(host_lines) else "<end of dump>"
      selfhost_at = selfhost_lines[position] if position < len(selfhost_lines) else "<end of dump>"

      if host_at == selfhost_at:
        excerpt.append(f"  {position + 1}   {host_at}")
      else:
        excerpt.append(f"  {position + 1} - {host_at}")
        excerpt.append(f"  {position + 1} + {selfhost_at}")

    return index + 1, excerpt

  return None, []


def run_case(
  case: DropCase,
  host: Path,
  compiler: Path,
  std_path: Path,
  repository_root: Path,
) -> DropResult:
  if not case.path.is_file():
    return DropResult(case, CLASS_HOST_NO_DUMP, reason=f"{case.path} does not exist")

  host_lines, host_error = run_dump(host, case, std_path, repository_root, HOST_TIMEOUT_SECONDS)

  if host_lines is None:
    classification = CLASS_TIMEOUT if "timed out" in host_error else CLASS_HOST_NO_DUMP
    return DropResult(case, classification, reason=host_error)

  selfhost_lines, selfhost_error = run_dump(
    compiler,
    case,
    std_path,
    repository_root,
    SELFHOST_TIMEOUT_SECONDS,
  )

  if selfhost_lines is None:
    classification = CLASS_TIMEOUT if "timed out" in selfhost_error else CLASS_SELFHOST_NO_DUMP
    return DropResult(
      case,
      classification,
      reason=selfhost_error,
      host_lines=len(host_lines),
    )

  first_difference, diff_lines = compare(host_lines, selfhost_lines)

  return DropResult(
    case,
    CLASS_PASS if first_difference is None else CLASS_DIFFERS,
    host_lines=len(host_lines),
    selfhost_lines=len(selfhost_lines),
    first_difference=first_difference,
    diff_lines=diff_lines,
  )


def build_report(
  results: list[DropResult],
  counts: dict[str, int],
  compiler: Path,
  host: Path,
) -> str:
  total = len(results)
  lines = [
    "# Drop-schedule parity (gate G7)",
    "",
    "`--dump-drop-schedule` renders, per function, every owned value with its",
    "declaration site and every scheduled drop site with its reason. Both",
    "compilers print the same format, so their dumps are compared byte for byte.",
    "",
    "**This gate is informational**: it does not affect the promotion `candidate`",
    "verdict. A differing case is a lead for an ownership bug, not a blocker.",
    "",
    f"- host: `{host}`",
    f"- selfhost: `{compiler}`",
    f"- cases: {total}",
    "",
    "## Summary",
    "",
    "| result | cases |",
    "| --- | --- |",
  ]

  for classification in CLASS_ORDER:
    lines.append(f"| {classification} | {counts.get(classification, 0)} |")

  diverging = [result for result in results if result.classification != CLASS_PASS]

  if not diverging:
    lines += ["", "Every case renders an identical drop schedule in both compilers."]
    return "\n".join(lines) + "\n"

  lines += ["", "## Diverging cases", "", "| case | result | first differing line |", "| --- | --- | --- |"]

  for result in diverging:
    position = str(result.first_difference) if result.first_difference else result.reason or "-"
    lines.append(f"| `{result.case.name}` | {result.classification} | {position} |")

  lines += ["", "## First difference per case", ""]

  for result in diverging[:REPORTED_CASES]:
    lines += [f"### `{result.case.name}` ({result.classification})", ""]

    if result.diff_lines:
      lines += [
        f"host {result.host_lines} lines, selfhost {result.selfhost_lines} lines; "
        f"first difference at line {result.first_difference} (`-` host, `+` selfhost):",
        "",
        "```diff",
        *result.diff_lines,
        "```",
        "",
      ]
    else:
      lines += [result.reason or "no detail was recorded.", ""]

  if len(diverging) > REPORTED_CASES:
    lines.append(f"_{len(diverging) - REPORTED_CASES} further diverging cases are listed in the table above._")

  return "\n".join(lines) + "\n"


def build_gate(
  results: list[DropResult],
  counts: dict[str, int],
) -> dict:
  total = len(results)
  passed = counts.get(CLASS_PASS, 0)

  return {
    "gate": GATE_ID,
    "status": "pass" if total > 0 and passed == total else "fail",
    "informational": True,
    "summary": f"drop-schedule parity {passed}/{total} (informational: does not gate promotion)",
    "details": {
      "informational": True,
      "total": total,
      "counts": {classification: counts.get(classification, 0) for classification in CLASS_ORDER},
      "diverging": [
        {
          "case": result.case.name,
          "classification": result.classification,
          "first_difference": result.first_difference,
          "reason": result.reason,
        }
        for result in results
        if result.classification != CLASS_PASS
      ],
    },
  }


def main() -> int:
  repository_root = Path(__file__).resolve().parent.parent

  parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
  parser.add_argument("--compiler", required=True, type=Path, help="selfhost-built compiler binary")
  parser.add_argument("--host", default="ignis", type=Path, help="host compiler binary")
  parser.add_argument("--std", type=Path, default=repository_root / "std", help="standard library directory")
  parser.add_argument(
    "--extra",
    type=Path,
    action="append",
    default=[],
    help="additional single-file entry point to dump, repeatable",
  )
  parser.add_argument(
    "--project",
    type=Path,
    action="append",
    default=[],
    help="project root to check as a whole, repeatable (e.g. `.` for the selfhost compiler)",
  )
  parser.add_argument("--filter", help="only run cases whose name contains this substring")
  parser.add_argument("--jobs", type=int, default=None, help="parallel cases (default: cpu count)")
  parser.add_argument("--report", type=Path, help="markdown report path")
  parser.add_argument("--counts-json", type=Path, help="per-class counts JSON path")
  parser.add_argument("--gate-json", type=Path, help="bootstrap gate result path")

  arguments = parser.parse_args()

  cases = collect_cases(repository_root, arguments.extra, arguments.project, arguments.filter)

  if not cases:
    print("no cases were discovered", file=sys.stderr)
    return 1

  # Both compilers build the standard library into the same `build/std` on
  # first use; a cold pool would have every worker racing to write the same
  # archive. One warm-up compile per compiler makes that build already done.
  for compiler, timeout in ((arguments.host, HOST_TIMEOUT_SECONDS), (arguments.compiler, SELFHOST_TIMEOUT_SECONDS)):
    run_dump(compiler, cases[0], arguments.std, repository_root, timeout)

  with ThreadPoolExecutor(max_workers=arguments.jobs) as executor:
    results = list(
      executor.map(
        lambda case: run_case(case, arguments.host, arguments.compiler, arguments.std, repository_root),
        cases,
      )
    )

  results.sort(key=lambda result: result.case.name)

  counts: dict[str, int] = {}
  for result in results:
    counts[result.classification] = counts.get(result.classification, 0) + 1

  gate = build_gate(results, counts)

  if arguments.report:
    arguments.report.parent.mkdir(parents=True, exist_ok=True)
    arguments.report.write_text(build_report(results, counts, arguments.compiler, arguments.host), encoding="utf-8")

  if arguments.counts_json:
    arguments.counts_json.parent.mkdir(parents=True, exist_ok=True)
    arguments.counts_json.write_text(
      json.dumps({"total": len(results), "counts": counts}, indent=2) + "\n",
      encoding="utf-8",
    )

  if arguments.gate_json:
    arguments.gate_json.parent.mkdir(parents=True, exist_ok=True)
    arguments.gate_json.write_text(json.dumps(gate, indent=2) + "\n", encoding="utf-8")

  print(
    "drop-schedule parity: "
    + ", ".join(f"{classification} {counts.get(classification, 0)}" for classification in CLASS_ORDER)
  )

  # The gate is informational: a divergence is reported, never fatal.
  return 0


if __name__ == "__main__":
  raise SystemExit(main())
