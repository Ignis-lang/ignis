#!/usr/bin/env python3
"""Check a compiler's `--dump-drop-schedule` output against committed baselines (gate G7).

Ownership bugs this quarter have all had the same shape: a binding that was or
was not dropped depending on the scrutinee, the construct, the pattern and the
arm. `--dump-drop-schedule` prints, per function, every owned value with its
declaration site and every scheduled drop site with its reason, in a fixed
order. A compiler whose ownership analysis is right therefore prints the same
bytes as the one that produced the baselines, and this harness turns that into
a gate: it runs the compiler under test over the `ok` end-to-end corpus and
diffs each case's dump against `test_cases/e2e/ok/__drop_schedules__/<case>.txt`.

The baselines are the reason this gate survives the host freeze. It used to
run the Rust host compiler as a live oracle on every case; the baselines were
generated from that host while it still existed, verified byte-identical
against the selfhost compiler, and committed. From now on they change only
when a developer regenerates them deliberately, and a baseline diff in a pull
request is a semantic change a reviewer has to read.

Only the code under test is compared: every function the compiler places in
the **standard library** is dropped from the dump first. Which std functions a
monomorphizer keeps alive is a mono question rather than a drop question, and
including them would make every baseline churn on an unrelated mono change.
The count of dropped std functions is reported, not gated.

Two kinds of case never get a baseline:

- `--project` roots (the selfhost compiler compiling itself) and `--extra`
  entry points. A project baseline would change with nearly every compiler
  commit, which makes it noise rather than evidence. Those cases are compared
  against a second compiler given with `--reference` instead — in the gate,
  stage2's dump of `ignis/` against stage1's, a self-consistency check on the
  same sources.

`--host` stays available as a cross-check (does the host still agree with the
baselines?) and `--host-compare` still performs the original direct
host-vs-selfhost diff, so the move to baselines is reversible for as long as
the host exists.

The gate feeds the promotion `candidate` verdict: a differing case is a real
ownership bug, not just a lead to follow.

Case discovery is reused verbatim from `selfhost_e2e_parity.py`, so G7 and G2
always run over the same corpus.
"""

import argparse
import json
import os
import subprocess
import sys
from concurrent.futures import ThreadPoolExecutor
from dataclasses import dataclass, field
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))

from selfhost_e2e_parity import CORPUS_OK, FIXTURE_DIR, extract_fixture_cases  # noqa: E402

GATE_ID = "G7"

DUMP_HEADER = "drop-schedule v1"

# Where the committed per-case dumps live, relative to the repository root, and
# what one is called. Deliberately next to the `__snapshots__` the same corpus
# already carries, so a fixture and everything recorded about it sit together.
BASELINE_DIR = "test_cases/e2e/ok/__drop_schedules__"
BASELINE_SUFFIX = ".txt"

REGENERATE_HINT = "regenerate with `scripts/selfhost_drop_schedule_parity.py --compiler <bin> --write-baselines`"

# Every line shape the renderer can emit after its header. A line that starts
# with none of these ends the dump.
DUMP_LINE_PREFIXES = (
  "function ",
  "  value ",
  "    drop at ",
  "    moved at ",
  "  defer ",
  "  <no owned values>",
)

COMPILER_TIMEOUT_SECONDS = 300

# A project-root case (`--project`) compiles the whole project from its own
# root rather than one fixture file — G7's own `project:.` case is the
# selfhost compiler compiling itself, ~98 CPU-s of work. That is fine in
# isolation but the fixture budget above is not: under contention (a 4-vCPU
# CI runner running every case's `ThreadPoolExecutor` worker at once) it was
# observed at 220s wall against the 300s selfhost budget, close enough to
# time out on a slower or busier runner. Project cases get their own, much
# larger budget instead of raising the fixture budget for every other case.
PROJECT_TIMEOUT_SECONDS = 900

CLASS_PASS = "pass"
CLASS_DIFFERS = "differs"
CLASS_BASELINE_MISSING = "baseline-missing"
CLASS_NO_DUMP = "no-dump"
CLASS_REFERENCE_NO_DUMP = "reference-no-dump"
CLASS_TIMEOUT = "timeout"

CLASS_ORDER = (
  CLASS_PASS,
  CLASS_DIFFERS,
  CLASS_BASELINE_MISSING,
  CLASS_NO_DUMP,
  CLASS_REFERENCE_NO_DUMP,
  CLASS_TIMEOUT,
)

RETRYABLE_CLASSES = (CLASS_NO_DUMP, CLASS_REFERENCE_NO_DUMP, CLASS_TIMEOUT)

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
  # A corpus fixture is checked against its committed baseline; anything else
  # is checked against the `--reference` compiler.
  has_baseline: bool = True


@dataclass
class Settings:
  compiler: Path
  std_path: Path
  repository_root: Path
  baseline_dir: Path
  reference: Path | None = None
  host: Path | None = None
  host_compare: bool = False
  compare_everything: bool = False
  # What the gate file calls itself. The pull-request run against stage1
  # writes `G7-STAGE1`, which the promotion report lists as an unscored row
  # instead of confusing it with the ladder's own G7.
  gate_id: str = GATE_ID


@dataclass
class DropResult:
  case: DropCase
  classification: str
  reason: str = ""
  expected_lines: int = 0
  actual_lines: int = 0
  first_difference: int | None = None
  diff_lines: list[str] = field(default_factory=list)
  # Observed on the whole dump even though the comparison is scoped, so the
  # report can say how much of it the scoping removed.
  std_functions: int = 0
  reference_std_functions: int = 0
  # Only set when `--host` asked for the cross-check: "pass", "differs", or a
  # reason the host produced nothing.
  host_cross_check: str | None = None


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
    cases.append(DropCase(path.as_posix(), resolved, has_baseline=False))

  for root in project_roots:
    resolved = (root if root.is_absolute() else repository_root / root).resolve()
    manifest = resolved / "ignis.toml"
    name = resolved.relative_to(repository_root).as_posix() if resolved != repository_root else "."
    cases.append(DropCase(f"project:{name}", manifest, project_root=resolved, has_baseline=False))

  if name_filter:
    cases = [case for case in cases if name_filter in case.name]

  return cases


def timeout_for(case: DropCase) -> int:
  """How long one compiler gets on one case, in seconds."""
  if case.project_root is not None:
    return PROJECT_TIMEOUT_SECONDS

  return COMPILER_TIMEOUT_SECONDS


def baseline_path(
  baseline_dir: Path,
  case: DropCase,
) -> Path:
  return baseline_dir / f"{case.name}{BASELINE_SUFFIX}"


def relative_to(
  path: Path,
  root: Path,
) -> str:
  try:
    return path.resolve().relative_to(root.resolve()).as_posix()
  except ValueError:
    return str(path)


def run_dump(
  compiler: Path,
  case: DropCase,
  std_path: Path,
  repository_root: Path,
  timeout: int,
) -> tuple[list[str] | None, str]:
  """Run one compiler over one case and return the dump's lines.

  The dump starts at the last `drop-schedule v1` header on stdout and ends at
  the first line that is not part of the dump grammar, so whatever the
  compiler logs around it — `- Scanning & parsing`, phase lines, a trailing
  `No errors found` — stays out of the comparison. `--quiet` is deliberately
  not passed: the selfhost CLI has no such flag.
  """
  command = [
    str(compiler),
    "check",
    "--dump-drop-schedule",
    "--std-path",
    str(std_path),
  ]

  if case.project_root is None:
    # The path must stay relative to the working directory: given an absolute
    # path while a project root is in scope, the selfhost compiler ignores the
    # argument and compiles the project instead.
    command.append(relative_to(case.path, repository_root))

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
  dump = [DUMP_HEADER]

  for line in lines[start + 1 :]:
    if not line.startswith(DUMP_LINE_PREFIXES):
      break
    dump.append(line)

  return dump, ""


def scoped_dump(
  compiler: Path,
  case: DropCase,
  settings: Settings,
) -> tuple[list[str] | None, int, str]:
  """One compiler's dump for one case, narrowed to the code under test."""
  lines, error = run_dump(
    compiler,
    case,
    settings.std_path,
    settings.repository_root,
    timeout_for(case),
  )

  if lines is None:
    return None, 0, error

  std_prefix = f"{relative_to(settings.std_path, settings.repository_root)}/"
  scoped, std_functions = scope_to_own_code(lines, std_prefix)

  if settings.compare_everything:
    scoped = lines

  return scoped, std_functions, ""


def compare(
  expected_lines: list[str],
  actual_lines: list[str],
) -> tuple[int | None, list[str]]:
  """First differing line number (1-based) and a short excerpt around it."""
  for index in range(max(len(expected_lines), len(actual_lines))):
    expected_line = expected_lines[index] if index < len(expected_lines) else "<end of dump>"
    actual_line = actual_lines[index] if index < len(actual_lines) else "<end of dump>"

    if expected_line == actual_line:
      continue

    excerpt = []
    for offset in range(min(REPORTED_DIFF_LINES, max(len(expected_lines), len(actual_lines)) - index)):
      position = index + offset
      expected_at = expected_lines[position] if position < len(expected_lines) else "<end of dump>"
      actual_at = actual_lines[position] if position < len(actual_lines) else "<end of dump>"

      if expected_at == actual_at:
        excerpt.append(f"  {position + 1}   {expected_at}")
      else:
        excerpt.append(f"  {position + 1} - {expected_at}")
        excerpt.append(f"  {position + 1} + {actual_at}")

    return index + 1, excerpt

  return None, []


def split_functions(lines: list[str]) -> list[tuple[str, list[str]]]:
  """The dump's body as (function line, its indented lines) pairs."""
  functions: list[tuple[str, list[str]]] = []

  for line in lines:
    if line.startswith("function "):
      functions.append((line, []))
    elif functions:
      functions[-1][1].append(line)

  return functions


def function_path(function_line: str) -> str:
  """The path out of `function <name> at <path>:<line>:<column>`."""
  position = function_line.rsplit(" at ", 1)[-1]

  return position.rsplit(":", 2)[0]


def scope_to_own_code(
  lines: list[str],
  std_prefix: str,
) -> tuple[list[str], int]:
  """Drop every standard-library function, and count what was dropped.

  What remains is the code the case is actually about: the fixture's own
  functions, or the compiler's own functions for a project case.
  """
  scoped = [DUMP_HEADER]
  dropped = 0

  for function_line, body in split_functions(lines[1:]):
    if function_path(function_line).startswith(std_prefix):
      dropped += 1
      continue

    scoped.append(function_line)
    scoped.extend(body)

  return scoped, dropped


def expected_for(
  case: DropCase,
  settings: Settings,
) -> tuple[list[str] | None, int, str, str]:
  """What this case's dump is compared against: (lines, std count, source, error).

  `source` names where the expectation came from, for the report.
  """
  if settings.host_compare:
    assert settings.host is not None
    lines, std_functions, error = scoped_dump(settings.host, case, settings)

    return lines, std_functions, "host", error

  if not case.has_baseline:
    if settings.reference is None:
      return None, 0, "reference", "no --reference compiler was given for a --project/--extra case"

    lines, std_functions, error = scoped_dump(settings.reference, case, settings)

    return lines, std_functions, "reference", error

  path = baseline_path(settings.baseline_dir, case)

  if not path.is_file():
    return None, 0, "baseline", ""

  return path.read_text(encoding="utf-8").splitlines(), 0, "baseline", ""


def run_case(
  case: DropCase,
  settings: Settings,
) -> DropResult:
  if not case.path.is_file():
    return DropResult(case, CLASS_NO_DUMP, reason=f"{case.path} does not exist")

  expected, reference_std, source, expected_error = expected_for(case, settings)

  if expected is None and source == "baseline":
    return DropResult(
      case,
      CLASS_BASELINE_MISSING,
      reason=f"no baseline at {relative_to(baseline_path(settings.baseline_dir, case), settings.repository_root)}; "
      f"a new fixture must come with its baseline — {REGENERATE_HINT}",
    )

  if expected is None:
    classification = CLASS_TIMEOUT if "timed out" in expected_error else CLASS_REFERENCE_NO_DUMP
    return DropResult(case, classification, reason=f"{source}: {expected_error}")

  actual, std_functions, actual_error = scoped_dump(settings.compiler, case, settings)

  if actual is None:
    classification = CLASS_TIMEOUT if "timed out" in actual_error else CLASS_NO_DUMP
    return DropResult(case, classification, reason=actual_error, expected_lines=len(expected))

  first_difference, diff_lines = compare(expected, actual)

  result = DropResult(
    case,
    CLASS_PASS if first_difference is None else CLASS_DIFFERS,
    expected_lines=len(expected),
    actual_lines=len(actual),
    first_difference=first_difference,
    diff_lines=diff_lines,
    std_functions=std_functions,
    reference_std_functions=reference_std,
  )

  if settings.host is not None and not settings.host_compare and case.has_baseline:
    result.host_cross_check = cross_check_host(case, expected, settings)

  return result


def cross_check_host(
  case: DropCase,
  expected: list[str],
  settings: Settings,
) -> str:
  """Does the host still produce the committed baseline for this case?"""
  assert settings.host is not None
  host_lines, _, error = scoped_dump(settings.host, case, settings)

  if host_lines is None:
    return error or "the host produced no dump"

  first_difference, _ = compare(expected, host_lines)

  return CLASS_PASS if first_difference is None else CLASS_DIFFERS


def stale_baselines(
  settings: Settings,
  cases: list[DropCase],
) -> list[str]:
  """Committed baselines with no corresponding fixture, repo-relative."""
  if not settings.baseline_dir.is_dir():
    return []

  expected = {baseline_path(settings.baseline_dir, case).resolve() for case in cases if case.has_baseline}

  return sorted(
    relative_to(path, settings.repository_root)
    for path in settings.baseline_dir.rglob(f"*{BASELINE_SUFFIX}")
    if path.resolve() not in expected
  )


def missing_baselines(
  settings: Settings,
  cases: list[DropCase],
) -> list[str]:
  """Fixtures with no committed baseline, repo-relative baseline paths."""
  return sorted(
    relative_to(baseline_path(settings.baseline_dir, case), settings.repository_root)
    for case in cases
    if case.has_baseline and not baseline_path(settings.baseline_dir, case).is_file()
  )


def write_baselines(
  settings: Settings,
  cases: list[DropCase],
  jobs: int,
) -> int:
  """(Re)generate every fixture baseline from `--compiler`."""
  fixtures = [case for case in cases if case.has_baseline]

  if not fixtures:
    print("no fixture cases were discovered", file=sys.stderr)
    return 1

  warm_up(settings, fixtures[0])

  with ThreadPoolExecutor(max_workers=jobs) as executor:
    dumps = list(executor.map(lambda case: (case, *scoped_dump(settings.compiler, case, settings)), fixtures))

  failed = [(case, error) for case, lines, _, error in dumps if lines is None]

  for case, lines, _, _ in dumps:
    if lines is None:
      continue

    path = baseline_path(settings.baseline_dir, case)
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text("\n".join(lines) + "\n", encoding="utf-8")

  removed = stale_baselines(settings, cases)

  for name in removed:
    (settings.repository_root / name).unlink()

  written = len(dumps) - len(failed)
  total_bytes = sum(
    baseline_path(settings.baseline_dir, case).stat().st_size
    for case, lines, _, _ in dumps
    if lines is not None
  )

  print(
    f"wrote {written} baselines ({total_bytes / 1024:.0f} KiB) under "
    f"{relative_to(settings.baseline_dir, settings.repository_root)}, removed {len(removed)} stale"
  )

  for case, error in failed:
    print(f"  {case.name}: {error}", file=sys.stderr)

  return 1 if failed else 0


def warm_up(
  settings: Settings,
  case: DropCase,
) -> None:
  """Build `build/std` once per compiler before the pool starts.

  Both compilers build the standard library into the same `build/std` on first
  use; a cold pool would have every worker racing to write the same archive.
  """
  for compiler in (settings.compiler, settings.reference, settings.host):
    if compiler is None:
      continue

    run_dump(compiler, case, settings.std_path, settings.repository_root, timeout_for(case))


def build_report(
  results: list[DropResult],
  counts: dict[str, int],
  settings: Settings,
  stale: list[str],
) -> str:
  total = len(results)
  scope = (
    "the whole dump, standard library included (`--all`)"
    if settings.compare_everything
    else "the code under test; standard-library functions are dropped from the dump first"
  )
  against = (
    f"the host (`{settings.host}`), directly"
    if settings.host_compare
    else f"committed baselines under `{relative_to(settings.baseline_dir, settings.repository_root)}`"
  )
  lines = [
    "# Drop-schedule parity (gate G7)",
    "",
    "`--dump-drop-schedule` renders, per function, every owned value with its",
    "declaration site and every scheduled drop site with its reason. The dump",
    "is compared byte for byte against what was recorded for that case.",
    "",
    "**This gate feeds the promotion `candidate` verdict**: a differing case",
    "is a real ownership bug, not just a lead to follow.",
    "",
    f"- compiler under test: `{settings.compiler}`",
    f"- compared against: {against}",
  ]

  if settings.reference is not None:
    lines.append(f"- reference for `--project`/`--extra` cases: `{settings.reference}`")

  lines += [
    f"- cases: {total}",
    f"- compared: {scope}",
    "",
    "## Summary",
    "",
    "| result | cases |",
    "| --- | --- |",
  ]

  for classification in CLASS_ORDER:
    lines.append(f"| {classification} | {counts.get(classification, 0)} |")

  lines.extend(build_cross_check_section(results, settings))
  lines.extend(build_stale_section(stale))
  lines.extend(build_std_section(results, settings))

  diverging = [result for result in results if result.classification != CLASS_PASS]

  if not diverging and not stale:
    lines += ["", "Every case renders the drop schedule that was recorded for it."]
    return "\n".join(lines) + "\n"

  if not diverging:
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
        f"recorded {result.expected_lines} lines, produced {result.actual_lines} lines; "
        f"first difference at line {result.first_difference} (`-` recorded, `+` produced):",
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


def build_stale_section(stale: list[str]) -> list[str]:
  if not stale:
    return []

  return [
    "",
    "## Stale baselines",
    "",
    "These files record a case the corpus no longer has. A baseline with no",
    f"fixture is dead weight that can never fail, so it fails the gate instead — {REGENERATE_HINT}.",
    "",
    *[f"- `{name}`" for name in stale],
    "",
  ]


def build_cross_check_section(
  results: list[DropResult],
  settings: Settings,
) -> list[str]:
  """`--host`: does the host the baselines came from still agree with them?"""
  checked = [result for result in results if result.host_cross_check is not None]

  if not checked:
    return []

  disagreeing = [result for result in checked if result.host_cross_check != CLASS_PASS]

  lines = [
    "",
    "## Host cross-check",
    "",
    f"`{settings.host}` was run over the same cases and its dump compared with the",
    "committed baselines. This is not what the gate needs — the baselines are the",
    "reference now — but while the host exists it shows they still describe it.",
    "",
    f"- cases cross-checked: {len(checked)}",
    f"- cases where the host disagrees with its baseline: {len(disagreeing)}",
    "",
  ]

  for result in disagreeing[:REPORTED_CASES]:
    lines.append(f"- `{result.case.name}`: {result.host_cross_check}")

  return lines


def build_std_section(
  results: list[DropResult],
  settings: Settings,
) -> list[str]:
  """How much of each dump the scoping removed — reported, not gated."""
  if settings.compare_everything:
    return []

  comparable = [result for result in results if result.std_functions]

  if not comparable:
    return []

  widest = max(comparable, key=lambda result: result.std_functions)

  lines = [
    "",
    "## Standard-library functions (informational, not compared)",
    "",
    "Which standard-library functions the monomorphizer keeps alive is a mono",
    "question rather than a drop question, and it would make every baseline",
    "churn on an unrelated mono change. Those functions are dropped from the",
    "dump before comparing; how many there were is recorded here instead.",
    "",
    f"- cases with standard-library functions in their dump: {len(comparable)}/{len(results)}",
    f"- most: `{widest.case.name}`, {widest.std_functions} functions",
    "",
  ]

  disagreeing = [
    result
    for result in results
    if result.reference_std_functions and result.reference_std_functions != result.std_functions
  ]

  if disagreeing:
    lines += [
      f"- cases where the reference kept a different number alive: {len(disagreeing)}",
      "",
    ]

  return lines


def build_gate(
  results: list[DropResult],
  counts: dict[str, int],
  settings: Settings,
  stale: list[str],
  retried_timeouts: int = 0,
) -> dict:
  total = len(results)
  passed = counts.get(CLASS_PASS, 0)
  scope = "whole-dump" if settings.compare_everything else "own-code"
  source = "host" if settings.host_compare else "baseline"
  # A timeout still fails the gate (it is real evidence something took too
  # long, not proof of a divergence), but a night where every timeout cleared
  # on retry looks identical in the counts to one where a case is
  # consistently too slow, unless the summary says so: the promotion report
  # is the only place a maintainer sees this run without digging into logs.
  retry_plural = "s" if retried_timeouts != 1 else ""
  retry_note = f" ({retried_timeouts} timeout{retry_plural} retried)" if retried_timeouts else ""
  stale_plural = "s" if len(stale) != 1 else ""
  stale_note = f", {len(stale)} stale baseline{stale_plural}" if stale else ""

  cross_checked = [result for result in results if result.host_cross_check is not None]
  cross_check_failures = [result for result in cross_checked if result.host_cross_check != CLASS_PASS]

  cross_note = ""
  if cross_checked:
    cross_note = f", host cross-check {len(cross_checked) - len(cross_check_failures)}/{len(cross_checked)}"

  healthy = total > 0 and passed == total and not stale and not cross_check_failures

  gate = {
    "gate": settings.gate_id,
    "status": "pass" if healthy else "fail",
    "summary": f"drop-schedule parity {passed}/{total} {scope} vs {source}{stale_note}{cross_note}{retry_note}",
    "details": {
      "scope": scope,
      "compared_against": source,
      "timeouts_retried": retried_timeouts,
      "stale_baselines": stale,
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

  if cross_checked:
    gate["details"]["host_cross_check"] = {
      "checked": len(cross_checked),
      "disagreeing": [
        {"case": result.case.name, "detail": result.host_cross_check} for result in cross_check_failures
      ],
    }

  return gate


def run_coverage_check(
  settings: Settings,
  cases: list[DropCase],
) -> int:
  """Does every fixture have a baseline, and every baseline a fixture?

  Runs no compiler, so pull-request CI can reject a fixture added without its
  baseline in under a second instead of waiting for the full harness.
  """
  missing = missing_baselines(settings, cases)
  stale = stale_baselines(settings, cases)
  fixtures = sum(1 for case in cases if case.has_baseline)

  for name in missing:
    print(f"missing baseline: {name}", file=sys.stderr)

  for name in stale:
    print(f"stale baseline: {name}", file=sys.stderr)

  if missing or stale:
    print(
      f"{len(missing)} missing and {len(stale)} stale baselines; {REGENERATE_HINT}",
      file=sys.stderr,
    )
    return 1

  print(f"drop-schedule baselines: {fixtures} fixtures, {fixtures} baselines, none stale")

  return 0


def parse_arguments(repository_root: Path) -> argparse.Namespace:
  parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
  parser.add_argument("--compiler", type=Path, help="the compiler under test")
  parser.add_argument(
    "--reference",
    type=Path,
    help="compiler the --project/--extra cases are compared against (they have no baseline)",
  )
  parser.add_argument(
    "--host",
    type=Path,
    help="also run this compiler and cross-check it against the baselines; with --host-compare, compare against it",
  )
  parser.add_argument(
    "--host-compare",
    action="store_true",
    help="compare the compiler under test against --host directly, ignoring the baselines (the pre-freeze mode)",
  )
  parser.add_argument("--std", type=Path, default=repository_root / "std", help="standard library directory")
  parser.add_argument(
    "--baselines",
    type=Path,
    default=repository_root / BASELINE_DIR,
    help=f"committed per-case dumps (default: {BASELINE_DIR})",
  )
  parser.add_argument(
    "--write-baselines",
    action="store_true",
    help="regenerate the baselines from --compiler and exit; review the diff, it is a semantic change",
  )
  parser.add_argument(
    "--check-coverage",
    action="store_true",
    help="only check that fixtures and baselines correspond, running no compiler",
  )
  parser.add_argument(
    "--extra",
    type=Path,
    action="append",
    default=[],
    help="additional single-file entry point to dump, repeatable (needs --reference)",
  )
  parser.add_argument(
    "--project",
    type=Path,
    action="append",
    default=[],
    help="project root to check as a whole, repeatable (e.g. `.` for the selfhost compiler; needs --reference)",
  )
  parser.add_argument("--filter", help="only run cases whose name contains this substring")
  parser.add_argument(
    "--all",
    dest="compare_everything",
    action="store_true",
    help="compare the whole dump, standard library included; only valid with --host-compare",
  )
  parser.add_argument(
    "--jobs",
    type=int,
    default=None,
    help="parallel cases, capped at the CPU count (default: cpu count)",
  )
  parser.add_argument("--report", type=Path, help="markdown report path")
  parser.add_argument("--counts-json", type=Path, help="per-class counts JSON path")
  parser.add_argument("--gate-json", type=Path, help="bootstrap gate result path")
  parser.add_argument(
    "--gate-id",
    default=GATE_ID,
    help=f"what the gate file calls itself (default: {GATE_ID}; the stage1 PR run uses {GATE_ID}-STAGE1)",
  )

  arguments = parser.parse_args()

  if not arguments.check_coverage and arguments.compiler is None:
    parser.error("--compiler is required unless --check-coverage is given")

  if arguments.host_compare and arguments.host is None:
    parser.error("--host-compare needs a --host compiler to compare against")

  if arguments.compare_everything and not arguments.host_compare:
    # The baselines record the own-code section only, so a whole-dump run has
    # nothing to compare against.
    parser.error("--all compares the whole dump, which the baselines do not record; it needs --host-compare")

  if (arguments.project or arguments.extra) and not (arguments.reference or arguments.host_compare):
    parser.error("--project/--extra cases have no baseline; give a --reference compiler to compare them against")

  return arguments


def main() -> int:
  repository_root = Path(__file__).resolve().parent.parent
  arguments = parse_arguments(repository_root)

  settings = Settings(
    compiler=arguments.compiler,
    std_path=arguments.std,
    repository_root=repository_root,
    baseline_dir=arguments.baselines,
    reference=arguments.reference,
    host=arguments.host,
    host_compare=arguments.host_compare,
    compare_everything=arguments.compare_everything,
    gate_id=arguments.gate_id,
  )

  cases = collect_cases(repository_root, arguments.extra, arguments.project, arguments.filter)

  if not cases:
    print("no cases were discovered", file=sys.stderr)
    return 1

  if arguments.check_coverage:
    if arguments.filter:
      print("--check-coverage looks at the whole corpus; --filter would hide real gaps", file=sys.stderr)
      return 1

    return run_coverage_check(settings, cases)

  # `ThreadPoolExecutor`'s own default (`min(32, cpu_count + 4)`) oversubscribes
  # a small runner: every worker's compiler subprocess wants a core of its own,
  # so more workers than cores makes each one slower, not the run faster, and
  # can turn a comfortable margin under a case's timeout into a miss. An
  # explicit `--jobs` above the CPU count is capped the same way, not honored.
  cpu_count = os.cpu_count() or 1
  jobs = min(arguments.jobs, cpu_count) if arguments.jobs else cpu_count

  if arguments.write_baselines:
    return write_baselines(settings, cases, jobs)

  warm_up(settings, cases[0])

  with ThreadPoolExecutor(max_workers=jobs) as executor:
    results = list(executor.map(lambda case: run_case(case, settings), cases))

  # A case that produced no dump usually lost a race for the shared `build/std`
  # directory against another worker, so it is retried once with the pool idle
  # rather than reported as a divergence it is not. A missing baseline is not
  # retried: no amount of rerunning will conjure the file.
  retryable = [result for result in results if result.classification in RETRYABLE_CLASSES]
  # Counted before the retry loop overwrites `results`: a case that timed out
  # and then passed on retry is still worth surfacing, since a slow-but-not-
  # hung machine is the kind of thing that turns into a real timeout later.
  retried_timeouts = sum(1 for result in retryable if result.classification == CLASS_TIMEOUT)

  for stale_result in retryable:
    results[results.index(stale_result)] = run_case(stale_result.case, settings)

  results.sort(key=lambda result: result.case.name)

  counts: dict[str, int] = {}
  for result in results:
    counts[result.classification] = counts.get(result.classification, 0) + 1

  # A filtered run only ever sees part of the corpus, so a baseline it did not
  # ask about is not evidence of staleness.
  stale = [] if arguments.filter else stale_baselines(settings, cases)

  gate = build_gate(results, counts, settings, stale, retried_timeouts)

  if arguments.report:
    arguments.report.parent.mkdir(parents=True, exist_ok=True)
    arguments.report.write_text(build_report(results, counts, settings, stale), encoding="utf-8")

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
    + (f", stale baselines {len(stale)}" if stale else "")
  )

  # scripts/bootstrap.sh's gate-g7 runs this with `|| true` and reads the gate
  # file, the same way it reads gate-g6's; the exit code still tells a direct
  # caller (a developer, this file's own tests) whether every case passed.
  return 0 if gate["status"] == "pass" else 1


if __name__ == "__main__":
  raise SystemExit(main())
