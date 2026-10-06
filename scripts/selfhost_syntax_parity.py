#!/usr/bin/env python3
"""Check a selfhost compiler's parse verdicts against committed baselines (gate G6).

The gate is a parse-level comparison only: for every case the compiler under
test has to reach the verdict recorded for it, accepted or rejected.
Diagnostics from later phases (analysis, ownership, codegen, linking) never
decide a case, so a program rejected for a type error still counts as "parse
accepted".

The recorded verdicts live one per case under `test_cases/__parse_verdicts__/`,
each file exactly `parse-verdict v1` followed by `accepted` or `rejected`. The
gate used to run the Rust host compiler as a live oracle on every case, and the
baselines were generated from that host before it was retired. They change
only when a developer regenerates them with `--write-baselines`, and a baseline
diff in a pull request is a parser change a reviewer has to read. A case
without a baseline, a baseline without a case, and a baseline that is neither
verdict all fail the gate; `--check-coverage` finds the same problems, plus two
cases whose paths flatten to the same case name, without running a compiler.

The corpus is every `.ign` file under `test_cases/`, `example/` and `std/`,
parsed standalone. Parsing needs no import to resolve, and the compiler
reports a parse verdict for a file whose imports do not resolve, so a
standalone file is still comparable.

Part of that corpus is the former host parser unit tests: every inline source
they parsed, wrapped the way their helper wrapped it, is committed byte for
byte under `test_cases/parser/host_unit_tests/` and read like any other case.

Each case is materialised as a one-file Ignis project, because the selfhost
driver resolves its module graph from an `ignis.toml`.
"""

import argparse
import json
import os
import re
import shutil
import subprocess
import sys
from concurrent.futures import ThreadPoolExecutor
from dataclasses import dataclass, field
from pathlib import Path

CORPUS_DIRECTORIES = ("test_cases", "example", "std")

# The codes a lexer or parser emits, taken from the retired host's diagnostics
# and `ignis/diagnostics/codes.ign` and restricted to the codes their
# `lexer`/`parser` modules raise. Besides the `I0xxx` family the host parser
# raised a few `A`/`M` codes of its own (A0067, A0143, A0145, A0203, M0005,
# M0006), which the baselines were generated with.
# Deliberately absent: the analyzer-only codes (I0031, I0033, I0041..I0043),
# and A0111, which the parser shares with the analyzer's `@compileError`, so
# it cannot tell a parse rejection from a later phase's.
PARSE_DIAGNOSTIC_CODES = frozenset(
  {
    "A0067",
    "A0143",
    "A0145",
    "A0203",
    "M0005",
    "M0006",
    "I0001",
    "I0002",
    "I0003",
    "I0004",
    "I0015",
    "I0016",
    "I0018",
    "I0020",
    "I0021",
    "I0022",
    "I0023",
    "I0024",
    "I0025",
    "I0044",
    "I0045",
    "I0046",
    "I0047",
    "I0048",
    "I0049",
    "I0051",
    "I0052",
    "I0053",
  }
)

SELFHOST_TIMEOUT_SECONDS = 300

OBSERVED_OUTPUT_LINES = 10

GATE_ID = "G6"

# Where the committed per-case verdicts live, relative to the repository root.
BASELINE_DIR = "test_cases/__parse_verdicts__"
BASELINE_SUFFIX = ".txt"
BASELINE_HEADER = "parse-verdict v1"
BASELINE_ACCEPTED = "accepted"
BASELINE_REJECTED = "rejected"

REGENERATE_HINT = "regenerate with `scripts/selfhost_syntax_parity.py --compiler <bin> --write-baselines`"

# How many cases a console listing names before summarizing the rest.
REPORTED_CASES = 25

CLASS_PASS = "pass"
CLASS_SELFHOST_REJECTS = "selfhost-rejects"
CLASS_SELFHOST_ACCEPTS = "selfhost-accepts"
CLASS_BASELINE_MISSING = "baseline-missing"
CLASS_BASELINE_MALFORMED = "baseline-malformed"
CLASS_SELFHOST_CRASH = "selfhost-crash"
CLASS_TIMEOUT = "timeout"

BASELINE_CLASS_ORDER = (
  CLASS_PASS,
  CLASS_SELFHOST_REJECTS,
  CLASS_SELFHOST_ACCEPTS,
  CLASS_BASELINE_MISSING,
  CLASS_BASELINE_MALFORMED,
  CLASS_SELFHOST_CRASH,
  CLASS_TIMEOUT,
)

BASELINE_CLASS_DESCRIPTIONS = {
  CLASS_PASS: "the selfhost reaches the recorded parse verdict",
  CLASS_SELFHOST_REJECTS: "the baseline records the case as parsed, the selfhost rejects it",
  CLASS_SELFHOST_ACCEPTS: "the baseline records a parse error, the selfhost parses the case",
  CLASS_BASELINE_MISSING: "the case has no committed baseline",
  CLASS_BASELINE_MALFORMED: "the committed baseline holds no valid verdict",
  CLASS_SELFHOST_CRASH: "the selfhost produced no parse verdict",
  CLASS_TIMEOUT: "the selfhost exceeded its time budget",
}

ORIGIN_REPOSITORY = "repository"
ORIGIN_PARSER_TEST = "parser-test"

# Where the former host parser unit-test snippets are committed, relative to
# the repository root. It sits under `test_cases/`, so the repository walk picks
# the files up as ordinary cases.
PARSER_TEST_DIR = "test_cases/parser/host_unit_tests"


@dataclass
class Case:
  name: str
  origin: str
  source: str
  location: str


@dataclass
class Verdict:
  """What one compiler did with one case at parse level."""

  accepted: bool | None
  codes: list[str] = field(default_factory=list)
  reason: str = ""
  output_tail: list[str] = field(default_factory=list)
  # Carried as a field rather than recovered from `reason`, so a compiler that
  # prints "exceeded" in its own output is never mistaken for a timeout.
  timed_out: bool = False

  def describe(self) -> str:
    if self.accepted is None:
      return self.reason or "no parse verdict"

    if self.accepted:
      return "parse accepted"

    codes = ", ".join(self.codes) if self.codes else "no code"

    return f"parse rejected ({codes})"


@dataclass
class CaseResult:
  case: Case
  classification: str
  reason: str = ""
  # None when the case was decided without running the selfhost (no usable baseline).
  selfhost: Verdict | None = None
  # The committed verdict the case was compared with.
  baseline: bool | None = None


@dataclass
class Settings:
  compiler: Path | None
  std_path: Path
  repository_root: Path
  baseline_dir: Path
  work_dir: Path
  gate_id: str = GATE_ID


# =============================================================================
# Corpus: repository sources
# =============================================================================


def collect_repository_cases(repository_root: Path) -> list[Case]:
  parser_test_root = repository_root / PARSER_TEST_DIR
  cases = []

  for directory in CORPUS_DIRECTORIES:
    root = repository_root / directory

    if not root.is_dir():
      continue

    for path in sorted(root.rglob("*.ign")):
      relative = path.relative_to(repository_root)
      cases.append(
        Case(
          name=case_name_from_path(relative),
          origin=ORIGIN_PARSER_TEST if path.is_relative_to(parser_test_root) else ORIGIN_REPOSITORY,
          source=path.read_text(encoding="utf-8", errors="replace"),
          location=str(relative),
        )
      )

  return cases


def case_name_from_path(relative: Path) -> str:
  return re.sub(r"[^A-Za-z0-9]+", "_", str(relative.with_suffix("")))


# =============================================================================
# Running one case
# =============================================================================


def project_manifest(std_path: Path) -> str:
  return (
    "[package]\n"
    'name = "case"\n'
    'version = "0.1.0"\n'
    "\n"
    "[ignis]\n"
    "std = true\n"
    f'std_path = "{std_path}"\n'
    "\n"
    "[build]\n"
    "bin = true\n"
    'source_dir = "src"\n'
    'entry = "main.ign"\n'
    'out_dir = "build"\n'
    'target = "c"\n'
    'cc = "gcc"\n'
  )


def materialise_case(case: Case, std_path: Path, work_dir: Path) -> Path:
  """Write the case as a single-file Ignis project and return its directory."""
  case_dir = work_dir / case.name
  shutil.rmtree(case_dir, ignore_errors=True)
  (case_dir / "src").mkdir(parents=True, exist_ok=True)
  (case_dir / "ignis.toml").write_text(project_manifest(std_path), encoding="utf-8")
  (case_dir / "src" / "main.ign").write_text(case.source, encoding="utf-8")

  return case_dir


ANSI_PATTERN = re.compile(r"\x1b\[[0-9;]*[A-Za-z]")


def strip_ansi(text: str) -> str:
  return ANSI_PATTERN.sub("", text)


def last_lines(text: str, count: int) -> list[str]:
  lines = [line for line in strip_ansi(text).splitlines() if line.strip()]

  return lines[-count:]


DIAGNOSTIC_PATTERN = re.compile(r"^\s*Error\[([A-Za-z]\d+)\]:")
LOCATION_PATTERN = re.compile(r"^\s*-->\s*(\S+?):\d+:\d+\s*$")
PHASE_LINE_PATTERN = re.compile(r"^\s*(lex|parse)\s*:\s*\w+\s*\((\d+) errors?, (\d+) warnings?\)")
DISCOVERY_LINE_PATTERN = re.compile(r"^\s*discover\s*:\s*\w+\s*\(\d+ errors?, \d+ warnings?\)", re.MULTILINE)
MISSING_MANIFEST_MESSAGE = "ignis.toml could not be found"


def parse_error_codes(output: str, case_file: Path) -> list[str]:
  """The lexer/parser error codes the output reports for the case file itself.

  A diagnostic is attributed to the file named by the `-->` line that follows
  it, so a diagnostic raised in a std module the compiler pulled in never
  decides the case.
  """
  lines = strip_ansi(output).splitlines()
  codes = []
  pending: str | None = None

  for line in lines:
    diagnostic = DIAGNOSTIC_PATTERN.match(line)

    if diagnostic:
      pending = diagnostic.group(1)
      continue

    if pending is None:
      continue

    location = LOCATION_PATTERN.match(line)

    if location is None:
      continue

    if pending in PARSE_DIAGNOSTIC_CODES and Path(location.group(1)).name == case_file.name:
      codes.append(pending)

    pending = None

  return codes


def selfhost_phase_errors(output: str) -> int | None:
  """The lex and parse error counts the selfhost's phase report prints.

  The phase lines print only under `--verbose`, on stderr, which is why the
  harness passes the flag and reads both streams.

  The report is absent when module discovery fails before the phases run, and
  then the parse verdict comes from the diagnostics discovery itself printed.
  """
  total = None

  for line in strip_ansi(output).splitlines():
    phase = PHASE_LINE_PATTERN.match(line)

    if phase:
      total = (total or 0) + int(phase.group(2))

  return total


def selfhost_verdict(
  compiler: Path,
  case_file: Path,
  case_dir: Path,
) -> Verdict:
  try:
    completed = subprocess.run(
      [str(compiler), str(case_file), "-o", str(case_dir / "case_bin"), "--verbose"],
      cwd=case_dir,
      capture_output=True,
      text=True,
      errors="replace",
      timeout=SELFHOST_TIMEOUT_SECONDS,
    )
  except subprocess.TimeoutExpired:
    return Verdict(None, reason=f"the selfhost exceeded {SELFHOST_TIMEOUT_SECONDS}s", timed_out=True)
  except OSError as error:
    return Verdict(None, reason=f"the selfhost could not be run: {error}")

  output = completed.stdout + completed.stderr
  tail = last_lines(output, OBSERVED_OUTPUT_LINES)

  if completed.returncode < 0:
    return Verdict(None, reason=f"the selfhost was killed by signal {-completed.returncode}", output_tail=tail)

  codes = parse_error_codes(output, case_file)
  phase_errors = selfhost_phase_errors(output)

  if phase_errors is not None:
    return Verdict(phase_errors == 0, codes, output_tail=tail)

  # Module discovery parses every module it walks, so a discovery that fails on
  # an import the case cannot resolve standalone still reports the parse
  # diagnostics of the case file and is a parse verdict.
  if MISSING_MANIFEST_MESSAGE in strip_ansi(output):
    return Verdict(None, reason="the selfhost did not pick up the case project", output_tail=tail)

  if DISCOVERY_LINE_PATTERN.search(strip_ansi(output)):
    return Verdict(not codes, codes, output_tail=tail)

  return Verdict(None, reason="the selfhost reported no parse phase", output_tail=tail)


# =============================================================================
# Baselines
# =============================================================================


def baseline_path(
  baseline_dir: Path,
  case: Case,
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


def format_baseline(accepted: bool) -> str:
  verdict = BASELINE_ACCEPTED if accepted else BASELINE_REJECTED

  return f"{BASELINE_HEADER}\n{verdict}\n"


def describe_baseline(accepted: bool) -> str:
  return "parse accepted" if accepted else "parse rejected"


def read_baseline(path: Path) -> tuple[bool | None, str | None]:
  """The verdict committed at an existing `path`, or None and why it holds none.

  Only the exact bytes `format_baseline` writes count as a verdict. Anything
  else, a missing trailing newline included, is malformed, so a hand edit can
  never pass the gate with a verdict it does not spell out exactly.
  """
  try:
    content = path.read_bytes()
  except OSError as error:
    return None, f"could not be read: {error}"

  for accepted in (True, False):
    if content == format_baseline(accepted).encode("utf-8"):
      return accepted, None

  return None, f"is not `{BASELINE_HEADER}` followed by a line `{BASELINE_ACCEPTED}` or `{BASELINE_REJECTED}`"


def stale_baselines(
  settings: Settings,
  cases: list[Case],
) -> list[str]:
  """Committed baselines with no corresponding case, repo-relative."""
  if not settings.baseline_dir.is_dir():
    return []

  expected = {baseline_path(settings.baseline_dir, case).resolve() for case in cases}

  return sorted(
    relative_to(path, settings.repository_root)
    for path in settings.baseline_dir.rglob(f"*{BASELINE_SUFFIX}")
    if path.resolve() not in expected
  )


def missing_baselines(
  settings: Settings,
  cases: list[Case],
) -> list[str]:
  """Cases with no committed baseline, as repo-relative baseline paths."""
  return sorted(
    relative_to(baseline_path(settings.baseline_dir, case), settings.repository_root)
    for case in cases
    if not baseline_path(settings.baseline_dir, case).is_file()
  )


def malformed_baselines(
  settings: Settings,
  cases: list[Case],
) -> list[str]:
  """Committed baselines of existing cases that hold no valid verdict, with the reason."""
  malformed = []

  for case in cases:
    path = baseline_path(settings.baseline_dir, case)

    if not path.is_file():
      continue

    accepted, problem = read_baseline(path)

    if accepted is None:
      malformed.append(f"{relative_to(path, settings.repository_root)} {problem}")

  return sorted(malformed)


def duplicate_case_names(cases: list[Case]) -> list[str]:
  """Case names shared by more than one source, each with the sources that share it.

  A case name flattens its path, so `a-b.ign` and `a_b.ign` collide. Two such
  cases would share one baseline, and one of them would go unchecked.
  """
  locations: dict[str, list[str]] = {}

  for case in cases:
    locations.setdefault(case.name, []).append(case.location)

  return sorted(f"{name}: {', '.join(sources)}" for name, sources in locations.items() if len(sources) > 1)


# =============================================================================
# Classifying one case
# =============================================================================


def classify_against_baseline(
  baseline: bool,
  selfhost: Verdict,
) -> tuple[str, str]:
  if selfhost.timed_out:
    return CLASS_TIMEOUT, selfhost.reason

  if selfhost.accepted is None:
    return CLASS_SELFHOST_CRASH, selfhost.reason

  if selfhost.accepted == baseline:
    return CLASS_PASS, ""

  if baseline:
    return CLASS_SELFHOST_REJECTS, "the baseline records this source as parsed, the selfhost reports {}".format(
      ", ".join(selfhost.codes) or "a parse error"
    )

  return CLASS_SELFHOST_ACCEPTS, "the baseline records a parse error, the selfhost parses this source"


def case_file_for(
  case: Case,
  settings: Settings,
) -> tuple[Path, Path]:
  """Materialise the case and return its source file and project directory."""
  case_dir = materialise_case(case, settings.std_path, settings.work_dir)

  return case_dir / "src" / "main.ign", case_dir


def run_case(
  case: Case,
  settings: Settings,
) -> CaseResult:
  assert settings.compiler is not None

  path = baseline_path(settings.baseline_dir, case)
  relative_path = relative_to(path, settings.repository_root)

  # A case with no usable baseline is decided without running a compiler: there
  # is nothing to hold its verdict to, and rerunning cannot produce the file.
  if not path.is_file():
    return CaseResult(
      case,
      CLASS_BASELINE_MISSING,
      f"no baseline at {relative_path}; a new case must come with its baseline, {REGENERATE_HINT}",
    )

  baseline, problem = read_baseline(path)

  if baseline is None:
    return CaseResult(case, CLASS_BASELINE_MALFORMED, f"{relative_path} {problem}; {REGENERATE_HINT}")

  case_file, case_dir = case_file_for(case, settings)
  selfhost = selfhost_verdict(settings.compiler, case_file, case_dir)
  classification, reason = classify_against_baseline(baseline, selfhost)

  return CaseResult(case, classification, reason, selfhost=selfhost, baseline=baseline)


def run_cases(
  settings: Settings,
  cases: list[Case],
  jobs: int,
  announce: bool = False,
) -> list[CaseResult]:
  """Run every case through the pool, in corpus order."""
  with ThreadPoolExecutor(max_workers=max(1, jobs)) as executor:
    futures = [executor.submit(run_case, case, settings) for case in cases]
    results = []

    for index, future in enumerate(futures, start=1):
      result = future.result()
      results.append(result)

      if announce and result.classification != CLASS_PASS:
        print(f"[syntax] {index}/{len(cases)} {result.classification}: {result.case.name}")

  return results


def count_classes(
  results: list[CaseResult],
  settings: Settings,
) -> dict[str, int]:
  counts = {classification: 0 for classification in BASELINE_CLASS_ORDER}

  for result in results:
    counts[result.classification] = counts.get(result.classification, 0) + 1

  return counts


def evaluate(
  settings: Settings,
  cases: list[Case],
  jobs: int,
  filtered: bool,
  announce: bool = False,
) -> tuple[list[CaseResult], dict[str, int], list[str], dict]:
  """Run the gate over `cases`: results, per-class counts, orphan baselines, gate.

  A filtered run only sees part of the corpus, so a baseline it did not ask
  about is out of scope rather than orphaned, and orphans are not computed.
  """
  results = run_cases(settings, cases, jobs, announce)
  counts = count_classes(results, settings)
  stale = [] if filtered else stale_baselines(settings, cases)

  return results, counts, stale, build_gate(results, counts, settings, stale)


# =============================================================================
# Writing baselines and checking coverage
# =============================================================================


def observe_case(
  case: Case,
  settings: Settings,
) -> tuple[Case, Verdict]:
  """The selfhost's verdict on one case."""
  assert settings.compiler is not None
  case_file, case_dir = case_file_for(case, settings)

  return case, selfhost_verdict(settings.compiler, case_file, case_dir)


def write_baselines(
  settings: Settings,
  cases: list[Case],
  jobs: int,
  filtered: bool,
) -> int:
  """(Re)generate the baselines from `--compiler`'s verdicts.

  Two rules keep a regeneration from quietly damaging the verdicts it is
  supposed to record:

  - Nothing is written unless every case produced a verdict. A compiler that
    fails on part of the corpus would otherwise leave a directory that is part
    old and part new, and the gate would read it as a set of parser changes.
  - Pruning only happens over the whole corpus. Under `--filter` the cases
    this run did not ask about are out of scope, not orphaned, and deleting
    their baselines would shrink the corpus to whatever the filter matched.
  """
  if not cases:
    print("no cases were discovered", file=sys.stderr)
    return 1

  duplicates = duplicate_case_names(cases)

  if duplicates:
    print(
      f"{len(duplicates)} case names are shared by more than one source; no baseline was written or removed",
      file=sys.stderr,
    )

    for entry in duplicates[:REPORTED_CASES]:
      print(f"  {entry}", file=sys.stderr)

    return 1

  with ThreadPoolExecutor(max_workers=max(1, jobs)) as executor:
    observations = list(executor.map(lambda case: observe_case(case, settings), cases))

  failed = [(case, selfhost) for case, selfhost in observations if selfhost.accepted is None]

  if failed:
    print(
      f"{len(failed)} of {len(observations)} cases produced no parse verdict under the selfhost; "
      "no baseline was written or removed",
      file=sys.stderr,
    )

    for case, selfhost in failed[:REPORTED_CASES]:
      print(f"  {case.name}: {selfhost.describe()}", file=sys.stderr)

    return 1

  settings.baseline_dir.mkdir(parents=True, exist_ok=True)
  accepted = 0

  for case, selfhost in observations:
    assert selfhost.accepted is not None
    baseline_path(settings.baseline_dir, case).write_text(format_baseline(selfhost.accepted), encoding="utf-8")

    if selfhost.accepted:
      accepted += 1

  removed: list[str] = []

  if not filtered:
    removed = stale_baselines(settings, cases)

    for name in removed:
      (settings.repository_root / name).unlink()

  pruning = f"removed {len(removed)} orphaned" if not filtered else "kept every other baseline (--filter)"

  print(
    f"wrote {len(observations)} baselines ({accepted} accepted, {len(observations) - accepted} rejected) under "
    f"{relative_to(settings.baseline_dir, settings.repository_root)}, {pruning}"
  )

  return 0


def run_coverage_check(
  settings: Settings,
  cases: list[Case],
) -> int:
  """Does every case have a valid baseline, and every baseline a case?

  Runs no compiler, so pull-request CI can reject a case added without its
  baseline in seconds instead of waiting for the full gate.
  """
  missing = missing_baselines(settings, cases)
  stale = stale_baselines(settings, cases)
  malformed = malformed_baselines(settings, cases)
  duplicates = duplicate_case_names(cases)

  for name in missing:
    print(f"missing baseline: {name}", file=sys.stderr)

  for name in stale:
    print(f"orphaned baseline: {name}", file=sys.stderr)

  for entry in malformed:
    print(f"malformed baseline: {entry}", file=sys.stderr)

  for entry in duplicates:
    print(f"duplicate case name: {entry}", file=sys.stderr)

  if missing or stale or malformed or duplicates:
    print(
      f"{len(missing)} missing, {len(stale)} orphaned and {len(malformed)} malformed baselines, "
      f"{len(duplicates)} duplicate case names; {REGENERATE_HINT}",
      file=sys.stderr,
    )
    return 1

  print(f"parse-verdict baselines: {len(cases)} cases, {len(cases)} baselines, none orphaned or malformed")

  return 0


# =============================================================================
# Reporting
# =============================================================================


def expected_description(result: CaseResult) -> str:
  """What the case was held to, for the report and the failing entries."""
  if result.baseline is None:
    return "baseline: none"

  return f"baseline: {describe_baseline(result.baseline)}"


def build_report(
  results: list[CaseResult],
  counts: dict[str, int],
  settings: Settings,
  stale: list[str],
) -> str:
  lines = [
    f"# Selfhost syntax parity report ({settings.gate_id})",
    "",
    "Every case is held to the parse verdict committed for it under",
    f"`{relative_to(settings.baseline_dir, settings.repository_root)}`. Only lexer and parser",
    "diagnostics reported against the case file decide a verdict; later phases never do.",
    "",
    "## Summary",
    "",
    "| class | count | meaning |",
    "| --- | --- | --- |",
  ]

  for classification in BASELINE_CLASS_ORDER:
    lines.append(
      f"| {classification} | {counts.get(classification, 0)} | {BASELINE_CLASS_DESCRIPTIONS[classification]} |"
    )

  lines.extend(["", f"| total | {len(results)} |", ""])
  lines.extend(build_stale_section(stale))

  origins: dict[str, dict[str, int]] = {}

  for result in results:
    bucket = origins.setdefault(result.case.origin, {"total": 0, "failing": 0})
    bucket["total"] += 1

    if result.classification != CLASS_PASS:
      bucket["failing"] += 1

  lines.extend(["## By origin", "", "| origin | cases | non-pass |", "| --- | --- | --- |"])

  for origin in sorted(origins):
    lines.append(f"| {origin} | {origins[origin]['total']} | {origins[origin]['failing']} |")

  lines.append("")

  for classification in BASELINE_CLASS_ORDER:
    if classification == CLASS_PASS:
      continue

    selected = [result for result in results if result.classification == classification]

    if not selected:
      continue

    lines.extend([f"## {classification} ({len(selected)})", ""])

    for result in selected:
      lines.extend(build_case_section(result))

  return "\n".join(lines) + "\n"


def build_case_section(result: CaseResult) -> list[str]:
  lines = [
    f"### `{result.case.name}`",
    "",
    f"- origin: `{result.case.location}`",
    f"- {expected_description(result)}",
    f"- selfhost: {result.selfhost.describe() if result.selfhost else 'not run'}",
    "",
    result.reason or "(no reason recorded)",
    "",
  ]

  if result.selfhost and result.selfhost.output_tail:
    lines.extend(["Selfhost printed:", "", "```", "\n".join(result.selfhost.output_tail), "```", ""])

  return lines


def build_stale_section(stale: list[str]) -> list[str]:
  if not stale:
    return []

  return [
    "## Orphaned baselines",
    "",
    "These files record a case the corpus no longer has. A baseline with no case can",
    f"never fail, so it fails the gate instead; {REGENERATE_HINT}.",
    "",
    *[f"- `{name}`" for name in stale],
    "",
  ]


def failing_entries(results: list[CaseResult]) -> list[dict]:
  entries = []

  for result in results:
    if result.classification == CLASS_PASS:
      continue

    entry = {
      "case": result.case.name,
      "origin": result.case.location,
      "class": result.classification,
      "selfhost": result.selfhost.describe() if result.selfhost else "not run",
      "reason": result.reason,
      "baseline": describe_baseline(result.baseline) if result.baseline is not None else "none",
    }

    entries.append(entry)

  return entries


def build_gate(
  results: list[CaseResult],
  counts: dict[str, int],
  settings: Settings,
  stale: list[str],
) -> dict:
  """Describe the run as a bootstrap gate result."""
  total = len(results)
  passed = counts.get(CLASS_PASS, 0)
  source = "baseline"

  stale_plural = "s" if len(stale) != 1 else ""
  stale_note = f", {len(stale)} orphaned baseline{stale_plural}" if stale else ""

  healthy = total > 0 and passed == total and not stale

  gate = {
    "gate": settings.gate_id,
    "status": "pass" if healthy else "fail",
    "summary": f"syntax parity {passed}/{total} vs {source}{stale_note}",
    "details": {
      "corpus": "syntax",
      "compared_against": source,
      "total": total,
      "counts": {classification: counts.get(classification, 0) for classification in BASELINE_CLASS_ORDER},
      "stale_baselines": stale,
      "failing": failing_entries(results),
    },
  }

  return gate


# =============================================================================
# Command line
# =============================================================================


def parse_arguments(repository_root: Path) -> argparse.Namespace:
  parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
  parser.add_argument("--compiler", help="selfhost-built compiler binary under test")
  parser.add_argument("--std", help="std directory (default: <repo>/std)")
  parser.add_argument(
    "--baselines",
    type=Path,
    default=repository_root / BASELINE_DIR,
    help=f"committed per-case parse verdicts (default: {BASELINE_DIR})",
  )
  parser.add_argument("--jobs", type=int, default=os.cpu_count() or 1, help="parallel cases")
  parser.add_argument("--filter", help="only run cases whose name contains this substring")
  parser.add_argument("--report", help="write a Markdown report to this path")
  parser.add_argument("--counts-json", help="write the per-class counts to this path as JSON")
  parser.add_argument("--work-dir", help="directory for the generated projects")
  parser.add_argument("--gate-json", help="write the bootstrap gate result to this path")
  parser.add_argument("--gate-id", default=GATE_ID, help=f"what the gate file calls itself (default: {GATE_ID})")

  modes = parser.add_mutually_exclusive_group()
  modes.add_argument(
    "--write-baselines",
    action="store_true",
    help="regenerate the baselines from --compiler and exit; review the diff, it is a parser change",
  )
  modes.add_argument(
    "--check-coverage",
    action="store_true",
    help="only check that cases and baselines correspond and are well formed, running no compiler",
  )
  arguments = parser.parse_args()

  if not arguments.check_coverage and arguments.compiler is None:
    parser.error("--compiler is required unless --check-coverage is given")

  # A filtered coverage check would hide real gaps.
  if arguments.check_coverage and arguments.filter:
    parser.error("--check-coverage covers the whole corpus; --filter does not apply")

  return arguments


def resolve_binary(
  value: str,
  label: str,
  search_path: bool,
) -> Path | None:
  """An executable given as a path or, when `search_path` is set, as a bare name looked up on PATH."""
  path = Path(value)

  if search_path and path.name == value:
    found = shutil.which(value)

    if found is None:
      print(f"error: {label} not found: {value}", file=sys.stderr)
      return None

    path = Path(found)

  path = path.resolve()

  if not path.is_file():
    print(f"error: {label} not found: {path}", file=sys.stderr)
    return None

  return path


def write_outputs(
  arguments: argparse.Namespace,
  results: list[CaseResult],
  counts: dict[str, int],
  settings: Settings,
  stale: list[str],
  gate: dict,
) -> None:
  if arguments.report:
    report_path = Path(arguments.report).resolve()
    report_path.parent.mkdir(parents=True, exist_ok=True)
    report_path.write_text(build_report(results, counts, settings, stale), encoding="utf-8")
    print("")
    print(f"[syntax] report written to {report_path}")

  if arguments.counts_json:
    counts_path = Path(arguments.counts_json).resolve()
    counts_path.parent.mkdir(parents=True, exist_ok=True)
    counts_path.write_text(
      json.dumps(
        {
          "corpus": "syntax",
          "compared_against": gate["details"]["compared_against"],
          "total": len(results),
          "counts": gate["details"]["counts"],
          "stale_baselines": stale,
          "failing": gate["details"]["failing"],
        },
        indent=2,
      )
      + "\n",
      encoding="utf-8",
    )
    print(f"[syntax] counts written to {counts_path}")

  if arguments.gate_json:
    gate_path = Path(arguments.gate_json).resolve()
    gate_path.parent.mkdir(parents=True, exist_ok=True)
    gate_path.write_text(json.dumps(gate, indent=2) + "\n", encoding="utf-8")
    print(f"[syntax] gate result written to {gate_path}")


def print_summary(
  results: list[CaseResult],
  counts: dict[str, int],
  settings: Settings,
  stale: list[str],
) -> None:
  print("")
  print("class            count")

  for classification in BASELINE_CLASS_ORDER:
    print(f"{classification:<16} {counts.get(classification, 0)}")

  print(f"{'total':<16} {len(results)}")

  failing = [result for result in results if result.classification != CLASS_PASS]

  if failing:
    print("")
    print("non-passing cases:")

    for result in failing[:REPORTED_CASES]:
      print(f"  {result.classification:<16} {result.case.name}: {result.reason}")

    if len(failing) > REPORTED_CASES:
      print(f"  ... and {len(failing) - REPORTED_CASES} more")

  if stale:
    print("")
    print("orphaned baselines:")

    for name in stale[:REPORTED_CASES]:
      print(f"  {name}")

    if len(stale) > REPORTED_CASES:
      print(f"  ... and {len(stale) - REPORTED_CASES} more")


def main() -> int:
  repository_root = Path(__file__).resolve().parent.parent
  arguments = parse_arguments(repository_root)

  std_path = Path(arguments.std).resolve() if arguments.std else repository_root / "std"
  work_dir = Path(arguments.work_dir).resolve() if arguments.work_dir else repository_root / "build/parity-syntax"

  settings = Settings(
    compiler=None,
    std_path=std_path,
    repository_root=repository_root,
    baseline_dir=arguments.baselines.resolve(),
    work_dir=work_dir,
    gate_id=arguments.gate_id,
  )

  cases = collect_repository_cases(repository_root)

  if not cases:
    print("error: no cases were discovered", file=sys.stderr)
    return 1

  if arguments.check_coverage:
    return run_coverage_check(settings, cases)

  settings.compiler = resolve_binary(arguments.compiler, "compiler", search_path=False)

  if settings.compiler is None:
    return 2

  if not std_path.is_dir():
    print(f"error: std directory not found: {std_path}", file=sys.stderr)
    return 2

  filtered = bool(arguments.filter)

  if filtered:
    cases = [case for case in cases if arguments.filter in case.name]

  work_dir.mkdir(parents=True, exist_ok=True)

  if arguments.write_baselines:
    return write_baselines(settings, cases, arguments.jobs, filtered)

  print(f"[syntax] compiler: {settings.compiler}")
  print(f"[syntax] against:  baselines under {relative_to(settings.baseline_dir, repository_root)}")

  print(f"[syntax] cases:    {len(cases)} (jobs: {arguments.jobs})")

  results, counts, stale, gate = evaluate(settings, cases, arguments.jobs, filtered, announce=True)

  print_summary(results, counts, settings, stale)
  write_outputs(arguments, results, counts, settings, stale, gate)

  return 0 if gate["status"] == "pass" else 1


if __name__ == "__main__":
  sys.exit(main())
