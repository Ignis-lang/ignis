#!/usr/bin/env python3
"""Run a host end-to-end corpus through a selfhost-built compiler.

`--corpus ok` (the default) materialises every `e2e_test`,
`e2e_test_allow_leak` and `e2e_workspace_std_test` case in
`crates/ignis_driver/tests/e2e_ok.rs` as a small Ignis project, compiles it with
the given compiler binary and runs it. The observed exit code, stdout and stderr
are formatted exactly like the host's `format_e2e_result` and compared with the
recorded insta snapshot.

`--corpus err` runs the error corpus (`crates/ignis_driver/tests/e2e_err.rs`)
under the "equal or better" rule: every diagnostic line the host records must
appear in the selfhost's output, while diagnostics the host does not emit are
allowed. Cases are only compiled; the host helpers behind that corpus record
analysis diagnostics and never run a binary.
"""

import argparse
import difflib
import json
import os
import re
import shutil
import subprocess
import sys
from concurrent.futures import ThreadPoolExecutor
from dataclasses import dataclass, field
from pathlib import Path

CORPUS_OK = "ok"
CORPUS_ERR = "err"

# `e2e_error_test` and `e2e_ownership_error_test` snapshot the error messages of
# a program the host rejects; `e2e_warning_test` snapshots the warnings of a
# program the host accepts. None of the three runs the produced binary.
KIND_RUN = "run"
KIND_ERROR = "error"
KIND_WARNING = "warning"

CASE_HELPERS = {
  CORPUS_OK: {
    "e2e_test": KIND_RUN,
    "e2e_test_allow_leak": KIND_RUN,
    "e2e_workspace_std_test": KIND_RUN,
  },
  CORPUS_ERR: {
    "e2e_error_test": KIND_ERROR,
    "e2e_ownership_error_test": KIND_ERROR,
    "e2e_warning_test": KIND_WARNING,
  },
}

CORPUS_FILE = {
  CORPUS_OK: "crates/ignis_driver/tests/e2e_ok.rs",
  CORPUS_ERR: "crates/ignis_driver/tests/e2e_err.rs",
}

SNAPSHOT_PREFIX = {CORPUS_OK: "e2e_ok__", CORPUS_ERR: "e2e_err__"}

COMPILE_TIMEOUT_SECONDS = 120
RUN_TIMEOUT_SECONDS = 10

OBSERVED_OUTPUT_LINES = 10

CLASS_PASS = "pass"
CLASS_MISMATCH = "mismatch"
CLASS_MISSING = "missing"
CLASS_COMPILED = "compiled"
CLASS_COMPILE_ERROR = "compile-error"
CLASS_COMPILE_TIMEOUT = "compile-timeout"
CLASS_RUN_TIMEOUT = "run-timeout"
CLASS_SKIPPED = "skipped"

CLASS_ORDER = {
  CORPUS_OK: (
    CLASS_PASS,
    CLASS_MISMATCH,
    CLASS_COMPILE_ERROR,
    CLASS_COMPILE_TIMEOUT,
    CLASS_RUN_TIMEOUT,
    CLASS_SKIPPED,
  ),
  CORPUS_ERR: (
    CLASS_PASS,
    CLASS_MISSING,
    CLASS_COMPILED,
    CLASS_COMPILE_TIMEOUT,
    CLASS_SKIPPED,
  ),
}


@dataclass
class Case:
  name: str
  helper: str
  source: str | None
  kind: str = KIND_RUN
  snapshot: str | None = None
  skip_reason: str | None = None


@dataclass
class CaseResult:
  case: Case
  classification: str
  reason: str = ""
  details: str = ""
  diff: str = ""
  compiler_tail: list[str] = field(default_factory=list)
  expected_lines: list[str] = field(default_factory=list)
  missing_lines: list[str] = field(default_factory=list)


class SourceScanner:
  """Minimal scanner over the Rust corpus, sufficient for the call shapes used."""

  def __init__(self, text: str):
    self.text = text
    self.position = 0

  def skip_trivia(self) -> None:
    while self.position < len(self.text):
      character = self.text[self.position]

      if character.isspace():
        self.position += 1
        continue

      if self.text.startswith("//", self.position):
        end = self.text.find("\n", self.position)
        self.position = len(self.text) if end == -1 else end + 1
        continue

      break

  def read_plain_string(self) -> str | None:
    if self.position >= len(self.text) or self.text[self.position] != '"':
      return None

    self.position += 1
    characters = []

    while self.position < len(self.text):
      character = self.text[self.position]

      if character == "\\":
        characters.append(self.text[self.position:self.position + 2])
        self.position += 2
        continue

      if character == '"':
        self.position += 1
        return "".join(characters)

      characters.append(character)
      self.position += 1

    return None

  def read_raw_string(self) -> str | None:
    if self.position >= len(self.text) or self.text[self.position] != "r":
      return None

    cursor = self.position + 1
    hashes = 0

    while cursor < len(self.text) and self.text[cursor] == "#":
      hashes += 1
      cursor += 1

    if cursor >= len(self.text) or self.text[cursor] != '"':
      return None

    terminator = '"' + "#" * hashes
    end = self.text.find(terminator, cursor + 1)

    if end == -1:
      return None

    value = self.text[cursor + 1:end]
    self.position = end + len(terminator)

    return value


def unescape_rust_string(value: str) -> str:
  escapes = {"n": "\n", "t": "\t", "r": "\r", "0": "\0", "\\": "\\", '"': '"', "'": "'"}
  characters = []
  index = 0

  while index < len(value):
    character = value[index]

    if character == "\\" and index + 1 < len(value):
      following = value[index + 1]

      if following in escapes:
        characters.append(escapes[following])
        index += 2
        continue

    characters.append(character)
    index += 1

  return "".join(characters)


def extract_cases(
  corpus_path: Path,
  helpers: dict[str, str],
) -> list[Case]:
  text = corpus_path.read_text(encoding="utf-8")
  pattern = re.compile(r"(?<![A-Za-z0-9_])(" + "|".join(helpers) + r")\s*\(")
  cases: list[Case] = []

  for match in pattern.finditer(text):
    helper = match.group(1)
    kind = helpers[helper]
    preceding = text.rfind("\n", 0, match.start())

    # `fn e2e_test(` declares the helper instead of calling it.
    if text[preceding + 1:match.start()].rstrip().endswith("fn"):
      continue

    scanner = SourceScanner(text)
    scanner.position = match.end()
    scanner.skip_trivia()

    raw_name = scanner.read_plain_string()

    if raw_name is None:
      continue

    name = unescape_rust_string(raw_name)

    scanner.skip_trivia()

    if scanner.position >= len(text) or text[scanner.position] != ",":
      cases.append(Case(name, helper, None, kind, skip_reason="source not literal"))
      continue

    scanner.position += 1
    scanner.skip_trivia()

    source = scanner.read_raw_string()

    if source is None:
      source = scanner.read_plain_string()
      source = unescape_rust_string(source) if source is not None else None

    if source is None:
      cases.append(Case(name, helper, None, kind, skip_reason="source not literal"))
      continue

    cases.append(Case(name, helper, source, kind))

  return cases


def read_snapshot_body(snapshot_path: Path) -> str | None:
  if not snapshot_path.is_file():
    return None

  text = snapshot_path.read_text(encoding="utf-8")
  lines = text.split("\n")

  if not lines or lines[0].strip() != "---":
    return None

  for index in range(1, len(lines)):
    if lines[index].strip() == "---":
      return "\n".join(lines[index + 1:]).rstrip("\n")

  return None


def format_e2e_result(exit_code: int, stdout: str, stderr: str) -> str:
  return "exit_code: {}\nstdout: {}\nstderr: {}".format(
    exit_code,
    "(empty)" if stdout == "" else stdout.rstrip(),
    "(empty)" if stderr == "" else stderr.rstrip(),
  )


def project_manifest(std_path: Path) -> str:
  return (
    "[package]\n"
    'name = "case"\n'
    'version = "0.1.0"\n'
    "\n"
    "[ignis]\n"
    "std = true\n"
    f'std_path = "{std_path}"\n'
    f'runtime_path = "{std_path / "runtime"}"\n'
    "\n"
    "[build]\n"
    "bin = true\n"
    'source_dir = "src"\n'
    'entry = "main.ign"\n'
    'out_dir = "build"\n'
    'target = "c"\n'
    'cc = "gcc"\n'
  )


ANSI_PATTERN = re.compile(r"\x1b\[[0-9;]*[A-Za-z]")


def strip_ansi(text: str) -> str:
  return ANSI_PATTERN.sub("", text)


def last_lines(text: str, count: int) -> list[str]:
  lines = [line for line in strip_ansi(text).splitlines() if line.strip()]

  return lines[-count:]


DIAGNOSTIC_PATTERN = re.compile(r"^\s*(Error|error)\[[A-Z]?\d+\]:")
C_ERROR_PATTERN = re.compile(r":\s*(error|fatal error):")
PHASE_REPORT_PATTERN = re.compile(r"\(\d+ errors?, \d+ warnings?\)")


def first_problem_line(output: str) -> str:
  """Pick the line that explains the failure, ignoring the phase report."""
  lines = [line.strip() for line in strip_ansi(output).splitlines() if line.strip()]
  interesting = [line for line in lines if not PHASE_REPORT_PATTERN.search(line)]

  # The C error is more specific than the `gcc compilation failed` wrapper the
  # driver reports around it, so it wins when both are present.
  for matcher in (C_ERROR_PATTERN.search, DIAGNOSTIC_PATTERN.search):
    for line in interesting:
      if matcher(line):
        return line

  for line in interesting:
    lowered = line.lower()

    if "error" in lowered or "panic" in lowered:
      return line

  return interesting[-1] if interesting else "no compiler output"


def materialise_case(case: Case, std_path: Path, work_dir: Path) -> Path:
  """Write the case as a single-file Ignis project and return its directory."""
  case_dir = work_dir / case.name
  shutil.rmtree(case_dir, ignore_errors=True)
  (case_dir / "src").mkdir(parents=True, exist_ok=True)
  (case_dir / "ignis.toml").write_text(project_manifest(std_path), encoding="utf-8")
  (case_dir / "src" / "main.ign").write_text(case.source or "", encoding="utf-8")

  return case_dir


def compile_case(
  compiler: Path,
  case_dir: Path,
  binary_path: Path,
) -> subprocess.CompletedProcess | None:
  """Compile a materialised case, or return None when the compiler times out."""
  try:
    return subprocess.run(
      [str(compiler), str(case_dir / "src" / "main.ign"), "-o", str(binary_path)],
      cwd=case_dir,
      capture_output=True,
      text=True,
      errors="replace",
      timeout=COMPILE_TIMEOUT_SECONDS,
    )
  except subprocess.TimeoutExpired:
    return None


def run_case(case: Case, compiler: Path, std_path: Path, work_dir: Path) -> CaseResult:
  if case.skip_reason is not None:
    return CaseResult(case, CLASS_SKIPPED, case.skip_reason)

  if case.snapshot is None:
    return CaseResult(case, CLASS_SKIPPED, "no snapshot recorded")

  if case.kind == KIND_RUN:
    return run_ok_case(case, compiler, std_path, work_dir)

  return run_err_case(case, compiler, std_path, work_dir)


def run_ok_case(case: Case, compiler: Path, std_path: Path, work_dir: Path) -> CaseResult:
  case_dir = materialise_case(case, std_path, work_dir)
  binary_path = case_dir / "case_bin"
  compilation = compile_case(compiler, case_dir, binary_path)

  if compilation is None:
    return CaseResult(case, CLASS_COMPILE_TIMEOUT, f"compiler exceeded {COMPILE_TIMEOUT_SECONDS}s")

  compiler_output = compilation.stdout + compilation.stderr

  if compilation.returncode != 0 or not binary_path.is_file():
    tail = last_lines(compiler_output, 20)

    if compilation.returncode < 0:
      reason = f"compiler killed by signal {-compilation.returncode}"
    elif compilation.returncode == 0:
      reason = "compiler reported success but produced no binary"
    else:
      reason = first_problem_line(compiler_output)

    return CaseResult(case, CLASS_COMPILE_ERROR, reason, compiler_tail=tail)

  try:
    execution = subprocess.run(
      [str(binary_path)],
      cwd=case_dir,
      capture_output=True,
      text=True,
      errors="replace",
      timeout=RUN_TIMEOUT_SECONDS,
    )
  except subprocess.TimeoutExpired:
    return CaseResult(case, CLASS_RUN_TIMEOUT, f"program exceeded {RUN_TIMEOUT_SECONDS}s")

  observed = format_e2e_result(execution.returncode, execution.stdout, execution.stderr)

  if observed == case.snapshot:
    return CaseResult(case, CLASS_PASS)

  diff = "\n".join(
    difflib.unified_diff(
      case.snapshot.split("\n"),
      observed.split("\n"),
      fromfile="host snapshot",
      tofile="selfhost run",
      lineterm="",
    )
  )

  return CaseResult(case, CLASS_MISMATCH, first_diff_line(case.snapshot, observed), diff=diff)


def first_diff_line(expected: str, observed: str) -> str:
  expected_lines = expected.split("\n")
  observed_lines = observed.split("\n")

  for index in range(max(len(expected_lines), len(observed_lines))):
    expected_line = expected_lines[index] if index < len(expected_lines) else "<missing>"
    observed_line = observed_lines[index] if index < len(observed_lines) else "<missing>"

    if expected_line != observed_line:
      return f"expected `{expected_line}`, got `{observed_line}`"

  return "outputs differ"


SEVERITY_PREFIX_PATTERN = re.compile(r"^[A-Za-z]+\[[A-Za-z]?\d+\]:\s*")
LINE_DECORATION_PATTERN = re.compile(r"^[=|\-\s]+")


def normalize_diagnostic_line(line: str) -> str:
  """Reduce a diagnostic line to the message text both compilers should share.

  The host snapshots store the bare message (plus `  note: ` continuations)
  while the selfhost prints `Error[A0001]: message`, so the severity and code
  prefix and any gutter decoration are dropped before comparing.
  """
  text = strip_ansi(line).strip()
  text = LINE_DECORATION_PATTERN.sub("", text)
  text = SEVERITY_PREFIX_PATTERN.sub("", text)

  return re.sub(r"\s+", " ", text).strip()


NOTE_LINE_PATTERN = re.compile(r"^\s*(?:note|help):")


def missing_diagnostics(expected: str, output: str) -> tuple[list[str], list[str]]:
  """Return the expected lines the output does not carry, and the expected set.

  Only diagnostic messages gate the case. A `note:`/`help:` continuation the
  host records but the selfhost omits is reported through `missing_notes`,
  not here: the promotion rule asks for every host diagnostic to be present,
  and treats extra or richer output as an improvement rather than a failure.
  """
  expected_lines = [line for line in expected.split("\n") if line.strip()]
  observed_lines = [normalize_diagnostic_line(line) for line in strip_ansi(output).splitlines()]
  observed_lines = [line for line in observed_lines if line]

  missing = []

  for line in expected_lines:
    if NOTE_LINE_PATTERN.match(line):
      continue

    wanted = normalize_diagnostic_line(line)

    if not any(wanted in observed for observed in observed_lines):
      missing.append(line.strip())

  return missing, expected_lines


def missing_notes(expected: str, output: str) -> list[str]:
  """The `note:`/`help:` lines the host records that the selfhost did not print."""
  observed_lines = [normalize_diagnostic_line(line) for line in strip_ansi(output).splitlines()]
  observed_lines = [line for line in observed_lines if line]
  missing = []

  for line in expected.split("\n"):
    if not NOTE_LINE_PATTERN.match(line):
      continue

    wanted = normalize_diagnostic_line(line)

    if not any(wanted in observed for observed in observed_lines):
      missing.append(line.strip())

  return missing


REPORTED_DIAGNOSTIC_PATTERN = re.compile(r"^\s*(?:[A-Za-z]+\[[A-Za-z]?\d+\]:|note:|help:)")


def observed_diagnostics(output: str, count: int) -> list[str]:
  """Quote what the selfhost printed, preferring its diagnostics over the phase banner."""
  lines = [line for line in strip_ansi(output).splitlines() if line.strip()]
  diagnostics = [line for line in lines if REPORTED_DIAGNOSTIC_PATTERN.match(line)]

  return (diagnostics or lines)[:count]


def run_err_case(case: Case, compiler: Path, std_path: Path, work_dir: Path) -> CaseResult:
  case_dir = materialise_case(case, std_path, work_dir)
  binary_path = case_dir / "case_bin"
  compilation = compile_case(compiler, case_dir, binary_path)

  if compilation is None:
    return CaseResult(case, CLASS_COMPILE_TIMEOUT, f"compiler exceeded {COMPILE_TIMEOUT_SECONDS}s")

  compiler_output = compilation.stdout + compilation.stderr
  observed = observed_diagnostics(compiler_output, OBSERVED_OUTPUT_LINES)
  missing, expected_lines = missing_diagnostics(case.snapshot or "", compiler_output)

  # A program the host rejects must not compile, whatever the selfhost printed.
  if case.kind == KIND_ERROR and compilation.returncode == 0:
    return CaseResult(
      case,
      CLASS_COMPILED,
      f"selfhost accepted a program the host rejects with `{expected_lines[0].strip()}`"
      if expected_lines
      else "selfhost accepted a program the host rejects",
      compiler_tail=observed,
      expected_lines=expected_lines,
      missing_lines=missing,
    )

  if not missing:
    notes = missing_notes(case.snapshot or "", compiler_output)
    reason = f"{len(notes)} host note line(s) not printed: `{notes[0]}`" if notes else ""

    return CaseResult(case, CLASS_PASS, reason, expected_lines=expected_lines)

  reason = "{} of {} expected diagnostic line(s) missing: `{}`".format(
    len(missing),
    len(expected_lines),
    missing[0],
  )

  return CaseResult(
    case,
    CLASS_MISSING,
    reason,
    compiler_tail=observed,
    expected_lines=expected_lines,
    missing_lines=missing,
  )


def group_diagnostics(results: list[CaseResult], classification: str) -> list[tuple[str, list[str]]]:
  """Group cases by the first diagnostic the host records, which is the message to file."""
  groups: dict[str, list[str]] = {}

  for result in results:
    if result.classification != classification:
      continue

    key = normalize_error(result.expected_lines[0]) if result.expected_lines else "(no expected diagnostic)"
    groups.setdefault(key, []).append(result.case.name)

  return sorted(groups.items(), key=lambda item: (-len(item[1]), item[0]))


def group_compile_errors(results: list[CaseResult]) -> list[tuple[str, list[str]]]:
  groups: dict[str, list[str]] = {}

  for result in results:
    if result.classification != CLASS_COMPILE_ERROR:
      continue

    groups.setdefault(normalize_error(result.reason), []).append(result.case.name)

  return sorted(groups.items(), key=lambda item: (-len(item[1]), item[0]))


def normalize_error(reason: str) -> str:
  """Collapse the parts that vary per case so one missing feature groups once."""
  normalized = re.sub(r"(/[^\s:]+)+", "<path>", reason)
  normalized = re.sub(r"[\w.]+\.c:\d+:\d+", "<file>", normalized)
  normalized = re.sub(r"\b\d+:\d+\b", "<pos>", normalized)
  normalized = re.sub(r"[‘'][^’']+[’']", "<name>", normalized)

  return re.sub(r"\s+", " ", normalized).strip()


def format_group_table(title: str, groups: list[tuple[str, list[str]]]) -> list[str]:
  if not groups:
    return []

  lines = [f"## {title}", "", "| count | message | cases |", "| --- | --- | --- |"]

  for message, names in groups:
    shown = ", ".join(names[:8])

    if len(names) > 8:
      shown += f", … (+{len(names) - 8})"

    lines.append(f"| {len(names)} | `{message}` | {shown} |")

  lines.append("")

  return lines


def build_report(results: list[CaseResult], counts: dict[str, int], corpus: str) -> str:
  class_order = CLASS_ORDER[corpus]
  lines = [
    f"# Selfhost e2e parity report ({corpus} corpus)",
    "",
    "## Summary",
    "",
    "| class | count |",
    "| --- | --- |",
  ]

  for classification in class_order:
    lines.append(f"| {classification} | {counts.get(classification, 0)} |")

  lines.extend(["", f"| total | {len(results)} |", ""])

  if corpus == CORPUS_ERR:
    lines.extend(format_group_table("Missing diagnostics by host message", group_diagnostics(results, CLASS_MISSING)))
    lines.extend(format_group_table("Accepted programs by host message", group_diagnostics(results, CLASS_COMPILED)))
  else:
    lines.extend(format_group_table("Compile errors by message", group_compile_errors(results)))

  for classification in class_order:
    if classification == CLASS_PASS:
      continue

    selected = [result for result in results if result.classification == classification]

    if not selected:
      continue

    lines.extend([f"## {classification} ({len(selected)})", ""])

    for result in selected:
      lines.append(f"### `{result.case.name}`")
      lines.append("")
      lines.append(result.reason or "(no reason recorded)")
      lines.append("")

      if result.diff:
        lines.extend(["```diff", result.diff, "```", ""])

      if result.missing_lines:
        lines.append("Missing from the selfhost output:")
        lines.append("")
        lines.extend(f"- `{line}`" for line in result.missing_lines)
        lines.append("")

      if result.compiler_tail:
        label = "Selfhost printed:" if result.case.kind != KIND_RUN else ""

        if label:
          lines.extend([label, ""])

        lines.extend(["```", "\n".join(result.compiler_tail), "```", ""])

  return "\n".join(lines) + "\n"


def main() -> int:
  parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
  parser.add_argument("--compiler", required=True, help="selfhost-built compiler binary")
  parser.add_argument(
    "--corpus",
    choices=(CORPUS_OK, CORPUS_ERR),
    default=CORPUS_OK,
    help="which host corpus to replay (default: ok)",
  )
  parser.add_argument("--std", help="std directory (default: <repo>/std)")
  parser.add_argument("--jobs", type=int, default=os.cpu_count() or 1, help="parallel cases")
  parser.add_argument("--filter", help="only run cases whose name contains this substring")
  parser.add_argument("--report", help="write a Markdown report to this path")
  parser.add_argument("--counts-json", help="write the per-class counts to this path as JSON")
  parser.add_argument("--work-dir", help="directory for the generated projects")
  arguments = parser.parse_args()

  corpus = arguments.corpus
  class_order = CLASS_ORDER[corpus]
  repository_root = Path(__file__).resolve().parent.parent
  compiler = Path(arguments.compiler).resolve()

  if not compiler.is_file():
    print(f"error: compiler not found: {compiler}", file=sys.stderr)
    return 2

  std_path = Path(arguments.std).resolve() if arguments.std else repository_root / "std"

  if not std_path.is_dir():
    print(f"error: std directory not found: {std_path}", file=sys.stderr)
    return 2

  corpus_path = repository_root / CORPUS_FILE[corpus]
  snapshots_dir = repository_root / "crates/ignis_driver/tests/snapshots"
  cases = extract_cases(corpus_path, CASE_HELPERS[corpus])

  if arguments.filter:
    cases = [case for case in cases if arguments.filter in case.name]

  for case in cases:
    case.snapshot = read_snapshot_body(snapshots_dir / f"{SNAPSHOT_PREFIX[corpus]}{case.name}.snap")

  work_dir = Path(arguments.work_dir).resolve() if arguments.work_dir else repository_root / f"build/parity-{corpus}"
  work_dir.mkdir(parents=True, exist_ok=True)

  print(f"[parity] compiler: {compiler}")
  print(f"[parity] corpus:   {corpus} ({corpus_path.relative_to(repository_root)})")
  print(f"[parity] cases:    {len(cases)} (jobs: {arguments.jobs})")

  with ThreadPoolExecutor(max_workers=max(1, arguments.jobs)) as executor:
    futures = [executor.submit(run_case, case, compiler, std_path, work_dir) for case in cases]
    results = []

    for index, future in enumerate(futures, start=1):
      result = future.result()
      results.append(result)

      if result.classification != CLASS_PASS:
        print(f"[parity] {index}/{len(cases)} {result.classification}: {result.case.name}")

  counts = {classification: 0 for classification in class_order}

  for result in results:
    counts[result.classification] = counts.get(result.classification, 0) + 1

  print("")
  print("class            count")

  for classification in class_order:
    print(f"{classification:<16} {counts.get(classification, 0)}")

  print(f"{'total':<16} {len(results)}")

  # A skipped err case is a diagnostic that was never compared, which a
  # promotion gate must not read as parity.
  passing_classes = (CLASS_PASS,) if corpus == CORPUS_ERR else (CLASS_PASS, CLASS_SKIPPED)
  failing = [result for result in results if result.classification not in passing_classes]

  if failing:
    print("")
    print("non-passing cases:")

    for result in failing:
      print(f"  {result.classification:<16} {result.case.name}: {result.reason}")

  if arguments.report:
    report_path = Path(arguments.report).resolve()
    report_path.parent.mkdir(parents=True, exist_ok=True)
    report_path.write_text(build_report(results, counts, corpus), encoding="utf-8")
    print("")
    print(f"[parity] report written to {report_path}")

  if arguments.counts_json:
    counts_path = Path(arguments.counts_json).resolve()
    counts_path.parent.mkdir(parents=True, exist_ok=True)
    counts_path.write_text(
      json.dumps(
        {
          "corpus": corpus,
          "total": len(results),
          "counts": {classification: counts.get(classification, 0) for classification in class_order},
          "failing": [
            {"case": result.case.name, "class": result.classification, "reason": result.reason}
            for result in failing
          ],
        },
        indent=2,
      )
      + "\n",
      encoding="utf-8",
    )
    print(f"[parity] counts written to {counts_path}")

  return 0 if not failing else 1


if __name__ == "__main__":
  sys.exit(main())
