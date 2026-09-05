#!/usr/bin/env python3
"""Run a host end-to-end corpus through a selfhost-built compiler.

`--corpus ok` (the default) materialises every `.ign` fixture under
`test_cases/e2e/ok` as a small Ignis project, compiles it with the given
compiler binary and runs it. The observed exit code, stdout and stderr are
formatted exactly like the host's `format_e2e_result` and compared with the
fixture's own `__snapshots__/<name>.snap` baseline.

A fixture's leading `// e2e: <option>` header lines select its mode, the same
way `ignis test` reads them (see `crates/ignis_driver/src/fixture_tests.rs`):
`std` forces the standard library on (irrelevant here, since every generated
project already links std), `allow-leak` exempts the case from leak checking,
`err` and `warn` route it to the error corpus below instead of a program run.

Cases run under LeakSanitizer are replayed the same way the host testsuite
would: the generated project asks for `-fsanitize=leak` through
`[build] cflags` and the binary runs with leak checking on, so a selfhost
whose output leaks memory is reported as `leak` instead of passing.
`--no-leak-check` turns that off.

`--corpus err` runs every fixture under `test_cases/e2e/err` under the
"equal or better" rule: every diagnostic line the fixture's baseline records
must appear in the selfhost's output, while diagnostics the baseline does not
emit are allowed. This covers both `// e2e: err` fixtures (the baseline holds
error diagnostics) and `// e2e: warn` fixtures (the baseline holds warning
diagnostics); cases are only compiled, never run.
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

# A `// e2e: err` fixture's baseline holds error diagnostics; a `// e2e: warn`
# fixture's baseline holds warning diagnostics. Neither runs the produced
# binary.
KIND_RUN = "run"
KIND_ERROR = "error"
KIND_WARNING = "warning"

FIXTURE_DIR = {
  CORPUS_OK: "test_cases/e2e/ok",
  CORPUS_ERR: "test_cases/e2e/err",
}

FIXTURE_HEADER_MARKER = "// e2e:"

# The header option that exempts a fixture from leak checking, mirroring the
# host's escape hatch for a case whose leak is known and accepted.
LEAK_EXEMPT_HELPERS = {"allow-leak"}

LEAK_CFLAGS = ("-fsanitize=leak", "-g", "-fno-omit-frame-pointer")
LSAN_ENVIRONMENT = "detect_leaks=1:leak_check_at_exit=1"
LSAN_EXIT_CODE = 23
LSAN_HEADER = "ERROR: LeakSanitizer:"

COMPILE_TIMEOUT_SECONDS = 120
RUN_TIMEOUT_SECONDS = 10

OBSERVED_OUTPUT_LINES = 10

CLASS_PASS = "pass"
CLASS_MISMATCH = "mismatch"
CLASS_LEAK = "leak"
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
    CLASS_LEAK,
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
  leak_report: str = ""


def parse_fixture_header(source: str) -> tuple[str, str]:
  """Reads a fixture's mode from its leading `// e2e: <option>` comment lines.

  Mirrors `crates/ignis_driver/src/fixture_tests.rs::parse_fixture_header`:
  scanning stops at the first line that is neither blank nor a comment.
  Returns `(kind, helper)`, where `helper` is `"allow-leak"` for a program
  fixture that opted out of leak checking, and `""` otherwise.
  """
  is_err = False
  is_warn = False
  allow_leak = False

  for line in source.splitlines():
    trimmed = line.strip()

    if trimmed == "":
      continue

    if not trimmed.startswith("//"):
      break

    if not trimmed.startswith(FIXTURE_HEADER_MARKER):
      continue

    options = trimmed[len(FIXTURE_HEADER_MARKER):]

    for option in options.split(","):
      option = option.strip()

      if option == "err":
        is_err = True
      elif option == "warn":
        is_warn = True
      elif option == "allow-leak":
        allow_leak = True

  if is_err:
    return KIND_ERROR, ""

  if is_warn:
    return KIND_WARNING, ""

  return KIND_RUN, ("allow-leak" if allow_leak else "")


def extract_fixture_cases(corpus: str, repository_root: Path) -> list[Case]:
  """Reads every `.ign` fixture under the corpus directory, source and header."""
  fixture_dir = repository_root / FIXTURE_DIR[corpus]
  cases: list[Case] = []

  for fixture_path in sorted(fixture_dir.rglob("*.ign")):
    name = fixture_path.relative_to(fixture_dir).with_suffix("").as_posix()
    source = fixture_path.read_text(encoding="utf-8")
    kind, helper = parse_fixture_header(source)

    cases.append(Case(name, helper, source, kind))

  return cases


def read_fixture_snapshot(snapshot_path: Path) -> str | None:
  """Reads a fixture baseline, which is the raw expected body with no header."""
  if not snapshot_path.is_file():
    return None

  return snapshot_path.read_text(encoding="utf-8")


def format_e2e_result(exit_code: int, stdout: str, stderr: str) -> str:
  return "exit_code: {}\nstdout: {}\nstderr: {}".format(
    exit_code,
    "(empty)" if stdout == "" else stdout.rstrip(),
    "(empty)" if stderr == "" else stderr.rstrip(),
  )


def project_manifest(std_path: Path, leak_check: bool) -> str:
  cflags = ", ".join(f'"{flag}"' for flag in LEAK_CFLAGS) if leak_check else ""

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
    f"cflags = [{cflags}]\n"
  )


def leak_checked(case: Case, leak_check: bool) -> bool:
  """Whether this case is compiled and run under LeakSanitizer."""
  return leak_check and case.kind == KIND_RUN and case.helper not in LEAK_EXEMPT_HELPERS


def split_lsan_output(stderr: str) -> tuple[str, str]:
  """Split stderr into the program's own output and the LeakSanitizer report.

  Mirrors the host harness: the report starts at the `==<pid>==ERROR:
  LeakSanitizer:` line and runs to the end, so removing it leaves exactly what
  the recorded snapshot was taken from.
  """
  if LSAN_HEADER not in stderr:
    return stderr, ""

  user_lines = []
  leak_lines = []
  in_lsan = False

  for line in stderr.splitlines():
    if not in_lsan and line.startswith("==") and LSAN_HEADER in line:
      in_lsan = True

    if in_lsan:
      leak_lines.append(line)
    else:
      user_lines.append(line)

  return "\n".join(user_lines), "\n".join(leak_lines)


LSAN_SUMMARY_PATTERN = re.compile(r"SUMMARY: LeakSanitizer: (.+)$", re.MULTILINE)


def leak_summary(report: str) -> str:
  """The one line of an LSan report that says how much leaked."""
  match = LSAN_SUMMARY_PATTERN.search(report)

  return match.group(1).strip() if match else "LeakSanitizer reported a leak"


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


def materialise_case(case: Case, std_path: Path, work_dir: Path, leak_check: bool = False) -> Path:
  """Write the case as a single-file Ignis project and return its directory."""
  case_dir = work_dir / case.name
  shutil.rmtree(case_dir, ignore_errors=True)
  (case_dir / "src").mkdir(parents=True, exist_ok=True)
  (case_dir / "ignis.toml").write_text(project_manifest(std_path, leak_check), encoding="utf-8")
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


def run_case(
  case: Case,
  compiler: Path,
  std_path: Path,
  work_dir: Path,
  leak_check: bool,
) -> CaseResult:
  if case.skip_reason is not None:
    return CaseResult(case, CLASS_SKIPPED, case.skip_reason)

  if case.snapshot is None:
    return CaseResult(case, CLASS_SKIPPED, "no snapshot recorded")

  if case.kind == KIND_RUN:
    return run_ok_case(case, compiler, std_path, work_dir, leak_check)

  return run_err_case(case, compiler, std_path, work_dir)


def run_ok_case(
  case: Case,
  compiler: Path,
  std_path: Path,
  work_dir: Path,
  leak_check: bool,
) -> CaseResult:
  checked = leak_checked(case, leak_check)
  case_dir = materialise_case(case, std_path, work_dir, checked)
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

  environment = dict(os.environ)
  environment["LSAN_OPTIONS"] = LSAN_ENVIRONMENT if checked else "detect_leaks=0"

  try:
    execution = subprocess.run(
      [str(binary_path)],
      cwd=case_dir,
      capture_output=True,
      text=True,
      errors="replace",
      timeout=RUN_TIMEOUT_SECONDS,
      env=environment,
    )
  except subprocess.TimeoutExpired:
    return CaseResult(case, CLASS_RUN_TIMEOUT, f"program exceeded {RUN_TIMEOUT_SECONDS}s")

  user_stderr, leak_report = split_lsan_output(execution.stderr)
  leaked = checked and execution.returncode == LSAN_EXIT_CODE and leak_report != ""

  if leaked:
    return CaseResult(case, CLASS_LEAK, leak_summary(leak_report), leak_report=leak_report)

  # LSan writes nothing when the program is clean, so stripping its report only
  # matters for a binary that leaked without failing the run.
  observed = format_e2e_result(execution.returncode, execution.stdout, user_stderr)

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

      if result.leak_report:
        lines.extend(["```", result.leak_report, "```", ""])

      if result.compiler_tail:
        label = "Selfhost printed:" if result.case.kind != KIND_RUN else ""

        if label:
          lines.extend([label, ""])

        lines.extend(["```", "\n".join(result.compiler_tail), "```", ""])

  return "\n".join(lines) + "\n"


def build_gate(results: list[CaseResult], counts: dict[str, int]) -> dict:
  """Describe the corpus run as a bootstrap gate result (G2)."""
  failing = [result for result in results if result.classification not in (CLASS_PASS, CLASS_SKIPPED)]
  skipped = [result for result in results if result.classification == CLASS_SKIPPED]
  total = len(results)
  passed = counts.get(CLASS_PASS, 0)
  status = "pass" if total > 0 and passed == total else "fail"

  return {
    "gate": "G2",
    "status": status,
    "summary": f"e2e parity {passed}/{total}",
    "details": {
      "counts": {classification: counts.get(classification, 0) for classification in CLASS_ORDER},
      "total": total,
      "failing": [
        {"case": result.case.name, "classification": result.classification, "reason": result.reason}
        for result in failing
      ],
      "skipped": [{"case": result.case.name, "reason": result.reason} for result in skipped],
    },
  }


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
  parser.add_argument(
    "--leak-check",
    dest="leak_check",
    action=argparse.BooleanOptionalAction,
    default=None,
    help="compile and run the cases under LeakSanitizer (default: on for the ok corpus)",
  )
  parser.add_argument("--jobs", type=int, default=os.cpu_count() or 1, help="parallel cases")
  parser.add_argument("--filter", help="only run cases whose name contains this substring")
  parser.add_argument("--report", help="write a Markdown report to this path")
  parser.add_argument("--counts-json", help="write the per-class counts to this path as JSON")
  parser.add_argument("--work-dir", help="directory for the generated projects")
  parser.add_argument("--gate-json", help="write the G2 bootstrap gate result to this path")
  arguments = parser.parse_args()

  corpus = arguments.corpus
  class_order = CLASS_ORDER[corpus]
  leak_check = arguments.leak_check if arguments.leak_check is not None else corpus == CORPUS_OK
  repository_root = Path(__file__).resolve().parent.parent
  compiler = Path(arguments.compiler).resolve()

  if not compiler.is_file():
    print(f"error: compiler not found: {compiler}", file=sys.stderr)
    return 2

  std_path = Path(arguments.std).resolve() if arguments.std else repository_root / "std"

  if not std_path.is_dir():
    print(f"error: std directory not found: {std_path}", file=sys.stderr)
    return 2

  corpus_dir = repository_root / FIXTURE_DIR[corpus]
  cases = extract_fixture_cases(corpus, repository_root)

  if arguments.filter:
    cases = [case for case in cases if arguments.filter in case.name]

  for case in cases:
    case.snapshot = read_fixture_snapshot(corpus_dir / "__snapshots__" / f"{case.name}.snap")

  work_dir = Path(arguments.work_dir).resolve() if arguments.work_dir else repository_root / f"build/parity-{corpus}"
  work_dir.mkdir(parents=True, exist_ok=True)

  print(f"[parity] compiler: {compiler}")
  print(f"[parity] corpus:   {corpus} ({corpus_dir.relative_to(repository_root)})")
  print(f"[parity] cases:    {len(cases)} (jobs: {arguments.jobs})")
  print(f"[parity] leaks:    {'checked' if leak_check else 'not checked'}")

  with ThreadPoolExecutor(max_workers=max(1, arguments.jobs)) as executor:
    futures = [executor.submit(run_case, case, compiler, std_path, work_dir, leak_check) for case in cases]
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
  if arguments.gate_json:
    gate_path = Path(arguments.gate_json).resolve()
    gate_path.parent.mkdir(parents=True, exist_ok=True)
    gate_path.write_text(json.dumps(build_gate(results, counts), indent=2) + "\n", encoding="utf-8")
    print(f"[parity] gate result written to {gate_path}")

  return 0 if not failing else 1


if __name__ == "__main__":
  sys.exit(main())
