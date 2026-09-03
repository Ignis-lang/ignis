#!/usr/bin/env python3
"""Compare what the host parser accepts with what a selfhost compiler accepts.

The gate (G6) is a parse-level comparison only: for every case the host is the
oracle and the selfhost has to reach the same verdict, accepted or rejected.
Diagnostics from later phases (analysis, ownership, codegen, linking) never
decide a case, so a program the host rejects for a type error still counts as
"parse accepted" on both sides.

The corpus has two halves:

  * every `.ign` file under `test_cases/`, `example/` and `std/`, parsed
    standalone. Parsing needs no import to resolve, and both compilers report a
    parse verdict for a file whose imports do not resolve, so a standalone file
    is still comparable.
  * every inline source string the host parser unit tests pass to their
    `parse`, `parse_expr`, `parse_stmt` and `parse_type` helpers, wrapped the
    way each helper wraps it.

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

# The host parser unit tests and the helper each one calls. `parse` takes a
# whole program; the other three wrap their argument, and the wrapper here is
# copied from the helper so both compilers see exactly what the host test sees.
HELPER_FILES = (
  "crates/ignis_parser/src/parser/declarations.rs",
  "crates/ignis_parser/src/parser/expression.rs",
  "crates/ignis_parser/src/parser/statement.rs",
  "crates/ignis_parser/src/parser/type_syntax.rs",
  "crates/ignis_parser/src/parser/mod.rs",
)

HELPER_WRAPPERS = {
  "parse": "{}",
  "parse_expr": "function test(): void {{ {}; }}",
  "parse_stmt": "function test(): void {{ {} }}",
  "parse_type": "function test(): {} {{ }}",
}

# The `I0xxx` codes either compiler's lexer or parser emits, taken from
# `crates/ignis_diagnostics/src/message.rs` and `ignis/diagnostics/codes.ign`
# and restricted to the codes their `lexer`/`parser` modules actually raise.
# The analyzer-only codes (I0031, I0033, I0041..I0043) are deliberately absent:
# they belong to a later phase and must not decide a parse verdict.
PARSE_DIAGNOSTIC_CODES = frozenset(
  {
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
  }
)

HOST_TIMEOUT_SECONDS = 120
SELFHOST_TIMEOUT_SECONDS = 300

OBSERVED_OUTPUT_LINES = 10

CLASS_PASS = "pass"
CLASS_SELFHOST_REJECTS = "selfhost-rejects"
CLASS_SELFHOST_ACCEPTS = "selfhost-accepts"
CLASS_HOST_ERROR = "host-error"
CLASS_SELFHOST_CRASH = "selfhost-crash"
CLASS_TIMEOUT = "timeout"

CLASS_ORDER = (
  CLASS_PASS,
  CLASS_SELFHOST_REJECTS,
  CLASS_SELFHOST_ACCEPTS,
  CLASS_HOST_ERROR,
  CLASS_SELFHOST_CRASH,
  CLASS_TIMEOUT,
)

CLASS_DESCRIPTIONS = {
  CLASS_PASS: "both compilers reach the same parse verdict",
  CLASS_SELFHOST_REJECTS: "the host parses the case, the selfhost rejects it",
  CLASS_SELFHOST_ACCEPTS: "the host rejects the case, the selfhost parses it",
  CLASS_HOST_ERROR: "the host produced no parse verdict",
  CLASS_SELFHOST_CRASH: "the selfhost produced no parse verdict",
  CLASS_TIMEOUT: "a compiler exceeded its time budget",
}

ORIGIN_REPOSITORY = "repository"
ORIGIN_PARSER_TEST = "parser-test"


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
  host: Verdict
  selfhost: Verdict
  reason: str = ""


# =============================================================================
# Corpus: repository sources
# =============================================================================


def collect_repository_cases(repository_root: Path) -> list[Case]:
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
          origin=ORIGIN_REPOSITORY,
          source=path.read_text(encoding="utf-8", errors="replace"),
          location=str(relative),
        )
      )

  return cases


def case_name_from_path(relative: Path) -> str:
  return re.sub(r"[^A-Za-z0-9]+", "_", str(relative.with_suffix("")))


# =============================================================================
# Corpus: inline sources in the host parser unit tests
# =============================================================================


class SourceScanner:
  """Minimal scanner over the Rust sources, sufficient for the call shapes used."""

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


HELPER_CALL_PATTERN = re.compile(
  r"(?<![A-Za-z0-9_])(" + "|".join(HELPER_WRAPPERS) + r")\s*\("
)
TEST_FUNCTION_PATTERN = re.compile(r"(?<![A-Za-z0-9_])fn\s+([A-Za-z0-9_]+)\s*\(")


IDENTIFIER_ARGUMENT_PATTERN = re.compile(r"([A-Za-z_][A-Za-z0-9_]*)\s*\)")


def enclosing_test_name(text: str, position: int) -> str:
  """The name of the `fn` the call sits in, used to name the case."""
  last = None

  for match in TEST_FUNCTION_PATTERN.finditer(text, 0, position):
    last = match.group(1)

  return last or "unknown"


def read_literal(scanner: SourceScanner) -> str | None:
  value = scanner.read_raw_string()

  if value is not None:
    return value

  value = scanner.read_plain_string()

  return unescape_rust_string(value) if value is not None else None


def resolve_binding(text: str, name: str, position: int) -> str | None:
  """The literal the last `let <name> = "..."` before `position` binds."""
  pattern = re.compile(r"(?<![A-Za-z0-9_])let\s+" + re.escape(name) + r"\s*(?::[^=;]+)?=\s*")
  found = None

  for match in pattern.finditer(text, 0, position):
    scanner = SourceScanner(text)
    scanner.position = match.end()
    value = read_literal(scanner)

    if value is not None:
      found = value

  return found


def collect_parser_test_cases(repository_root: Path) -> list[Case]:
  cases: list[Case] = []
  used_names: dict[str, int] = {}

  for relative_path in HELPER_FILES:
    path = repository_root / relative_path

    if not path.is_file():
      continue

    text = path.read_text(encoding="utf-8")
    file_stem = path.stem

    for match in HELPER_CALL_PATTERN.finditer(text):
      helper = match.group(1)
      line_start = text.rfind("\n", 0, match.start())

      # `fn parse(` declares the helper instead of calling it.
      if text[line_start + 1:match.start()].rstrip().endswith("fn"):
        continue

      scanner = SourceScanner(text)
      scanner.position = match.end()
      scanner.skip_trivia()

      source = read_literal(scanner)

      if source is not None:
        scanner.skip_trivia()

        if scanner.position < len(text) and text[scanner.position] not in (")", ","):
          continue
      else:
        # `let source = "..."; parse(source);` is the other shape these tests
        # use. A call over a table of tuples or a `format!` carries no single
        # literal, and there is nothing to reconstruct from it.
        argument = IDENTIFIER_ARGUMENT_PATTERN.match(text, scanner.position)

        if argument is None:
          continue

        source = resolve_binding(text, argument.group(1), match.start())

        if source is None:
          continue

      base_name = f"{file_stem}__{enclosing_test_name(text, match.start())}"
      occurrence = used_names.get(base_name, 0)
      used_names[base_name] = occurrence + 1
      name = base_name if occurrence == 0 else f"{base_name}__{occurrence + 1}"

      line = text.count("\n", 0, match.start()) + 1

      cases.append(
        Case(
          name=name,
          origin=ORIGIN_PARSER_TEST,
          source=HELPER_WRAPPERS[helper].format(source),
          location=f"{relative_path}:{line} ({helper})",
        )
      )

  return cases


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


def host_verdict(
  host: Path,
  case_file: Path,
  std_path: Path,
) -> Verdict:
  try:
    completed = subprocess.run(
      [str(host), "check", "--analyze-only", "--std-path", str(std_path), str(case_file)],
      cwd=case_file.parent,
      capture_output=True,
      text=True,
      errors="replace",
      timeout=HOST_TIMEOUT_SECONDS,
    )
  except subprocess.TimeoutExpired:
    return Verdict(None, reason=f"the host exceeded {HOST_TIMEOUT_SECONDS}s")
  except OSError as error:
    return Verdict(None, reason=f"the host could not be run: {error}")

  output = completed.stdout + completed.stderr

  if completed.returncode < 0:
    return Verdict(
      None,
      reason=f"the host was killed by signal {-completed.returncode}",
      output_tail=last_lines(output, OBSERVED_OUTPUT_LINES),
    )

  if "panicked at" in output:
    return Verdict(None, reason="the host panicked", output_tail=last_lines(output, OBSERVED_OUTPUT_LINES))

  codes = parse_error_codes(output, case_file)

  return Verdict(not codes, codes, output_tail=last_lines(output, OBSERVED_OUTPUT_LINES))


def selfhost_phase_errors(output: str) -> int | None:
  """The lex and parse error counts the selfhost's phase report prints.

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
      [str(compiler), str(case_file), "-o", str(case_dir / "case_bin")],
      cwd=case_dir,
      capture_output=True,
      text=True,
      errors="replace",
      timeout=SELFHOST_TIMEOUT_SECONDS,
    )
  except subprocess.TimeoutExpired:
    return Verdict(None, reason=f"the selfhost exceeded {SELFHOST_TIMEOUT_SECONDS}s")
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


def classify(host: Verdict, selfhost: Verdict) -> tuple[str, str]:
  if host.accepted is None and "exceeded" in host.reason:
    return CLASS_TIMEOUT, host.reason

  if selfhost.accepted is None and "exceeded" in selfhost.reason:
    return CLASS_TIMEOUT, selfhost.reason

  if host.accepted is None:
    return CLASS_HOST_ERROR, host.reason

  if selfhost.accepted is None:
    return CLASS_SELFHOST_CRASH, selfhost.reason

  if host.accepted == selfhost.accepted:
    return CLASS_PASS, ""

  if host.accepted:
    return CLASS_SELFHOST_REJECTS, "the host parses this source, the selfhost reports {}".format(
      ", ".join(selfhost.codes) or "a parse error"
    )

  return CLASS_SELFHOST_ACCEPTS, "the host reports {}, the selfhost parses this source".format(
    ", ".join(host.codes) or "a parse error"
  )


def run_case(case: Case, host: Path, compiler: Path, std_path: Path, work_dir: Path) -> CaseResult:
  case_dir = materialise_case(case, std_path, work_dir)
  case_file = case_dir / "src" / "main.ign"

  host_result = host_verdict(host, case_file, std_path)
  selfhost_result = selfhost_verdict(compiler, case_file, case_dir)
  classification, reason = classify(host_result, selfhost_result)

  return CaseResult(case, classification, host_result, selfhost_result, reason)


# =============================================================================
# Reporting
# =============================================================================


def build_report(results: list[CaseResult], counts: dict[str, int]) -> str:
  lines = [
    "# Selfhost syntax parity report (G6)",
    "",
    "The host is the oracle: for every case both compilers must reach the same",
    "parse verdict. Only lexer and parser diagnostics reported against the case",
    "file decide a verdict; later phases never do.",
    "",
    "Wrappers used for the host parser unit tests:",
    "",
  ]

  for helper, wrapper in HELPER_WRAPPERS.items():
    shown = wrapper.replace("{}", "<source>").replace("{{", "{").replace("}}", "}")
    lines.append(f"- `{helper}` -> `{shown}`")

  lines.extend(["", "## Summary", "", "| class | count | meaning |", "| --- | --- | --- |"])

  for classification in CLASS_ORDER:
    lines.append(
      f"| {classification} | {counts.get(classification, 0)} | {CLASS_DESCRIPTIONS[classification]} |"
    )

  lines.extend(["", f"| total | {len(results)} |", ""])

  origins = {}

  for result in results:
    bucket = origins.setdefault(result.case.origin, {"total": 0, "failing": 0})
    bucket["total"] += 1

    if result.classification != CLASS_PASS:
      bucket["failing"] += 1

  lines.extend(["## By origin", "", "| origin | cases | non-pass |", "| --- | --- | --- |"])

  for origin in sorted(origins):
    lines.append(f"| {origin} | {origins[origin]['total']} | {origins[origin]['failing']} |")

  lines.append("")

  for classification in CLASS_ORDER:
    if classification == CLASS_PASS:
      continue

    selected = [result for result in results if result.classification == classification]

    if not selected:
      continue

    lines.extend([f"## {classification} ({len(selected)})", ""])

    for result in selected:
      lines.extend(
        [
          f"### `{result.case.name}`",
          "",
          f"- origin: `{result.case.location}`",
          f"- host: {result.host.describe()}",
          f"- selfhost: {result.selfhost.describe()}",
          "",
          result.reason or "(no reason recorded)",
          "",
        ]
      )

      if result.selfhost.output_tail:
        lines.extend(["Selfhost printed:", "", "```", "\n".join(result.selfhost.output_tail), "```", ""])

      if result.host.output_tail:
        lines.extend(["The host printed:", "", "```", "\n".join(result.host.output_tail), "```", ""])

  return "\n".join(lines) + "\n"


def failing_entries(results: list[CaseResult]) -> list[dict]:
  return [
    {
      "case": result.case.name,
      "origin": result.case.location,
      "class": result.classification,
      "host": result.host.describe(),
      "selfhost": result.selfhost.describe(),
      "reason": result.reason,
    }
    for result in results
    if result.classification != CLASS_PASS
  ]


def build_gate(results: list[CaseResult], counts: dict[str, int]) -> dict:
  """Describe the run as a bootstrap gate result (G6)."""
  total = len(results)
  passed = counts.get(CLASS_PASS, 0)
  status = "pass" if total > 0 and passed == total else "fail"

  return {
    "gate": "G6",
    "status": status,
    "summary": f"syntax parity {passed}/{total}",
    "details": {
      "corpus": "syntax",
      "total": total,
      "counts": {classification: counts.get(classification, 0) for classification in CLASS_ORDER},
      "failing": failing_entries(results),
    },
  }


def main() -> int:
  parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
  parser.add_argument("--compiler", required=True, help="selfhost-built compiler binary")
  parser.add_argument("--host", default="ignis", help="host compiler binary (default: ignis on PATH)")
  parser.add_argument("--std", help="std directory (default: <repo>/std)")
  parser.add_argument("--jobs", type=int, default=os.cpu_count() or 1, help="parallel cases")
  parser.add_argument("--filter", help="only run cases whose name contains this substring")
  parser.add_argument("--report", help="write a Markdown report to this path")
  parser.add_argument("--counts-json", help="write the per-class counts to this path as JSON")
  parser.add_argument("--work-dir", help="directory for the generated projects")
  parser.add_argument("--gate-json", help="write the G6 bootstrap gate result to this path")
  arguments = parser.parse_args()

  repository_root = Path(__file__).resolve().parent.parent
  compiler = Path(arguments.compiler).resolve()

  if not compiler.is_file():
    print(f"error: compiler not found: {compiler}", file=sys.stderr)
    return 2

  host = Path(arguments.host)

  if host.name == str(host):
    resolved_host = shutil.which(arguments.host)

    if resolved_host is None:
      print(f"error: host compiler not found: {arguments.host}", file=sys.stderr)
      return 2

    host = Path(resolved_host)

  host = host.resolve()

  if not host.is_file():
    print(f"error: host compiler not found: {host}", file=sys.stderr)
    return 2

  std_path = Path(arguments.std).resolve() if arguments.std else repository_root / "std"

  if not std_path.is_dir():
    print(f"error: std directory not found: {std_path}", file=sys.stderr)
    return 2

  cases = collect_repository_cases(repository_root) + collect_parser_test_cases(repository_root)

  if arguments.filter:
    cases = [case for case in cases if arguments.filter in case.name]

  work_dir = Path(arguments.work_dir).resolve() if arguments.work_dir else repository_root / "build/parity-syntax"
  work_dir.mkdir(parents=True, exist_ok=True)

  print(f"[syntax] host:     {host}")
  print(f"[syntax] compiler: {compiler}")
  print(f"[syntax] cases:    {len(cases)} (jobs: {arguments.jobs})")

  with ThreadPoolExecutor(max_workers=max(1, arguments.jobs)) as executor:
    futures = [executor.submit(run_case, case, host, compiler, std_path, work_dir) for case in cases]
    results = []

    for index, future in enumerate(futures, start=1):
      result = future.result()
      results.append(result)

      if result.classification != CLASS_PASS:
        print(f"[syntax] {index}/{len(cases)} {result.classification}: {result.case.name}")

  counts = {classification: 0 for classification in CLASS_ORDER}

  for result in results:
    counts[result.classification] = counts.get(result.classification, 0) + 1

  print("")
  print("class            count")

  for classification in CLASS_ORDER:
    print(f"{classification:<16} {counts.get(classification, 0)}")

  print(f"{'total':<16} {len(results)}")

  failing = [result for result in results if result.classification != CLASS_PASS]

  if failing:
    print("")
    print("non-passing cases:")

    for result in failing:
      print(f"  {result.classification:<16} {result.case.name}: {result.reason}")

  if arguments.report:
    report_path = Path(arguments.report).resolve()
    report_path.parent.mkdir(parents=True, exist_ok=True)
    report_path.write_text(build_report(results, counts), encoding="utf-8")
    print("")
    print(f"[syntax] report written to {report_path}")

  if arguments.counts_json:
    counts_path = Path(arguments.counts_json).resolve()
    counts_path.parent.mkdir(parents=True, exist_ok=True)
    counts_path.write_text(
      json.dumps(
        {
          "corpus": "syntax",
          "total": len(results),
          "counts": {classification: counts.get(classification, 0) for classification in CLASS_ORDER},
          "failing": failing_entries(results),
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
    gate_path.write_text(json.dumps(build_gate(results, counts), indent=2) + "\n", encoding="utf-8")
    print(f"[syntax] gate result written to {gate_path}")

  return 0 if not failing else 1


if __name__ == "__main__":
  sys.exit(main())
