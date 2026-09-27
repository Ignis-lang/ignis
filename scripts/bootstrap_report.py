#!/usr/bin/env python3
"""Promotion gates for the selfhost bootstrap ladder.

Two commands, both driven by `scripts/bootstrap.sh`:

  gate-g3   Judge a selfhost test run under a built stage and write the gate
            result: the run has to pass outright, and with --reference-log
            it also has to match stage1's run of the same suite (default
            build/bootstrap/gates/G3.json for stage2; --label/--gate-id let a
            caller judge another stage, e.g. stage1 on its own).
  report    Read build/bootstrap/gates/*.json and write build/bootstrap/report.md
            and build/bootstrap/promotion.json.

`report` always exits 0. Whether the run is a promotion candidate is data in
promotion.json, not the exit code: the report has to exist even for a run where
every gate failed.
"""

import argparse
import json
import re
import subprocess
import sys
from datetime import datetime, timezone
from pathlib import Path

# Gates whose status decides promotion. A gate stays out of this tuple until a
# failure of it is a reason not to ship. G7 (drop-schedule parity) joined this
# set once the selfhost's ownership analysis reached 623/623 own-code parity
# with the host (nightly-34803378772): a divergence is now a real ownership
# bug, not the expected state, so it holds the `candidate` verdict like every
# other gate.
GATE_IDS = ("G1", "G2", "G3", "G4", "G5", "G6", "G7")

# No gate is informational anymore, but the split is kept so a future gate can
# be reported without blocking promotion the same way G7 used to.
INFORMATIONAL_GATE_IDS = ()

REPORTED_GATE_IDS = GATE_IDS + INFORMATIONAL_GATE_IDS

GATE_TITLES = {
  "G1": "Fixed point (stage3 C identical to stage2)",
  "G2": "End-to-end parity under stage2",
  "G3": "Selfhost test suite under stage2",
  "G4": "Resource budget within 1.25x of stage1",
  "G5": "Diagnostics keep every one the committed error corpus records",
  "G6": "Parse verdicts match the committed baselines",
  "G7": "Drop schedules match the committed baselines",
}

STATUS_PASS = "pass"
STATUS_FAIL = "fail"
STATUS_SKIPPED = "skipped"

ANSI_PATTERN = re.compile(r"\x1b\[[0-9;]*[A-Za-z]")
TEST_LINE_PATTERN = re.compile(r"^\s*-\s+(?P<name>\S+)\s+\.\.\.\s+(?P<status>ok|FAILED|skip \(.*\))\s*$")
SUMMARY_COUNT_PATTERN = re.compile(r"^\s*-\s+(?P<count>\d+)\s+(?P<label>total|passed|failed|skipped)\s*$")
SUMMARY_HEADER = "Summary"

ERROR_LINE_PATTERN = re.compile(r"^(Error\[|Error:|error(\[|:))")

# The run G3 compares stage2's against. It used to be the host compiler's.
REFERENCE_LABEL = "stage1"

LOG_TAIL_LINES = 40
LOG_ERROR_LINES = 20


# =============================================================================
# G3: selfhost test suite
# =============================================================================


def strip_ansi(text: str) -> str:
  return ANSI_PATTERN.sub("", text)


def parse_test_log(text: str) -> dict:
  """Extract the per-test lines and the `• Summary` block from a test run.

  Only the lines the runner prints for each test and the four summary counts
  (total, passed, failed, skipped) are read. Everything else (phase reports,
  failure details, timings) differs between two runs of the same suite and
  says nothing about the result.
  """
  results: dict[str, str] = {}
  summary: dict[str, int] = {}
  in_summary = False

  for raw_line in strip_ansi(text).splitlines():
    line = raw_line.rstrip()

    if line.lstrip().startswith("•"):
      in_summary = line.lstrip().lstrip("•").strip() == SUMMARY_HEADER
      continue

    if in_summary:
      count_match = SUMMARY_COUNT_PATTERN.match(line)

      if count_match:
        summary[count_match.group("label")] = int(count_match.group("count"))
        continue

    test_match = TEST_LINE_PATTERN.match(line)

    if test_match:
      results[test_match.group("name")] = test_match.group("status")

  return {"tests": results, "summary": summary}


def log_tail(path: Path) -> list[str]:
  if not path.is_file():
    return []

  lines = strip_ansi(path.read_text(encoding="utf-8", errors="replace")).splitlines()

  return [line.rstrip() for line in lines[-LOG_TAIL_LINES:]]


def log_errors(path: Path) -> list[str]:
  """Pick the error lines out of a run that produced no summary.

  The tail of such a log is usually warnings, so the line that explains the
  failure would otherwise not reach the report.
  """
  if not path.is_file():
    return []

  found = []

  for line in strip_ansi(path.read_text(encoding="utf-8", errors="replace")).splitlines():
    stripped = line.strip()

    if ERROR_LINE_PATTERN.match(stripped):
      found.append(stripped)

    if len(found) == LOG_ERROR_LINES:
      break

  return found


def failing_names(parsed: dict) -> set[str]:
  return {name for name, status in parsed["tests"].items() if status == "FAILED"}


def skipped_names(parsed: dict) -> set[str]:
  return {name for name, status in parsed["tests"].items() if status.startswith("skip (")}


def has_summary(parsed: dict) -> bool:
  return {"total", "passed", "failed"}.issubset(parsed["summary"])


def read_test_log(path: Path) -> dict:
  if not path.is_file():
    return {"tests": {}, "summary": {}}

  return parse_test_log(path.read_text(encoding="utf-8", errors="replace"))


def summary_counts(parsed: dict) -> tuple[int, int, int, int]:
  summary = parsed["summary"]

  return (summary["total"], summary["passed"], summary["failed"], summary.get("skipped", 0))


def run_details(log: Path, exit_status: int, parsed: dict) -> dict:
  return {
    "log": str(log),
    "exit_status": exit_status,
    "summary": parsed["summary"],
    "tests": len(parsed["tests"]),
  }


def no_summary_reason(label: str, exit_status: int, timeout_seconds: int) -> str:
  if exit_status == 124:
    return f"{label} timed out after {timeout_seconds}s"

  return f"{label} produced no test summary (exit {exit_status})"


def build_gate_g3(arguments: argparse.Namespace) -> dict:
  """Judge a built stage's run of the selfhost test suite.

  The run has to pass outright: a summary, no failing test, and a zero exit.
  When a reference run is given (the nightly's stage2 gate passes stage1's run
  of the same suite), both runs also have to report the same test names, the
  same skipped set and the same counts, so a stage that quietly loses or skips
  tests cannot pass on a clean summary alone.

  `label` names the judged run in the JSON details and summary text ("stage2"
  for the promotion gate, "stage1" for ci.yml's PR-only check); `gate_id`
  names the JSON payload's "gate" field, so the two never collide.
  """
  label = arguments.label
  gate_id = arguments.gate_id
  timeout_seconds = arguments.timeout_seconds
  log = Path(arguments.log)
  run = read_test_log(log)

  details = {
    label: run_details(log, arguments.status, run),
    "failing": sorted(failing_names(run)),
    "timeout_seconds": timeout_seconds,
  }

  reference = None
  reference_log = None

  if arguments.reference_log is not None:
    reference_log = Path(arguments.reference_log)
    reference = read_test_log(reference_log)
    details[REFERENCE_LABEL] = run_details(reference_log, arguments.reference_status, reference)

  timed_out = [label] if arguments.status == 124 else []

  if reference is not None and arguments.reference_status == 124:
    timed_out.append(REFERENCE_LABEL)

  if timed_out:
    details["timed_out"] = timed_out

  def fail(reason: str) -> dict:
    return {"gate": gate_id, "status": STATUS_FAIL, "summary": reason, "details": details}

  if not has_summary(run):
    details[label]["errors"] = log_errors(log)
    details[label]["log_tail"] = log_tail(log)

    return fail(no_summary_reason(label, arguments.status, timeout_seconds))

  counts = summary_counts(run)
  failed = max(counts[2], len(details["failing"]))

  if failed:
    noun = "test" if failed == 1 else "tests"

    return fail(f"{failed} {noun} failed under {label}")

  if arguments.status != 0:
    return fail(f"{label} reported no failing test but exited non-zero (exit {arguments.status})")

  passing = f"{counts[1]}/{counts[0]} passing ({counts[3]} skipped)"

  if reference is None:
    return {"gate": gate_id, "status": STATUS_PASS, "summary": f"{label}: {passing}", "details": details}

  if not has_summary(reference):
    details[REFERENCE_LABEL]["errors"] = log_errors(reference_log)
    details[REFERENCE_LABEL]["log_tail"] = log_tail(reference_log)

    return fail(no_summary_reason(REFERENCE_LABEL, arguments.reference_status, timeout_seconds))

  if arguments.reference_status != 0:
    return fail(f"{REFERENCE_LABEL} exited non-zero (exit {arguments.reference_status})")

  run_skipped = skipped_names(run)
  reference_skipped = skipped_names(reference)

  details[f"missing_from_{label}"] = sorted(set(reference["tests"]) - set(run["tests"]))
  details[f"missing_from_{REFERENCE_LABEL}"] = sorted(set(run["tests"]) - set(reference["tests"]))
  details[f"skipped_only_under_{label}"] = sorted(run_skipped - reference_skipped)
  details[f"skipped_only_under_{REFERENCE_LABEL}"] = sorted(reference_skipped - run_skipped)

  reference_counts = summary_counts(reference)

  if counts != reference_counts:
    return fail(
      f"{label} reported {counts[1]}/{counts[0]} passing ({counts[3]} skipped), "
      f"{REFERENCE_LABEL} {reference_counts[1]}/{reference_counts[0]} ({reference_counts[3]} skipped)"
    )

  if set(run["tests"]) != set(reference["tests"]):
    return fail(f"{label} and {REFERENCE_LABEL} report different test names")

  if run_skipped != reference_skipped:
    return fail(f"{label} and {REFERENCE_LABEL} skip different tests")

  return {
    "gate": gate_id,
    "status": STATUS_PASS,
    "summary": f"{label} matches {REFERENCE_LABEL}: {passing}",
    "details": details,
  }


def command_gate_g3(arguments: argparse.Namespace) -> int:
  gate = build_gate_g3(arguments)

  output = Path(arguments.output)
  output.parent.mkdir(parents=True, exist_ok=True)
  output.write_text(json.dumps(gate, indent=2) + "\n", encoding="utf-8")

  print(f"[bootstrap] gate {gate['gate']}: {gate['status']} — {gate['summary']}", file=sys.stderr)

  return 0


# =============================================================================
# Promotion report
# =============================================================================


def read_gate(path: Path) -> dict:
  try:
    payload = json.loads(path.read_text(encoding="utf-8"))
  except (OSError, json.JSONDecodeError) as error:
    return {
      "gate": path.stem,
      "status": STATUS_FAIL,
      "summary": f"unreadable gate result: {error}",
      "details": {},
    }

  if not isinstance(payload, dict):
    return {"gate": path.stem, "status": STATUS_FAIL, "summary": "gate result is not an object", "details": {}}

  payload.setdefault("gate", path.stem)
  payload.setdefault("status", STATUS_SKIPPED)
  payload.setdefault("summary", "")
  payload.setdefault("details", {})

  return payload


def collect_gates(gates_dir: Path) -> dict[str, dict]:
  found = {}

  if gates_dir.is_dir():
    for path in sorted(gates_dir.glob("*.json")):
      gate = read_gate(path)
      found[str(gate["gate"])] = gate

  gates = {}

  for gate_id in REPORTED_GATE_IDS:
    gates[gate_id] = found.pop(
      gate_id,
      {"gate": gate_id, "status": STATUS_SKIPPED, "summary": "no result was produced", "details": {}},
    )

  # A gate file this script does not know about is still reported rather than
  # dropped, so a new gate shows up before the report learns its name.
  for gate_id in sorted(found):
    gates[gate_id] = found[gate_id]

  return gates


def read_commit(project_root: Path) -> str:
  try:
    completed = subprocess.run(
      ["git", "rev-parse", "HEAD"],
      cwd=project_root,
      capture_output=True,
      text=True,
      check=False,
    )
  except OSError:
    return "unknown"

  return completed.stdout.strip() or "unknown"


def read_stage0(bootstrap_root: Path) -> dict:
  path = bootstrap_root / "stage0.json"

  # scripts/bootstrap.sh records every official or seed stage0 it builds with,
  # so a missing file means stage1 came from an explicit IGNIS_STAGE0 of no
  # recorded kind. An unreadable one is reported as such, not as missing.
  if not path.is_file():
    return {}

  try:
    return json.loads(path.read_text(encoding="utf-8"))
  except (OSError, json.JSONDecodeError):
    return {"unreadable": True}


def format_stage0_line(stage0: dict) -> str:
  kind = stage0.get("kind")

  if kind == "official":
    sha256 = stage0.get("sha256") or "unknown"
    return f"stage0: official (sha {sha256})"

  if kind == "seed":
    seed_sha256 = stage0.get("seed_xz_sha256") or "unknown"
    return f"stage0: seed (bootstrap/seed xz sha {seed_sha256})"

  if kind:
    return f"stage0: {kind}"

  if stage0.get("unreadable"):
    return "stage0: unknown (build/bootstrap/stage0.json is unreadable)"

  return "stage0: not recorded (no build/bootstrap/stage0.json)"


def collect_stage_logs(bootstrap_root: Path) -> list[dict]:
  logs = []

  for stage in ("stage1", "stage2", "stage3", "stage2-tests"):
    path = bootstrap_root / stage / "log.txt"

    if not path.is_file():
      continue

    tail = log_tail(path)
    logs.append({"stage": stage, "path": str(path), "lines": len(tail), "tail": tail})

  return logs


def format_report(
  commit: str,
  generated_at: str,
  gates: dict[str, dict],
  candidate: bool,
  stage_logs: list[dict],
  stage0: dict,
) -> str:
  lines = [
    "# Selfhost bootstrap promotion report",
    "",
    f"- {format_stage0_line(stage0)}",
    f"- Commit: `{commit}`",
    f"- Generated: {generated_at}",
    f"- Candidate: **{'yes' if candidate else 'no'}**",
    "",
    "A run is a candidate when every promotion gate passes. Three consecutive",
    "candidate nightly runs promote the stage2 binary to official.",
  ]

  if INFORMATIONAL_GATE_IDS:
    lines += [
      "Gates marked informational (" + ", ".join(INFORMATIONAL_GATE_IDS) + ") are reported",
      "but never counted towards that verdict.",
    ]

  lines += [
    "",
    "## Gates",
    "",
    "| gate | status | summary |",
    "| --- | --- | --- |",
  ]

  for gate_id, gate in gates.items():
    title = GATE_TITLES.get(gate_id, gate_id)
    summary = str(gate["summary"]).replace("|", "\\|") or "—"
    status = f"`{gate['status']}`" + (" (informational)" if gate_id in INFORMATIONAL_GATE_IDS else "")
    lines.append(f"| **{gate_id}** {title} | {status} | {summary} |")

  lines.extend(["", "## Details", ""])

  for gate_id, gate in gates.items():
    lines.extend(
      [
        f"### {gate_id} — {gate['status']}",
        "",
        GATE_TITLES.get(gate_id, gate_id),
        "",
        str(gate["summary"]) or "(no summary recorded)",
        "",
        "```json",
        json.dumps(gate["details"], indent=2),
        "```",
        "",
      ]
    )

  lines.extend(["## Stage logs", ""])

  if not stage_logs:
    lines.extend(["No stage log was produced by this run.", ""])
  else:
    for log in stage_logs:
      lines.extend(
        [
          f"### `{log['stage']}`",
          "",
          f"`{log['path']}` (last {log['lines']} lines)",
          "",
          "```",
          "\n".join(log["tail"]),
          "```",
          "",
        ]
      )

  return "\n".join(lines) + "\n"


def command_report(arguments: argparse.Namespace) -> int:
  bootstrap_root = Path(arguments.bootstrap_root).resolve()
  project_root = Path(arguments.project_root).resolve()

  gates = collect_gates(bootstrap_root / "gates")
  commit = read_commit(project_root)
  generated_at = datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")
  candidate = all(gates[gate_id]["status"] == STATUS_PASS for gate_id in GATE_IDS)
  stage0 = read_stage0(bootstrap_root)

  bootstrap_root.mkdir(parents=True, exist_ok=True)

  report_path = bootstrap_root / "report.md"
  report_path.write_text(
    format_report(commit, generated_at, gates, candidate, collect_stage_logs(bootstrap_root), stage0),
    encoding="utf-8",
  )

  promotion_path = bootstrap_root / "promotion.json"
  promotion_path.write_text(
    json.dumps(
      {
        "commit": commit,
        "generated_at": generated_at,
        "candidate": candidate,
        "stage0": stage0,
        "gates": {
          gate_id: {"status": gate["status"], "summary": gate["summary"]} for gate_id, gate in gates.items()
        },
      },
      indent=2,
    )
    + "\n",
    encoding="utf-8",
  )

  print(f"[bootstrap] {format_stage0_line(stage0)}", file=sys.stderr)
  print(f"[bootstrap] report    -> {report_path}", file=sys.stderr)
  print(f"[bootstrap] promotion -> {promotion_path}", file=sys.stderr)
  print(f"[bootstrap] candidate: {'yes' if candidate else 'no'}", file=sys.stderr)

  for gate_id, gate in gates.items():
    print(f"[bootstrap]   {gate_id}: {gate['status']} — {gate['summary']}", file=sys.stderr)

  return 0


def main() -> int:
  parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
  subparsers = parser.add_subparsers(dest="command", required=True)

  gate_g3 = subparsers.add_parser("gate-g3", help="judge a built stage's run of the selfhost test suite")
  gate_g3.add_argument("--log", required=True, help="captured output of the built-stage test run")
  gate_g3.add_argument("--status", type=int, default=0, help="exit status of the built-stage run")
  gate_g3.add_argument(
    "--reference-log", help="captured output of stage1's run of the same suite, to compare against (optional)"
  )
  gate_g3.add_argument("--reference-status", type=int, default=0, help="exit status of the reference run")
  gate_g3.add_argument("--timeout-seconds", type=int, default=0, help="timeout the runs were given")
  gate_g3.add_argument("--output", required=True, help="path of the gate result")
  gate_g3.add_argument(
    "--label", default="stage2", help="name of the built-stage side in the JSON details (default: stage2)"
  )
  gate_g3.add_argument("--gate-id", default="G3", help="value of the JSON payload's \"gate\" field (default: G3)")

  report = subparsers.add_parser("report", help="write report.md and promotion.json")
  report.add_argument("--bootstrap-root", required=True, help="build/bootstrap directory")
  report.add_argument("--project-root", required=True, help="repository root, read for the commit")

  arguments = parser.parse_args()

  if arguments.command == "gate-g3":
    return command_gate_g3(arguments)

  return command_report(arguments)


if __name__ == "__main__":
  sys.exit(main())
