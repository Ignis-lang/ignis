#!/usr/bin/env python3
"""Promotion gates for the selfhost bootstrap ladder.

Two commands, both driven by `scripts/bootstrap.sh`:

  gate-g3   Compare a selfhost test run under stage2 with the same run under the
            host compiler and write build/bootstrap/gates/G3.json.
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

GATE_IDS = ("G1", "G2", "G3", "G4", "G5")

GATE_TITLES = {
  "G1": "Fixed point (stage3 C identical to stage2)",
  "G2": "End-to-end parity under stage2",
  "G3": "Selfhost test suite under stage2",
  "G4": "Resource budget within 1.25x of the host",
  "G5": "Diagnostics equal or better than the host",
}

STATUS_PASS = "pass"
STATUS_FAIL = "fail"
STATUS_SKIPPED = "skipped"

ANSI_PATTERN = re.compile(r"\x1b\[[0-9;]*[A-Za-z]")
TEST_LINE_PATTERN = re.compile(r"^\s*-\s+(?P<name>\S+)\s+\.\.\.\s+(?P<status>ok|FAILED)\s*$")
SUMMARY_COUNT_PATTERN = re.compile(r"^\s*-\s+(?P<count>\d+)\s+(?P<label>total|passed|failed)\s*$")
SUMMARY_HEADER = "Summary"

LOG_TAIL_LINES = 40


# =============================================================================
# G3: selfhost test suite parity
# =============================================================================


def strip_ansi(text: str) -> str:
  return ANSI_PATTERN.sub("", text)


def parse_test_log(text: str) -> dict:
  """Extract the per-test lines and the `• Summary` block from a test run.

  Only the lines the runner prints for each test and the three summary counts
  are read. Everything else (phase reports, failure details, timings) differs
  between two runs of the same suite and says nothing about the result.
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


def failing_names(parsed: dict) -> set[str]:
  return {name for name, status in parsed["tests"].items() if status == "FAILED"}


def has_summary(parsed: dict) -> bool:
  return {"total", "passed", "failed"}.issubset(parsed["summary"])


def build_gate_g3(arguments: argparse.Namespace) -> dict:
  stage2_log = Path(arguments.stage2_log)
  host_log = Path(arguments.host_log)

  stage2 = parse_test_log(stage2_log.read_text(encoding="utf-8", errors="replace")) if stage2_log.is_file() else {
    "tests": {},
    "summary": {},
  }
  host = parse_test_log(host_log.read_text(encoding="utf-8", errors="replace")) if host_log.is_file() else {
    "tests": {},
    "summary": {},
  }

  stage2_failing = failing_names(stage2)
  host_failing = failing_names(host)

  details = {
    "stage2": {
      "log": str(stage2_log),
      "exit_status": arguments.stage2_status,
      "summary": stage2["summary"],
      "tests": len(stage2["tests"]),
    },
    "host": {
      "log": str(host_log),
      "exit_status": arguments.host_status,
      "summary": host["summary"],
      "tests": len(host["tests"]),
    },
    "failing_only_under_stage2": sorted(stage2_failing - host_failing),
    "failing_only_under_host": sorted(host_failing - stage2_failing),
    "missing_from_stage2": sorted(set(host["tests"]) - set(stage2["tests"])),
    "missing_from_host": sorted(set(stage2["tests"]) - set(host["tests"])),
    "timeout_seconds": arguments.timeout_seconds,
  }

  timed_out = [
    name
    for name, status in (("stage2", arguments.stage2_status), ("host", arguments.host_status))
    if status == 124
  ]

  if timed_out:
    details["timed_out"] = timed_out

  if not has_summary(stage2):
    details["stage2"]["log_tail"] = log_tail(stage2_log)

    reason = (
      f"stage2 timed out after {arguments.timeout_seconds}s"
      if arguments.stage2_status == 124
      else f"stage2 produced no test summary (exit {arguments.stage2_status})"
    )

    return {"gate": "G3", "status": STATUS_FAIL, "summary": reason, "details": details}

  if not has_summary(host):
    details["host"]["log_tail"] = log_tail(host_log)

    reason = (
      f"the host timed out after {arguments.timeout_seconds}s"
      if arguments.host_status == 124
      else f"the host produced no test summary (exit {arguments.host_status})"
    )

    return {"gate": "G3", "status": STATUS_FAIL, "summary": reason, "details": details}

  stage2_counts = (stage2["summary"]["total"], stage2["summary"]["passed"], stage2["summary"]["failed"])
  host_counts = (host["summary"]["total"], host["summary"]["passed"], host["summary"]["failed"])

  if stage2_counts != host_counts:
    return {
      "gate": "G3",
      "status": STATUS_FAIL,
      "summary": (
        f"stage2 reported {stage2_counts[1]}/{stage2_counts[0]} passing, "
        f"the host {host_counts[1]}/{host_counts[0]}"
      ),
      "details": details,
    }

  if stage2_failing != host_failing or set(stage2["tests"]) != set(host["tests"]):
    return {
      "gate": "G3",
      "status": STATUS_FAIL,
      "summary": "stage2 and the host disagree on which tests fail",
      "details": details,
    }

  return {
    "gate": "G3",
    "status": STATUS_PASS,
    "summary": f"stage2 matches the host: {stage2_counts[1]}/{stage2_counts[0]} passing",
    "details": details,
  }


def command_gate_g3(arguments: argparse.Namespace) -> int:
  gate = build_gate_g3(arguments)

  output = Path(arguments.output)
  output.parent.mkdir(parents=True, exist_ok=True)
  output.write_text(json.dumps(gate, indent=2) + "\n", encoding="utf-8")

  print(f"[bootstrap] gate G3: {gate['status']} — {gate['summary']}", file=sys.stderr)

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

  for gate_id in GATE_IDS:
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
) -> str:
  lines = [
    "# Selfhost bootstrap promotion report",
    "",
    f"- Commit: `{commit}`",
    f"- Generated: {generated_at}",
    f"- Candidate: **{'yes' if candidate else 'no'}**",
    "",
    "A run is a candidate when every gate passes. Three consecutive candidate",
    "nightly runs promote the stage2 binary to official.",
    "",
    "## Gates",
    "",
    "| gate | status | summary |",
    "| --- | --- | --- |",
  ]

  for gate_id, gate in gates.items():
    title = GATE_TITLES.get(gate_id, gate_id)
    summary = str(gate["summary"]).replace("|", "\\|") or "—"
    lines.append(f"| **{gate_id}** {title} | `{gate['status']}` | {summary} |")

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

  bootstrap_root.mkdir(parents=True, exist_ok=True)

  report_path = bootstrap_root / "report.md"
  report_path.write_text(
    format_report(commit, generated_at, gates, candidate, collect_stage_logs(bootstrap_root)),
    encoding="utf-8",
  )

  promotion_path = bootstrap_root / "promotion.json"
  promotion_path.write_text(
    json.dumps(
      {
        "commit": commit,
        "generated_at": generated_at,
        "candidate": candidate,
        "gates": {
          gate_id: {"status": gate["status"], "summary": gate["summary"]} for gate_id, gate in gates.items()
        },
      },
      indent=2,
    )
    + "\n",
    encoding="utf-8",
  )

  print(f"[bootstrap] report    -> {report_path}", file=sys.stderr)
  print(f"[bootstrap] promotion -> {promotion_path}", file=sys.stderr)
  print(f"[bootstrap] candidate: {'yes' if candidate else 'no'}", file=sys.stderr)

  for gate_id, gate in gates.items():
    print(f"[bootstrap]   {gate_id}: {gate['status']} — {gate['summary']}", file=sys.stderr)

  return 0


def main() -> int:
  parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
  subparsers = parser.add_subparsers(dest="command", required=True)

  gate_g3 = subparsers.add_parser("gate-g3", help="compare a stage2 test run with the host's")
  gate_g3.add_argument("--stage2-log", required=True, help="captured output of the stage2 test run")
  gate_g3.add_argument("--host-log", required=True, help="captured output of the host test run")
  gate_g3.add_argument("--stage2-status", type=int, default=0, help="exit status of the stage2 run")
  gate_g3.add_argument("--host-status", type=int, default=0, help="exit status of the host run")
  gate_g3.add_argument("--timeout-seconds", type=int, default=0, help="timeout both runs were given")
  gate_g3.add_argument("--output", required=True, help="path of the G3 gate result")

  report = subparsers.add_parser("report", help="write report.md and promotion.json")
  report.add_argument("--bootstrap-root", required=True, help="build/bootstrap directory")
  report.add_argument("--project-root", required=True, help="repository root, read for the commit")

  arguments = parser.parse_args()

  if arguments.command == "gate-g3":
    return command_gate_g3(arguments)

  return command_report(arguments)


if __name__ == "__main__":
  sys.exit(main())
