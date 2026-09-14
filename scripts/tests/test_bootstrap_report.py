#!/usr/bin/env python3
"""Exercises scripts/bootstrap_report.py's `report` command, and
scripts/selfhost_drop_schedule_parity.py's gate-summary formatting, in
isolation.

G7 (drop-schedule parity) was promoted from informational to a real
promotion gate once the selfhost's ownership analysis reached 623/623
own-code parity with the host (nightly-34803378772). These tests pin the
part of that promotion a shell test can't see as easily as a Python one: a
failing G7 result must flip `candidate` to false in promotion.json exactly
like a failing G5 or G6 would, a passing one must leave `candidate` true
alongside every other passing gate, and the gate summary
`selfhost_drop_schedule_parity.py`'s own `build_gate` writes — not a copy of
its wording retyped into a fixture here — must no longer carry the old
"(informational)" framing for G7 anywhere, while still noting how many
timeouts a run retried.

Each `bootstrap_report.py` test runs the real script as a subprocess against
a throwaway `build/bootstrap`-shaped temp directory, the same black-box
style scripts/tests/test_stage0_fallback.sh uses for scripts/bootstrap.sh, so
nothing here touches the real repository's build/ directory. The G7-specific
tests instead import `build_gate` directly and feed it a tiny synthetic
`DropResult`, so what they assert against is the real formatting code.

Usage: python3 scripts/tests/test_bootstrap_report.py
"""

import json
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

SCRIPT_DIR = Path(__file__).resolve().parent.parent
BOOTSTRAP_REPORT_PY = SCRIPT_DIR / "bootstrap_report.py"
REPO_ROOT = SCRIPT_DIR.parent

sys.path.insert(0, str(SCRIPT_DIR))

# The module under test for the G7-specific fixtures below: real gate-summary
# formatting, not prose retyped into this file, so a wording change to
# build_gate is what these tests see change, not a copy of it.
from selfhost_drop_schedule_parity import (  # noqa: E402
  CLASS_DIFFERS,
  CLASS_PASS,
  CLASS_TIMEOUT,
  DropCase,
  DropResult,
  build_gate,
)

# Every gate promotion.json's `candidate` verdict is computed over. Kept as a
# literal here (rather than imported from bootstrap_report.py) so this test
# fails loudly if G7 is ever quietly dropped back out of the set instead of
# passing by construction.
PROMOTION_GATE_IDS = ("G1", "G2", "G3", "G4", "G5", "G6", "G7")


def real_g7_gate(status: str, retried_timeouts: int = 0) -> dict:
  """The gate `selfhost_drop_schedule_parity.py`'s own `build_gate` would
  write for one synthetic case, so a fixture below asserts against the real
  gate-summary formatting code instead of a copy of its wording."""
  classification = CLASS_PASS if status == "pass" else (CLASS_TIMEOUT if status == "timeout" else CLASS_DIFFERS)
  case = DropCase(name="synthetic", path=Path("synthetic.ign"))
  result = DropResult(case=case, classification=classification)
  counts = {classification: 1}

  gate = build_gate([result], counts, compare_everything=False, retried_timeouts=retried_timeouts)

  assert gate["status"] == ("pass" if status == "pass" else "fail"), gate

  return gate


def write_gate(gates_dir: Path, gate_id: str, status: str, summary: str | None = None) -> None:
  gates_dir.mkdir(parents=True, exist_ok=True)
  payload = {
    "gate": gate_id,
    "status": status,
    "summary": summary if summary is not None else f"{gate_id} {status} (test fixture)",
    "details": {},
  }
  (gates_dir / f"{gate_id}.json").write_text(json.dumps(payload) + "\n", encoding="utf-8")


def run_report(bootstrap_root: Path) -> tuple[dict, str]:
  """Runs `bootstrap_report.py report` and returns (promotion.json, report.md)."""
  completed = subprocess.run(
    [
      sys.executable,
      str(BOOTSTRAP_REPORT_PY),
      "report",
      "--bootstrap-root",
      str(bootstrap_root),
      "--project-root",
      str(REPO_ROOT),
    ],
    capture_output=True,
    text=True,
    check=False,
  )

  assert completed.returncode == 0, (
    f"report exited {completed.returncode}\nstdout: {completed.stdout}\nstderr: {completed.stderr}"
  )

  promotion = json.loads((bootstrap_root / "promotion.json").read_text(encoding="utf-8"))
  report_md = (bootstrap_root / "report.md").read_text(encoding="utf-8")

  return promotion, report_md


class BootstrapReportG7PromotionTests(unittest.TestCase):
  def setUp(self) -> None:
    self._tmp = tempfile.TemporaryDirectory()
    self.addCleanup(self._tmp.cleanup)
    self.bootstrap_root = Path(self._tmp.name) / "bootstrap"
    self.gates_dir = self.bootstrap_root / "gates"

  def all_passing(self) -> None:
    for gate_id in PROMOTION_GATE_IDS:
      if gate_id == "G7":
        write_gate(self.gates_dir, "G7", "pass", summary=real_g7_gate("pass")["summary"])
      else:
        write_gate(self.gates_dir, gate_id, "pass")

  def test_all_gates_passing_including_g7_is_a_candidate(self) -> None:
    self.all_passing()

    promotion, report_md = run_report(self.bootstrap_root)

    self.assertTrue(promotion["candidate"], promotion)
    self.assertEqual(promotion["gates"]["G7"]["status"], "pass")
    self.assertIn("Candidate: **yes**", report_md)

  def test_failing_g7_alone_blocks_candidacy(self) -> None:
    self.all_passing()
    write_gate(self.gates_dir, "G7", "fail", summary=real_g7_gate("differs")["summary"])

    promotion, report_md = run_report(self.bootstrap_root)

    self.assertFalse(promotion["candidate"], promotion)
    self.assertEqual(promotion["gates"]["G7"]["status"], "fail")
    self.assertIn("Candidate: **no**", report_md)

  def test_missing_g7_result_is_skipped_and_blocks_candidacy(self) -> None:
    # seal-gates' escape hatch: a gate a run never produced is "skipped", the
    # same as G5 or G6 would be, not silently dropped from the verdict.
    for gate_id in PROMOTION_GATE_IDS:
      if gate_id != "G7":
        write_gate(self.gates_dir, gate_id, "pass")

    promotion, _report_md = run_report(self.bootstrap_root)

    self.assertFalse(promotion["candidate"], promotion)
    self.assertEqual(promotion["gates"]["G7"]["status"], "skipped")

  def test_g7_gate_summary_no_longer_calls_itself_informational(self) -> None:
    # Runs the real gate-summary formatting code (build_gate), not a copy of
    # its wording — the promotion these tests protect against is exactly a
    # gate whose *status* joined GATE_IDS while its own *summary* still read
    # "informational: does not gate promotion", which a purely static fixture
    # would never catch.
    for status in ("pass", "differs", "timeout"):
      with self.subTest(status=status):
        summary = real_g7_gate(status)["summary"]
        self.assertNotIn("informational", summary.lower())

  def test_g7_gate_summary_notes_retried_timeouts(self) -> None:
    passing = real_g7_gate("pass", retried_timeouts=0)
    retried = real_g7_gate("pass", retried_timeouts=2)

    self.assertNotIn("retried", passing["summary"])
    self.assertIn("2 timeouts retried", retried["summary"])
    self.assertEqual(retried["details"]["timeouts_retried"], 2)

  def test_report_no_longer_calls_g7_informational(self) -> None:
    self.all_passing()

    _promotion, report_md = run_report(self.bootstrap_root)

    self.assertNotIn("informational", report_md.lower())
    # The real gate summary (not a fixture's prose) is what ends up in the
    # rendered report.
    self.assertIn(real_g7_gate("pass")["summary"], report_md)


if __name__ == "__main__":
  unittest.main()
