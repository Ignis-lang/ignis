#!/usr/bin/env python3
"""Exercises scripts/bootstrap_report.py's `report` command in isolation.

G7 (drop-schedule parity) was promoted from informational to a real
promotion gate once the selfhost's ownership analysis reached 623/623
own-code parity with the host (nightly-34803378772). These tests pin the
part of that promotion a shell test can't see as easily as a Python one: a
failing G7 result must flip `candidate` to false in promotion.json exactly
like a failing G5 or G6 would, a passing one must leave `candidate` true
alongside every other passing gate, and report.md must no longer carry the
old "(informational)" framing for G7 anywhere.

Each test runs the real script as a subprocess against a throwaway
`build/bootstrap`-shaped temp directory, the same black-box style
scripts/tests/test_stage0_fallback.sh uses for scripts/bootstrap.sh, so
nothing here touches the real repository's build/ directory.

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

# Every gate promotion.json's `candidate` verdict is computed over. Kept as a
# literal here (rather than imported from bootstrap_report.py) so this test
# fails loudly if G7 is ever quietly dropped back out of the set instead of
# passing by construction.
PROMOTION_GATE_IDS = ("G1", "G2", "G3", "G4", "G5", "G6", "G7")


def write_gate(gates_dir: Path, gate_id: str, status: str) -> None:
  gates_dir.mkdir(parents=True, exist_ok=True)
  payload = {
    "gate": gate_id,
    "status": status,
    "summary": f"{gate_id} {status} (test fixture)",
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
      write_gate(self.gates_dir, gate_id, "pass")

  def test_all_gates_passing_including_g7_is_a_candidate(self) -> None:
    self.all_passing()

    promotion, report_md = run_report(self.bootstrap_root)

    self.assertTrue(promotion["candidate"], promotion)
    self.assertEqual(promotion["gates"]["G7"]["status"], "pass")
    self.assertIn("Candidate: **yes**", report_md)

  def test_failing_g7_alone_blocks_candidacy(self) -> None:
    self.all_passing()
    write_gate(self.gates_dir, "G7", "fail")

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

  def test_report_no_longer_calls_g7_informational(self) -> None:
    self.all_passing()

    _promotion, report_md = run_report(self.bootstrap_root)

    self.assertNotIn("informational", report_md.lower())


if __name__ == "__main__":
  unittest.main()
