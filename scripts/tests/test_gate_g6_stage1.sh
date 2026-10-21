#!/usr/bin/env bash
# Exercises scripts/bootstrap.sh's gate-g6-stage1 (trim-promotion-gates S4): the
# syntax gate runs against stage1 in PR CI, so it writes its own gate file and
# report/counts/work paths and never touches stage2's G6.json; gate-g6 (stage2)
# keeps the unsuffixed paths every existing reader names. A stub
# scripts/selfhost_syntax_parity.py records the arguments and writes the gate
# file, so this needs no compiler and no corpus.
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(dirname "$SCRIPT_DIR")"
BOOTSTRAP_SH="${REPO_ROOT}/bootstrap.sh"

FAILURES=0
TESTS_RUN=0

pass() { echo "  ok: $1"; }
fail_test() { echo "  FAIL: $1"; FAILURES=$((FAILURES + 1)); }

json_get() { python3 -c 'import json,sys; v=json.load(open(sys.argv[1])).get(sys.argv[2]); print(v if v is not None else "")' "$1" "$2"; }

make_sandbox() {
  local root stage
  root="$(mktemp -d)"
  mkdir -p "$root/scripts" "$root/ignis" "$root/std" "$root/build/bootstrap/gates"
  cp "$BOOTSTRAP_SH" "$root/scripts/bootstrap.sh"
  printf 'function main(): i32 { return 0; }\n' >"$root/ignis/main.ign"

  for stage in stage1 stage2; do
    mkdir -p "$root/build/bootstrap/${stage}"
    printf '#!/usr/bin/env bash\nexit 0\n' >"$root/build/bootstrap/${stage}/ignis"
    chmod +x "$root/build/bootstrap/${stage}/ignis"
  done

  cat >"$root/scripts/selfhost_syntax_parity.py" <<'STUB'
import json, os, pathlib, sys
arguments = sys.argv[1:]
def value(flag): return arguments[arguments.index(flag) + 1]
gate_id = value("--gate-id")
gate_path = pathlib.Path(value("--gate-json"))
gate_path.parent.mkdir(parents=True, exist_ok=True)
gate_path.write_text(json.dumps({"gate": gate_id, "status": "pass", "summary": "stub", "details": {}}) + "\n")
pathlib.Path(value("--report")).write_text(f"# stub report ({gate_id})\n")
pathlib.Path(value("--counts-json")).write_text("{}\n")
label = "stage1" if "stage1" in value("--compiler") else "stage2"
pathlib.Path(os.environ["G6_PROBE_DIR"]).joinpath(f"{label}.argv").write_text("\n".join(arguments) + "\n")
STUB

  echo "$root"
}

run_gate() {
  local root="$1" command="$2"
  ( cd "$root" && IGNIS_BOOTSTRAP_TRUST_STAGES=1 G6_PROBE_DIR="$root" "$root/scripts/bootstrap.sh" "$command" ) >"$root/run.log" 2>&1
}

arg_of() {
  python3 - "$1/$2.argv" "$3" <<'PY'
import sys
lines = [line for line in open(sys.argv[1], encoding="utf-8").read().splitlines() if line]
print(lines[lines.index(sys.argv[2]) + 1] if sys.argv[2] in lines else "")
PY
}

test_gate_g6_stage1_keeps_its_own_files() {
  TESTS_RUN=$((TESTS_RUN + 1))
  echo "test: gate-g6-stage1 writes G6-STAGE1.json without touching G6.json"
  local root before
  root="$(make_sandbox)"
  printf '{"gate": "G6", "status": "pass", "summary": "stage2 sentinel", "details": {}}\n' >"$root/build/bootstrap/gates/G6.json"
  before="$(sha256sum "$root/build/bootstrap/gates/G6.json" | cut -d' ' -f1)"

  run_gate "$root" gate-g6-stage1 || fail_test "gate-g6-stage1 exited non-zero, see ${root}/run.log"

  [[ -f "$root/build/bootstrap/gates/G6-STAGE1.json" ]] && pass "G6-STAGE1.json was written" || fail_test "no G6-STAGE1.json"
  [[ "$(json_get "$root/build/bootstrap/gates/G6-STAGE1.json" gate)" == "G6-STAGE1" ]] && pass "the gate file calls itself G6-STAGE1" || fail_test "G6-STAGE1.json has the wrong gate id"
  [[ "$(sha256sum "$root/build/bootstrap/gates/G6.json" | cut -d' ' -f1)" == "$before" ]] && pass "the stage2 G6.json was left byte-identical" || fail_test "gate-g6-stage1 rewrote G6.json"
  [[ "$(arg_of "$root" stage1 --work-dir)" == "$root/build/bootstrap/parity-syntax-stage1" ]] && pass "its work-dir is parity-syntax-stage1" || fail_test "wrong work-dir"
  [[ "$(arg_of "$root" stage1 --report)" == "$root/build/bootstrap/parity-syntax-stage1.md" ]] && pass "its report is parity-syntax-stage1.md" || fail_test "wrong report"
  [[ "$(arg_of "$root" stage1 --counts-json)" == "$root/build/bootstrap/parity-syntax-stage1.json" ]] && pass "its counts are parity-syntax-stage1.json" || fail_test "wrong counts-json"
  [[ "$(arg_of "$root" stage1 --compiler)" == "$root/build/bootstrap/stage1/ignis" ]] && pass "it runs stage1's binary" || fail_test "wrong compiler"

  rm -rf "$root"
}

test_gate_g6_stage2_keeps_the_unsuffixed_paths() {
  TESTS_RUN=$((TESTS_RUN + 1))
  echo "test: gate-g6 still writes gates/G6.json and parity-syntax.{md,json}"
  local root
  root="$(make_sandbox)"

  run_gate "$root" gate-g6 || fail_test "gate-g6 exited non-zero, see ${root}/run.log"

  [[ -f "$root/build/bootstrap/gates/G6.json" ]] && pass "G6.json was written" || fail_test "no G6.json"
  [[ "$(json_get "$root/build/bootstrap/gates/G6.json" gate)" == "G6" ]] && pass "the gate file calls itself G6" || fail_test "wrong gate id"
  [[ "$(arg_of "$root" stage2 --report)" == "$root/build/bootstrap/parity-syntax.md" ]] && pass "its report is still parity-syntax.md" || fail_test "wrong report"
  [[ "$(arg_of "$root" stage2 --work-dir)" == "$root/build/bootstrap/parity-syntax" ]] && pass "its work-dir is still parity-syntax" || fail_test "wrong work-dir"
  [[ "$(arg_of "$root" stage2 --compiler)" == "$root/build/bootstrap/stage2/ignis" ]] && pass "it runs stage2's binary" || fail_test "wrong compiler"

  rm -rf "$root"
}

test_gate_g6_stage1_keeps_its_own_files
test_gate_g6_stage2_keeps_the_unsuffixed_paths

echo ""
if [[ "$FAILURES" -eq 0 ]]; then echo "${TESTS_RUN} test(s) run, 0 failure(s)"; exit 0; fi
echo "${TESTS_RUN} test(s) run, ${FAILURES} failure(s)"; exit 1
