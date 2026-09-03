#!/usr/bin/env bash
#
# Bootstrap ladder for the self-hosted Ignis compiler.
#
#   stage0  the host compiler (Rust, `cargo build -p ignis`) — never built here
#   stage1  `ignis/` compiled by stage0          -> build/bootstrap/stage1/ignis
#   stage2  `ignis/` compiled by stage1          -> build/bootstrap/stage2/ignis
#   stage3  `ignis/` compiled by stage2, and its emitted C compared byte for
#           byte with stage2's emitted C         -> build/bootstrap/stage3/ignis
#
# Each stage directory holds the C the previous stage emitted
# (`selfhost_emit.c`), the object file and the linked binary, plus a `log.txt`
# with the compiler's phase report. `stage3` passing is the fixed-point gate:
# the compiler built from stage1's output reproduces that output.
#
# The promotion gates each write build/bootstrap/gates/<gate>.json with
# {"gate", "status", "summary", "details"}:
#
#   G1  fixed point: stage3's emitted C is identical to stage2's
#   G2  e2e parity: the host corpus passes under stage2
#   G3  the selfhost test suite under stage2 matches the host's result
#   G4  resource budget: stage2 within 1.25x of the host
#   G5  diagnostics: stage2's messages equal or better than the host's
#   G6  syntax: stage2 accepts and rejects exactly what the host parser does
#
# `gates` runs all of them and then `report`, which turns the gate files into
# build/bootstrap/report.md and build/bootstrap/promotion.json.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
BOOTSTRAP_ROOT="${PROJECT_ROOT}/build/bootstrap"
GATES_DIR="${BOOTSTRAP_ROOT}/gates"
ENTRY="${PROJECT_ROOT}/ignis/main.ign"
STAGE0="${IGNIS_STAGE0:-ignis}"
STAGE1_MEASURE="stage1-measure"
G4_THRESHOLD="1.25"
SELF="${SCRIPT_DIR}/$(basename "${BASH_SOURCE[0]}")"

# Gate identifiers in report order. G4, G5 and G6 have their own subcommands;
# when those are absent `gates` still records a result for them.
GATE_IDS=(G1 G2 G3 G4 G5 G6)

# The selfhost test suite runs a full analysis of `ignis/` before it links, and
# a hung run still has to leave a gate result behind.
GATE_G3_TIMEOUT_SECONDS=10800

usage() {
  cat <<EOF
Usage: $(basename "$0") <command>

Commands:
  stage1   Build stage1 with the host compiler (\$IGNIS_STAGE0, default: \`ignis\` on PATH).
  stage2   Build stage2 with stage1 (builds stage1 first when missing).
  stage3   Build stage3 with stage2 and check that its C matches stage2's (fixed point, G1).
  all      stage1, stage2, stage3 in order.
  parity   Run the host e2e corpus through stage2 (builds stage2 first when missing, G2).
  gate-g3  Run the selfhost test suite under stage2 and under the host and compare them (G3).
  gate-g5  Run the host error corpus through stage2 and write gates/G5.json.
  gate-g6  Compare stage2's parse verdicts with the host's and write gates/G6.json.
  gate-g4  Compare stage2's resource use with stage1's -> build/bootstrap/gates/G4.json.
  gates    Run every stage and gate in order, then write the promotion report.
  report   Turn build/bootstrap/gates/*.json into report.md and promotion.json.
  status   Show which stage artifacts exist.
  clean    Remove build/bootstrap.

Every stage compiles ${ENTRY#"$PROJECT_ROOT/"} and writes its artifacts under
build/bootstrap/<stage>/. A self-compilation takes several minutes per stage.
EOF
}

info() { echo "[bootstrap] $*" >&2; }
fail() { echo "[bootstrap] error: $*" >&2; exit 1; }

stage_dir() { echo "${BOOTSTRAP_ROOT}/$1"; }
stage_bin() { echo "$(stage_dir "$1")/ignis"; }

# Build a flat JSON object from alternating key and value arguments.
json_object() {
  python3 -c '
import json
import sys

print(json.dumps(dict(zip(sys.argv[1::2], sys.argv[2::2]))))
' "$@"
}

file_md5() {
  if [[ -f "$1" ]]; then
    md5sum "$1" | cut -d' ' -f1
  else
    echo ""
  fi
}

# Write a gate result to build/bootstrap/gates/<gate>.json.
#
#   $1  gate id (G1..G5)
#   $2  status: pass, fail or skipped
#   $3  one-line summary
#   $4  details as a JSON object (optional)
write_gate() {
  local gate="$1"
  local status="$2"
  local summary="$3"
  local details="${4-}"

  [[ -n "$details" ]] || details='{}'

  mkdir -p "$GATES_DIR"

  GATE_ID="$gate" \
  GATE_STATUS="$status" \
  GATE_SUMMARY="$summary" \
  GATE_DETAILS="$details" \
    python3 -c '
import json
import os
import sys

raw = os.environ["GATE_DETAILS"]

try:
  details = json.loads(raw)
except json.JSONDecodeError:
  details = {"raw": raw}

payload = {
  "gate": os.environ["GATE_ID"],
  "status": os.environ["GATE_STATUS"],
  "summary": os.environ["GATE_SUMMARY"],
  "details": details,
}

with open(sys.argv[1], "w", encoding="utf-8") as handle:
  handle.write(json.dumps(payload, indent=2) + "\n")
' "${GATES_DIR}/${gate}.json"

  info "gate ${gate}: ${status} — ${summary}"
}

# Compile the selfhost entry with a given compiler binary into a stage directory.
#
#   $1  stage name (output directory under build/bootstrap)
#   $2  compiler binary to run
compile_stage() {
  local stage="$1"
  local compiler="$2"
  local dir
  dir="$(stage_dir "$stage")"

  rm -rf "$dir"
  mkdir -p "$dir"

  info "${stage}: compiling ${ENTRY#"$PROJECT_ROOT/"} with ${compiler}"

  # The selfhost driver writes `selfhost_emit.c` and `selfhost_emit.o` into the
  # working directory, so each stage runs inside its own directory. The
  # measurement wrapper only observes the run; it does not touch the emitted C.
  if ! python3 "${SCRIPT_DIR}/measure_run.py" \
    --cwd "$dir" \
    --out "$dir/measure.json" \
    --label "$stage" \
    -- "$compiler" "$ENTRY" -o "$dir/ignis" 2>&1 | tee "$dir/log.txt"; then
    fail "${stage}: the compiler reported errors, see ${dir}/log.txt"
  fi

  [[ -x "$dir/ignis" ]] || fail "${stage}: no binary produced, see ${dir}/log.txt"

  info "${stage}: ok -> ${dir}/ignis"
}

build_stage1() {
  local stage0_bin
  stage0_bin="$(command -v "$STAGE0" || true)"
  [[ -n "$stage0_bin" ]] || fail "stage0 compiler not found: ${STAGE0} (set IGNIS_STAGE0)"

  local dir
  dir="$(stage_dir stage1)"
  rm -rf "$dir"
  mkdir -p "$dir"

  info "stage1: building with ${stage0_bin}"

  # The host compiler reads ignis.toml and writes build/selfhost/bin/ignis; the
  # stage directory keeps a copy so later stages never depend on that path.
  if ! (cd "$PROJECT_ROOT" && "$stage0_bin" build) 2>&1 | tee "$dir/log.txt"; then
    fail "stage1: the host compiler reported errors, see ${dir}/log.txt"
  fi

  local host_out="${PROJECT_ROOT}/build/selfhost/bin/ignis"
  [[ -x "$host_out" ]] || fail "stage1: ${host_out} was not produced"

  cp "$host_out" "$dir/ignis"
  info "stage1: ok -> ${dir}/ignis"
}

ensure_stage() {
  local stage="$1"
  [[ -x "$(stage_bin "$stage")" ]] || "build_${stage}"
}

build_stage2() {
  ensure_stage stage1
  compile_stage stage2 "$(stage_bin stage1)"
}

# stage1 is a copy of the host build, so it is never measured while it is
# produced. The G4 baseline is stage1 compiling the same corpus every other
# stage compiles, in its own directory so it cannot disturb the ladder.
build_stage1_measure() {
  ensure_stage stage1
  compile_stage "$STAGE1_MEASURE" "$(stage_bin stage1)"
}

gate_g1_details() {
  STAGE2_C="$1" \
  STAGE3_C="$2" \
  STAGE2_MD5="$(file_md5 "$1")" \
  STAGE3_MD5="$(file_md5 "$2")" \
    python3 -c '
import json
import os

print(json.dumps({
  "stage2_c": os.environ["STAGE2_C"],
  "stage3_c": os.environ["STAGE3_C"],
  "stage2_md5": os.environ["STAGE2_MD5"] or None,
  "stage3_md5": os.environ["STAGE3_MD5"] or None,
}))
'
}

build_stage3() {
  ensure_stage stage2

  local stage2_c stage3_c stage2_md5 stage3_md5
  stage2_c="$(stage_dir stage2)/selfhost_emit.c"
  stage3_c="$(stage_dir stage3)/selfhost_emit.c"

  # A compilation error is a failed fixed-point gate rather than a missing one,
  # so G1 is written before the failure is propagated.
  if ! (compile_stage stage3 "$(stage_bin stage2)"); then
    write_gate G1 fail "stage3 did not compile" "$(gate_g1_details "$stage2_c" "$stage3_c")"
    fail "stage3: the compiler reported errors, see $(stage_dir stage3)/log.txt"
  fi

  stage2_md5="$(file_md5 "$stage2_c")"
  stage3_md5="$(file_md5 "$stage3_c")"

  # stage2's C was emitted by stage1 and stage3's C by stage2. Equal output
  # from two different binaries compiling the same source is the fixed point.
  if [[ -n "$stage2_md5" && "$stage2_md5" == "$stage3_md5" ]]; then
    write_gate G1 pass "stage3 C is identical to stage2 (${stage3_md5})" \
      "$(gate_g1_details "$stage2_c" "$stage3_c")"
    info "stage3: fixed point reached, emitted C is identical (${stage3_md5})"
  else
    write_gate G1 fail "stage3 C differs from stage2" "$(gate_g1_details "$stage2_c" "$stage3_c")"
    fail "stage3: emitted C differs from stage2 (diff ${stage2_c} ${stage3_c})"
  fi
}

run_parity() {
  ensure_stage stage2

  local report="${BOOTSTRAP_ROOT}/parity.md"

  info "parity: running the host e2e corpus through $(stage_bin stage2)"

  mkdir -p "$GATES_DIR"

  # A non-zero exit only means some cases diverge; the report is the product.
  python3 "${SCRIPT_DIR}/selfhost_e2e_parity.py" \
    --compiler "$(stage_bin stage2)" \
    --std "${PROJECT_ROOT}/std" \
    --work-dir "${BOOTSTRAP_ROOT}/parity" \
    --report "$report" \
    --gate-json "${GATES_DIR}/G2.json" || true

  info "parity: report -> ${report}"

  if [[ ! -f "${GATES_DIR}/G2.json" ]]; then
    write_gate G2 fail "the parity run produced no gate result" \
      "$(json_object report "$report")"
  fi
}

# G3: the selfhost test suite has to report the same result under stage2 as it
# does under the host compiler. Both runs write their output next to each other
# and only the test lines and the summary block are compared, so the timings and
# the phase reports around them do not matter.
run_gate_g3() {
  ensure_stage stage2

  local dir="${BOOTSTRAP_ROOT}/stage2-tests"
  local stage2_log="${dir}/log.txt"
  local host_log="${dir}/log-host.txt"
  local host_bin
  local stage2_status=0
  local host_status=0

  mkdir -p "$dir" "$GATES_DIR"

  host_bin="$(command -v "$STAGE0" || true)"

  if [[ -z "$host_bin" ]]; then
    write_gate G3 fail "host compiler not found: ${STAGE0}" \
      "$(json_object stage2_log "$stage2_log" host_log "$host_log")"
    return 0
  fi

  info "gate-g3: running the selfhost test suite under stage2"

  # The suite reads its fixtures relative to the working directory, so both
  # runs start from the project root. In test mode every artifact the selfhost
  # driver writes derives from `-o`, which has to name a file inside `dir`.
  (cd "$PROJECT_ROOT" && timeout "$GATE_G3_TIMEOUT_SECONDS" \
    env IGNIS_STD_PATH="${PROJECT_ROOT}/std" \
    "$(stage_bin stage2)" test "$ENTRY" -o "${dir}/ignis-tests") >"$stage2_log" 2>&1 || stage2_status=$?

  info "gate-g3: running the selfhost test suite under ${host_bin}"

  # The host runs the suite in project mode. Its single-file mode reads no
  # ignis.toml, so the `@compiler` alias the selfhost sources import through
  # would not resolve and the run would end before any test.
  (cd "$PROJECT_ROOT" && timeout "$GATE_G3_TIMEOUT_SECONDS" "$host_bin" test) \
    >"$host_log" 2>&1 || host_status=$?

  python3 "${SCRIPT_DIR}/bootstrap_report.py" gate-g3 \
    --stage2-log "$stage2_log" \
    --host-log "$host_log" \
    --stage2-status "$stage2_status" \
    --host-status "$host_status" \
    --timeout-seconds "$GATE_G3_TIMEOUT_SECONDS" \
    --output "${GATES_DIR}/G3.json"

  info "gate-g3: result -> ${GATES_DIR}/G3.json"
}

run_report() {
  mkdir -p "$GATES_DIR"

  python3 "${SCRIPT_DIR}/bootstrap_report.py" report \
    --bootstrap-root "$BOOTSTRAP_ROOT" \
    --project-root "$PROJECT_ROOT"
}

# Run every stage and gate, then the report. A failing step never stops the run:
# the report is the product and a missing gate result is recorded as skipped.
run_gates() {
  rm -rf "$GATES_DIR"
  mkdir -p "$GATES_DIR"

  local step
  for step in stage1 stage2 parity stage3 gate-g4 gate-g5 gate-g6 gate-g3; do
    info "gates: ${step}"
    "$SELF" "$step" || info "gates: ${step} exited non-zero, continuing"
  done

  local gate
  for gate in "${GATE_IDS[@]}"; do
    [[ -f "${GATES_DIR}/${gate}.json" ]] ||
      write_gate "$gate" skipped "no ${gate} result was produced by this run"
  done

  run_report
}

# G5: the selfhost's diagnostics must be equal or better than the host's over
# the error corpus, so every diagnostic the host records has to appear.
run_gate_g5() {
  ensure_stage stage2

  local gates_dir="${BOOTSTRAP_ROOT}/gates"
  local report="${BOOTSTRAP_ROOT}/parity-err.md"
  local counts="${BOOTSTRAP_ROOT}/parity-err.json"
  local gate_file="${gates_dir}/G5.json"

  mkdir -p "$gates_dir"
  rm -f "$counts"

  info "gate-g5: replaying the host error corpus through $(stage_bin stage2)"

  local status="pass"

  if ! python3 "${SCRIPT_DIR}/selfhost_e2e_parity.py" \
    --compiler "$(stage_bin stage2)" \
    --corpus err \
    --std "${PROJECT_ROOT}/std" \
    --work-dir "${BOOTSTRAP_ROOT}/parity-err" \
    --counts-json "$counts" \
    --report "$report"; then
    status="fail"
  fi

  [[ -f "$counts" ]] || fail "gate-g5: the harness wrote no counts, see ${report}"

  python3 - "$counts" "$gate_file" "$status" <<'PYTHON'
import json
import sys

counts_path, gate_path, status = sys.argv[1:4]

with open(counts_path, encoding="utf-8") as handle:
  data = json.load(handle)

counts = data["counts"]
gate = {
  "gate": "G5",
  "status": status,
  "summary": "{}/{} error-corpus cases keep every diagnostic the host records".format(
    counts.get("pass", 0), data["total"]
  ),
  "details": {
    "corpus": "err",
    "total": data["total"],
    "counts": counts,
    "failing": data["failing"],
  },
}

with open(gate_path, "w", encoding="utf-8") as handle:
  handle.write(json.dumps(gate, indent=2) + "\n")
PYTHON

  info "gate-g5: ${status} -> ${gate_file} (report ${report})"
}

# G6: every source the host parser accepts must parse under stage2, and every
# source it rejects must be rejected there too.
run_gate_g6() {
  ensure_stage stage2

  local report="${BOOTSTRAP_ROOT}/parity-syntax.md"
  local counts="${BOOTSTRAP_ROOT}/parity-syntax.json"
  local gate_file="${GATES_DIR}/G6.json"

  mkdir -p "$GATES_DIR"
  rm -f "$gate_file"

  info "gate-g6: comparing the parse verdicts of $(stage_bin stage2) with ${STAGE0}'s"

  # A non-zero exit only means some cases diverge; the gate file is the product.
  python3 "${SCRIPT_DIR}/selfhost_syntax_parity.py" \
    --compiler "$(stage_bin stage2)" \
    --host "$STAGE0" \
    --std "${PROJECT_ROOT}/std" \
    --work-dir "${BOOTSTRAP_ROOT}/parity-syntax" \
    --counts-json "$counts" \
    --report "$report" \
    --gate-json "$gate_file" || true

  if [[ ! -f "$gate_file" ]]; then
    write_gate G6 fail "the syntax parity run produced no gate result" \
      "$(json_object report "$report")"
    return 0
  fi

  info "gate-g6: result -> ${gate_file} (report ${report})"
}

# G4: the selfhost-built compiler (stage2) compiling the selfhost corpus must
# stay within G4_THRESHOLD of the host-built compiler (stage1) in peak RSS and
# wall time.
run_gate_g4() {
  local baseline candidate
  baseline="$(stage_dir "$STAGE1_MEASURE")/measure.json"
  candidate="$(stage_dir stage2)/measure.json"

  [[ -f "$candidate" ]] || build_stage2
  [[ -f "$baseline" ]] || build_stage1_measure

  mkdir -p "$GATES_DIR"

  python3 - "$baseline" "$candidate" "${GATES_DIR}/G4.json" "$G4_THRESHOLD" <<'PY'
import json
import sys

baseline_path, candidate_path, out_path, threshold_text = sys.argv[1:5]
threshold = float(threshold_text)

with open(baseline_path, encoding="utf-8") as handle:
  baseline = json.load(handle)
with open(candidate_path, encoding="utf-8") as handle:
  candidate = json.load(handle)

rss_ratio = candidate["rss_kb"] / baseline["rss_kb"]
wall_ratio = candidate["wall_s"] / baseline["wall_s"]

within_budget = rss_ratio <= threshold and wall_ratio <= threshold
status = "pass" if within_budget else "fail"

summary = (
  f"stage2 vs stage1: rss {rss_ratio:.2f}x, wall {wall_ratio:.2f}x "
  f"(threshold {threshold:.2f}x)"
)

report = {
  "gate": "G4",
  "status": status,
  "summary": summary,
  "details": {
    "threshold": threshold,
    "baseline": {
      "stage": "stage1-measure",
      "rss_kb": baseline["rss_kb"],
      "wall_s": baseline["wall_s"],
    },
    "candidate": {
      "stage": "stage2",
      "rss_kb": candidate["rss_kb"],
      "wall_s": candidate["wall_s"],
    },
    "rss_ratio": round(rss_ratio, 4),
    "wall_ratio": round(wall_ratio, 4),
  },
}

with open(out_path, "w", encoding="utf-8") as handle:
  json.dump(report, handle, indent=2)
  handle.write("\n")

print(f"G4 {status}: {summary}")
sys.exit(0 if within_budget else 1)
PY
}

show_status() {
  local stage
  for stage in stage1 stage2 stage3; do
    if [[ -x "$(stage_bin "$stage")" ]]; then
      echo "${stage}: $(stage_bin "$stage")"
    else
      echo "${stage}: missing"
    fi
  done
}

main() {
  local command="${1:-}"

  case "$command" in
    stage1) build_stage1 ;;
    stage2) build_stage2 ;;
    stage3) build_stage3 ;;
    all)
      build_stage1
      build_stage2
      build_stage3
      ;;
    parity) run_parity ;;
    gate-g5) run_gate_g5 ;;
    gate-g6) run_gate_g6 ;;
    gate-g4) run_gate_g4 ;;
    gate-g3) run_gate_g3 ;;
    gates) run_gates ;;
    report) run_report ;;
    status) show_status ;;
    clean) rm -rf "$BOOTSTRAP_ROOT"; info "removed ${BOOTSTRAP_ROOT}" ;;
    -h|--help|help|"") usage ;;
    *)
      usage
      fail "unknown command: ${command}"
      ;;
  esac
}

main "$@"
