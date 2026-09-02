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

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
BOOTSTRAP_ROOT="${PROJECT_ROOT}/build/bootstrap"
ENTRY="${PROJECT_ROOT}/ignis/main.ign"
STAGE0="${IGNIS_STAGE0:-ignis}"
GATES_DIR="${BOOTSTRAP_ROOT}/gates"
STAGE1_MEASURE="stage1-measure"
G4_THRESHOLD="1.25"

usage() {
  cat <<EOF
Usage: $(basename "$0") <command>

Commands:
  stage1   Build stage1 with the host compiler (\$IGNIS_STAGE0, default: \`ignis\` on PATH).
  stage2   Build stage2 with stage1 (builds stage1 first when missing).
  stage3   Build stage3 with stage2 and check that its C matches stage2's (fixed point).
  all      stage1, stage2, stage3 in order.
  parity   Run the host e2e corpus through stage2 (builds stage2 first when missing).
  gate-g5  Run the host error corpus through stage2 and write gates/G5.json.
  gate-g4  Compare stage2's resource use with stage1's -> build/bootstrap/gates/G4.json.
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

build_stage3() {
  ensure_stage stage2
  compile_stage stage3 "$(stage_bin stage2)"

  local stage2_c stage3_c
  stage2_c="$(stage_dir stage2)/selfhost_emit.c"
  stage3_c="$(stage_dir stage3)/selfhost_emit.c"

  # stage2's C was emitted by stage1 and stage3's C by stage2. Equal output
  # from two different binaries compiling the same source is the fixed point.
  if cmp -s "$stage2_c" "$stage3_c"; then
    info "stage3: fixed point reached, emitted C is identical ($(md5sum "$stage3_c" | cut -c1-32))"
  else
    fail "stage3: emitted C differs from stage2 (diff ${stage2_c} ${stage3_c})"
  fi
}

run_parity() {
  ensure_stage stage2

  local report="${BOOTSTRAP_ROOT}/parity.md"

  info "parity: running the host e2e corpus through $(stage_bin stage2)"

  # A non-zero exit only means some cases diverge; the report is the product.
  python3 "${SCRIPT_DIR}/selfhost_e2e_parity.py" \
    --compiler "$(stage_bin stage2)" \
    --std "${PROJECT_ROOT}/std" \
    --work-dir "${BOOTSTRAP_ROOT}/parity" \
    --report "$report" || true

  info "parity: report -> ${report}"
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
    gate-g4) run_gate_g4 ;;
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
