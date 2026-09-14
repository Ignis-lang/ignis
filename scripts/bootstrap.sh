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
#   G7  drop schedules: stage2's --dump-drop-schedule matches the host's
#
# `gates` runs all of them and then `report`, which turns the gate files into
# build/bootstrap/report.md and build/bootstrap/promotion.json. The nightly
# splits that same work across runners instead: `stages` on one, each gate
# subcommand on its own — G3's two test runs on a runner each — then
# `gate-g3-compare`, `seal-gates` and `report` over the collected gate files.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
BOOTSTRAP_ROOT="${PROJECT_ROOT}/build/bootstrap"
GATES_DIR="${BOOTSTRAP_ROOT}/gates"
ENTRY="${PROJECT_ROOT}/ignis/main.ign"
STAGE0="${IGNIS_STAGE0:-ignis}"
# Host compiler used when stage1 falls back from a failing official/selfhost
# stage0 (see build_stage1). The same binary `stage0=host` resolves to: the
# nightly puts target/ci/ignis on PATH before either runs, so plain `ignis`
# is right there too; a developer can point it elsewhere with this variable.
HOST_STAGE0_FALLBACK="${IGNIS_STAGE0_HOST_FALLBACK:-ignis}"
STAGE1_MEASURE="stage1-measure"
G4_THRESHOLD="1.25"
SELF="${SCRIPT_DIR}/$(basename "${BASH_SOURCE[0]}")"

# Gate identifiers in report order. G4, G5, G6 and G7 have their own
# subcommands; when those are absent `gates` still records a result for them,
# and `seal-gates` records a skipped placeholder for any of them a run never
# produced. scripts/bootstrap_report.py's `candidate` verdict is `all` over
# this same set.
GATE_IDS=(G1 G2 G3 G4 G5 G6 G7)

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
  stages   stage1, stage2, stage3, where only a stage3 failure is left to G1.
  parity   Run the host e2e corpus through stage2 (builds stage2 first when missing, G2).
  gate-g3  Run the selfhost test suite under stage2 and under the host and compare them (G3).
  gate-g3-stage2   Run only the stage2 half of G3 and keep its log and exit status.
  gate-g3-host     Run only the host half of G3 and keep its log and exit status.
  gate-g3-compare  Compare the two G3 logs and write gates/G3.json.
  gate-g3-stage1   G3 run against stage1 instead of stage2 -> gates/G3-STAGE1.json
                   (ci.yml's PR-only check; the promotion ladder still covers
                   stage2). Only runs the stage1 half and the compare: pass
                   the host half's exit status and log to gate-g3-record-host
                   instead of re-running the host suite. A later local
                   \`report\` lists these as unscored G3-STAGE1/G5-STAGE1 rows;
                   they never affect the \`candidate\` verdict.
  gate-g3-record-host <stage> <exit-status> <log-path>
                   Record a host run captured elsewhere (e.g. ci.yml's own
                   "Run the selfhost test suite" step) as the host half of
                   G3 for <stage>, instead of running the host suite again.
  gate-g5  Run the host error corpus through stage2 and write gates/G5.json.
  gate-g5-stage1   G5 run against stage1 instead of stage2 -> gates/G5-STAGE1.json
                   (ci.yml's PR-only check; the promotion ladder still covers
                   stage2). Same unscored-row note as gate-g3-stage1 above.
  gate-g6  Compare stage2's parse verdicts with the host's and write gates/G6.json.
  gate-g4  Compare stage2's resource use with stage1's -> build/bootstrap/gates/G4.json.
  gate-g7  Diff stage2's drop schedules against the host's -> gates/G7.json.
  gates    Run every stage and gate in order, then write the promotion report.
  seal-gates  Record a skipped result for every gate that produced no file.
  report   Turn build/bootstrap/gates/*.json into report.md and promotion.json.
  promotion-decide <candidate> <fallback> [previous-streak]
           Print the nightly's publish-step decision as JSON: whether to
           write promotion-streak.json, publish stage2, and re-seed the
           official lineage after a fallback candidate run.
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
#   $3  "soft" (optional) — return 1 instead of exiting on failure, so a
#       caller can decide what to do next (see build_stage1's fallback).
compile_stage() {
  local stage="$1"
  local compiler="$2"
  local soft="${3-}"
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
    [[ "$soft" == "soft" ]] && return 1
    fail "${stage}: the compiler reported errors, see ${dir}/log.txt"
  fi

  if [[ ! -x "$dir/ignis" ]]; then
    [[ "$soft" == "soft" ]] && return 1
    fail "${stage}: no binary produced, see ${dir}/log.txt"
  fi

  info "${stage}: ok -> ${dir}/ignis"
}

# The most specific error line in a stage log, used to summarize a stage0
# failure in one line for the fallback log message and
# build/bootstrap/stage0.json, without dumping the whole log there. Preferred
# in order: Ignis's own `Error[<code>]:` diagnostics, a linker/gcc `error:`
# line, any case-insensitive "error", then the first non-blank line (banner
# text from the compiler's phase report, as a last resort). ANSI color codes
# are stripped first since the compiler colors its own "Error" banner.
first_error_line() {
  local log="$1"
  local line=""
  local stripped=""

  if [[ -f "$log" ]]; then
    stripped="$(sed -E 's/\x1b\[[0-9;]*m//g' "$log")"
    line="$(grep -m1 -E 'Error\[' <<<"$stripped" || true)"
    [[ -n "$line" ]] || line="$(grep -m1 -E 'error:' <<<"$stripped" || true)"
    [[ -n "$line" ]] || line="$(grep -m1 -i 'error' <<<"$stripped" || true)"
    [[ -n "$line" ]] || line="$(grep -m1 -E '[^[:space:]]' <<<"$stripped" || true)"
    line="$(sed -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//' <<<"$line")"
  fi

  [[ -n "$line" ]] && echo "$line" || echo "see ${log}"
}

# stage0 is either the Rust host or a previously promoted selfhost binary
# (`IGNIS_STAGE0` pointed at the nightly's official asset). The two take
# different command forms: the host reads ignis.toml through `ignis build`,
# while a selfhost binary is invoked directly like every other stage, on
# `ignis/main.ign`. The nightly records which one it resolved in
# `stage0.json`; a developer can say so with `IGNIS_STAGE0_KIND=selfhost|host`.
# Only when neither is present does `--version` decide: the host's clap CLI
# exits 0, while the selfhost CLI rejects the flag today. That probe stops
# discriminating once the selfhost learns `--version`, which is why it is the
# last resort and not the rule.
stage0_is_selfhost() {
  local bin="$1"
  local recorded_kind=""

  if [[ -f "${BOOTSTRAP_ROOT}/stage0.json" ]]; then
    recorded_kind="$(python3 -c 'import json,sys; print(json.load(open(sys.argv[1])).get("kind", ""))' "${BOOTSTRAP_ROOT}/stage0.json" 2>/dev/null || true)"
  fi

  case "${IGNIS_STAGE0_KIND:-$recorded_kind}" in
    selfhost|official) return 0 ;;
    host) return 1 ;;
  esac

  "$bin" --version >/dev/null 2>&1 && return 1
  return 0
}

# Whether stage0 was explicitly forced to `official` (`workflow_dispatch
# stage0=official`, recorded as `"mode": "official"` in stage0.json, or a
# developer's `IGNIS_STAGE0_MODE=official`) rather than resolved there by
# `auto`. An explicit request exists to let a maintainer force and inspect
# the official path, so it must fail outright rather than silently falling
# back — only `auto` falls back to the host.
stage0_explicit_official() {
  local recorded_mode=""

  if [[ -f "${BOOTSTRAP_ROOT}/stage0.json" ]]; then
    recorded_mode="$(python3 -c 'import json,sys; print(json.load(open(sys.argv[1])).get("mode", ""))' "${BOOTSTRAP_ROOT}/stage0.json" 2>/dev/null || true)"
  fi

  [[ "${IGNIS_STAGE0_MODE:-$recorded_mode}" == "official" ]]
}

# Record that stage1 fell back from the official/selfhost stage0 to the host
# compiler, so `bootstrap_report.py report` can surface it in report.md and
# promotion.json (`stage0.fallback: true`).
#
# `kind` (and `mode`) are left exactly as they were: `stage0_is_selfhost` and
# `stage0_explicit_official` read them on every later `build_stage1` call
# (e.g. a second `stage1` invocation, or `ensure_stage stage1` from `stage2`
# when stage1 is missing), and overwriting `kind` to `host` here would make
# that oracle believe stage0 itself changed kind, so a later call would try
# to run the still-official stage0 binary in the host's `<bin> build` form
# instead of retrying the fallback. What was actually used to build stage1
# this run is recorded separately as `used_kind`.
write_stage0_fallback() {
  local host_bin="$1"
  local reason="$2"
  local existing='{"kind": "official", "source": "", "sha256": ""}'

  mkdir -p "$BOOTSTRAP_ROOT"

  if [[ -f "${BOOTSTRAP_ROOT}/stage0.json" ]]; then
    existing="$(cat "${BOOTSTRAP_ROOT}/stage0.json")"
  fi

  # Written to a temp file and renamed into place: the nightly's supersede
  # watcher can SIGTERM/SIGKILL this process group mid-run, and a half-written
  # stage0.json would otherwise corrupt every later read of it (including the
  # very next `stage0_is_selfhost` probe in this same run).
  EXISTING_JSON="$existing" HOST_BIN="$host_bin" REASON="$reason" python3 -c '
import json
import os
import sys
import tempfile

path = sys.argv[1]

try:
  payload = json.loads(os.environ["EXISTING_JSON"])
except json.JSONDecodeError:
  payload = {"kind": "official", "source": "", "sha256": ""}

payload["fallback"] = True
payload["fallback_reason"] = os.environ["REASON"]
payload["original_kind"] = payload.get("kind", "official")
payload["used_kind"] = "host"
payload["used_source"] = os.environ["HOST_BIN"]

directory = os.path.dirname(path) or "."
fd, temp_path = tempfile.mkstemp(prefix=".stage0.", suffix=".json.tmp", dir=directory)
try:
  with os.fdopen(fd, "w", encoding="utf-8") as handle:
    handle.write(json.dumps(payload, indent=2) + "\n")
  os.replace(temp_path, path)
except BaseException:
  os.unlink(temp_path)
  raise
' "${BOOTSTRAP_ROOT}/stage0.json" || fail "stage1: could not record the stage0 fallback in ${BOOTSTRAP_ROOT}/stage0.json"

  info "stage0: recorded the fallback in ${BOOTSTRAP_ROOT}/stage0.json"
}

# Build stage1 by driving a host-shaped compiler through `ignis build` against
# ignis.toml. Used both for the direct host path (stage0 is the host) and for
# the official-stage0 fallback below (stage0 is a selfhost binary that cannot
# build stage1, so this rebuilds it with the same binary `stage0=host` uses).
#
#   $1  compiler binary to run
#   $2  fallback reason (optional) — when set, stage0.json is rewritten to
#       record the fallback so the report shows it.
build_stage1_with_host() {
  local host_bin="$1"
  local fallback_reason="${2-}"
  local dir
  dir="$(stage_dir stage1)"

  # A fallback rebuilds over the official/selfhost stage0's failed attempt.
  # Its log is the only record of why the fallback happened, so it is copied
  # aside before the directory is wiped rather than lost with it.
  local preserved_official_log=""
  if [[ -n "$fallback_reason" && -f "$dir/log.txt" ]]; then
    preserved_official_log="$(mktemp)"
    cp "$dir/log.txt" "$preserved_official_log"
  fi

  rm -rf "$dir"
  mkdir -p "$dir"

  if [[ -n "$preserved_official_log" ]]; then
    mv "$preserved_official_log" "$dir/log-stage0-official.txt"
  fi

  info "stage1: building with ${host_bin}"

  # The host compiler reads ignis.toml and writes build/selfhost/bin/ignis; the
  # stage directory keeps a copy so later stages never depend on that path.
  if ! (cd "$PROJECT_ROOT" && "$host_bin" build) 2>&1 | tee "$dir/log.txt"; then
    fail "stage1: the host compiler reported errors, see ${dir}/log.txt"
  fi

  local host_out="${PROJECT_ROOT}/build/selfhost/bin/ignis"
  [[ -x "$host_out" ]] || fail "stage1: ${host_out} was not produced"

  cp "$host_out" "$dir/ignis"
  info "stage1: ok -> ${dir}/ignis"

  [[ -z "$fallback_reason" ]] || write_stage0_fallback "$host_bin" "$fallback_reason"
}

build_stage1() {
  local stage0_bin
  stage0_bin="$(command -v "$STAGE0" || true)"
  [[ -n "$stage0_bin" ]] || fail "stage0 compiler not found: ${STAGE0} (set IGNIS_STAGE0)"

  if stage0_is_selfhost "$stage0_bin"; then
    info "stage1: stage0 (${stage0_bin}) is a selfhost compiler, compiling ${ENTRY#"$PROJECT_ROOT/"} directly"

    if IGNIS_STD_PATH="${PROJECT_ROOT}/std" compile_stage stage1 "$stage0_bin" soft; then
      return
    fi

    local first_error
    first_error="$(first_error_line "$(stage_dir stage1)/log.txt")"

    if stage0_explicit_official; then
      fail "stage1: the official stage0 compiler reported errors (${first_error}), see $(stage_dir stage1)/log.txt"
    fi

    # A published selfhost binary can go stale against a link or runtime
    # contract change on main (e.g. a std runtime archive it still passes to
    # `ld` gets removed) well before its promotion streak resets. Falling
    # back here keeps the ladder green on the host build CI already proved,
    # instead of failing the whole nightly on a stage0 that is simply behind.
    info "stage0: official binary cannot build stage1 (${first_error}), falling back to the host"

    local host_bin
    host_bin="$(command -v "$HOST_STAGE0_FALLBACK" || true)"
    [[ -n "$host_bin" ]] ||
      fail "stage1: official stage0 failed (${first_error}) and the host fallback (${HOST_STAGE0_FALLBACK}) was not found; set IGNIS_STAGE0_HOST_FALLBACK"

    build_stage1_with_host "$host_bin" "official binary cannot build stage1 (${first_error})"
    return
  fi

  build_stage1_with_host "$stage0_bin"
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

# G3: the selfhost test suite has to report the same result under a built
# stage (stage2 for the promotion gate and the nightly matrix; stage1 for
# ci.yml's PR-only `gate-g3-stage1`, added to close the gap PR #201 left where
# CI never ran this suite under a selfhost-built binary at all) as it does
# under the host compiler. Both runs write their output next to each other and
# only the test lines and the summary block are compared, so the timings and
# the phase reports around them do not matter.
#
# The two runs are separate subcommands because each takes the better part of a
# quarter of an hour and nothing connects them until the comparison: the nightly
# gives each one its own runner and compares the collected logs afterwards.
# `gate-g3` still runs the three steps in order for a developer machine.
#
# The functions below take the built stage's name as their first argument
# ("stage2", "stage1"), keyed to its own artifact directory
# (build/bootstrap/<stage>-tests/) so two stages' runs never collide, and the
# gate id to write ("G3" for stage2, "G3-STAGE1" for stage1) so their gate
# files never collide either.
gate_g3_dir() { echo "${BOOTSTRAP_ROOT}/$1-tests"; }

gate_g3_log() {
  case "$2" in
    host) echo "$(gate_g3_dir "$1")/log-host.txt" ;;
    *) echo "$(gate_g3_dir "$1")/log.txt" ;;
  esac
}

gate_g3_status_file() { echo "$(gate_g3_dir "$1")/status-$2.json"; }

# The exit status of a run and the budget it was given, kept next to its log so
# the comparison can read both back on another machine.
#
#   $1  stage ("stage2", "stage1")
#   $2  run ("$1" for the built-stage half, or "host")
#   $3  exit status
write_gate_g3_status() {
  json_object \
    run "$2" \
    exit_status "$3" \
    timeout_seconds "$GATE_G3_TIMEOUT_SECONDS" >"$(gate_g3_status_file "$1" "$2")"
}

# Print the exit status on the first line and the timeout budget on the second.
read_gate_g3_status() {
  python3 -c '
import json
import sys

try:
  with open(sys.argv[1], encoding="utf-8") as handle:
    data = json.load(handle)
except (OSError, ValueError):
  sys.exit(1)

print(data.get("exit_status", ""))
print(data.get("timeout_seconds", ""))
' "$1"
}

# Run the selfhost test suite under a built stage's own binary.
#
#   $1  stage ("stage2", "stage1")
run_gate_g3_stage() {
  local stage="$1"
  ensure_stage "$stage"

  local log status=0
  log="$(gate_g3_log "$stage" "$stage")"

  mkdir -p "$(gate_g3_dir "$stage")" "$GATES_DIR"

  info "gate-g3: running the selfhost test suite under ${stage}"

  # The suite reads its fixtures relative to the working directory, so both
  # runs start from the project root. In test mode every artifact the selfhost
  # driver writes derives from `-o`, which has to name a file inside the run
  # directory.
  (cd "$PROJECT_ROOT" && timeout "$GATE_G3_TIMEOUT_SECONDS" \
    env IGNIS_STD_PATH="${PROJECT_ROOT}/std" \
    "$(stage_bin "$stage")" test "$ENTRY" -o "$(gate_g3_dir "$stage")/ignis-tests") >"$log" 2>&1 || status=$?

  write_gate_g3_status "$stage" "$stage" "$status"

  info "gate-g3: ${stage} run exited ${status} -> ${log}"
}

run_gate_g3_stage2() { run_gate_g3_stage stage2; }

# Run the selfhost test suite under the host compiler, to compare against a
# built stage's run above.
#
#   $1  stage whose artifact directory this host run is paired with
#   $2  gate id to write if the host compiler cannot even be found (nothing to
#       compare against, and no later step can say why, so the verdict is
#       recorded here)
run_gate_g3_host_for() {
  local stage="$1" gate_id="$2"
  local log host_bin status=0
  log="$(gate_g3_log "$stage" host)"

  mkdir -p "$(gate_g3_dir "$stage")" "$GATES_DIR"

  host_bin="$(command -v "$STAGE0" || true)"

  if [[ -z "$host_bin" ]]; then
    write_gate "$gate_id" fail "host compiler not found: ${STAGE0}" \
      "$(json_object "${stage}_log" "$(gate_g3_log "$stage" "$stage")" host_log "$log")"
    return 0
  fi

  info "gate-g3: running the selfhost test suite under ${host_bin}"

  # The host runs the suite in project mode. Its single-file mode reads no
  # ignis.toml, so the `@compiler` alias the selfhost sources import through
  # would not resolve and the run would end before any test.
  (cd "$PROJECT_ROOT" && timeout "$GATE_G3_TIMEOUT_SECONDS" "$host_bin" test) \
    >"$log" 2>&1 || status=$?

  write_gate_g3_status "$stage" host "$status"

  info "gate-g3: host run exited ${status} -> ${log}"
}

run_gate_g3_host() { run_gate_g3_host_for stage2 G3; }

# Record a host run captured elsewhere instead of running the host suite a
# second time. ci.yml's PR-only `gate-g3-stage1` used to call
# `run_gate_g3_host_for` directly, but that re-runs `<host> test` byte-for-byte
# identically to the job's own "Run the selfhost test suite" step a few
# minutes earlier — this lets that step's own run feed the comparison instead.
#
#   $1  stage whose artifact directory this host run is paired with
#   $2  the host run's exit status
#   $3  path to the host run's already-captured log
run_gate_g3_record_host() {
  local stage="$1" status="$2" log="$3"
  mkdir -p "$(gate_g3_dir "$stage")" "$GATES_DIR"
  [[ "$log" -ef "$(gate_g3_log "$stage" host)" ]] || cp "$log" "$(gate_g3_log "$stage" host)"
  write_gate_g3_status "$stage" host "$status"
  info "gate-g3: recorded a host run that exited ${status} -> $(gate_g3_log "$stage" host)"
}

# Compare a built stage's run with the host's and write the gate result.
#
#   $1  stage ("stage2", "stage1")
#   $2  gate id to write ("G3", "G3-STAGE1")
#   $3  label bootstrap_report.py uses for the built-stage side in the JSON
#       details and summary text (defaults to $1)
run_gate_g3_compare_for() {
  local stage="$1" gate_id="$2" label="${3:-$1}"
  local stage_log host_log stage_status_file host_status_file
  stage_log="$(gate_g3_log "$stage" "$stage")"
  host_log="$(gate_g3_log "$stage" host)"
  stage_status_file="$(gate_g3_status_file "$stage" "$stage")"
  host_status_file="$(gate_g3_status_file "$stage" host)"

  mkdir -p "$GATES_DIR"

  local missing=()
  local path
  for path in "$stage_log" "$host_log" "$stage_status_file" "$host_status_file"; do
    [[ -f "$path" ]] || missing+=("$path")
  done

  if [[ ${#missing[@]} -gt 0 ]]; then
    # One side never produced anything. Whichever side did run may already have
    # recorded why, and that verdict says more than a missing file does.
    if [[ -f "${GATES_DIR}/${gate_id}.json" ]]; then
      info "gate-g3: missing ${missing[*]}, keeping the recorded ${gate_id} result"
      return 0
    fi

    write_gate "$gate_id" fail "the selfhost test runs left nothing to compare" \
      "$(json_object "${stage}_log" "$stage_log" host_log "$host_log" missing "${missing[*]}")"
    return 0
  fi

  local stage_fields host_fields stage_status host_status timeout_seconds
  stage_fields="$(read_gate_g3_status "$stage_status_file")" ||
    fail "gate-g3: ${stage_status_file} is not readable"
  host_fields="$(read_gate_g3_status "$host_status_file")" ||
    fail "gate-g3: ${host_status_file} is not readable"

  stage_status="$(sed -n 1p <<<"$stage_fields")"
  host_status="$(sed -n 1p <<<"$host_fields")"
  timeout_seconds="$(sed -n 2p <<<"$stage_fields")"

  [[ -n "$timeout_seconds" ]] || timeout_seconds="$GATE_G3_TIMEOUT_SECONDS"

  python3 "${SCRIPT_DIR}/bootstrap_report.py" gate-g3 \
    --stage2-log "$stage_log" \
    --host-log "$host_log" \
    --stage2-status "$stage_status" \
    --host-status "$host_status" \
    --timeout-seconds "$timeout_seconds" \
    --label "$label" \
    --gate-id "$gate_id" \
    --output "${GATES_DIR}/${gate_id}.json"

  info "gate-g3: result -> ${GATES_DIR}/${gate_id}.json"
}

run_gate_g3_compare() { run_gate_g3_compare_for stage2 G3 stage2; }

# The whole gate on one machine. A failing half still leaves its log behind, so
# the comparison always runs and holds the verdict.
run_gate_g3() {
  run_gate_g3_stage2 || info "gate-g3: the stage2 run exited non-zero, continuing"
  run_gate_g3_host || info "gate-g3: the host run exited non-zero, continuing"
  run_gate_g3_compare
}

# ci.yml's PR-only check: the same G3 mechanics as above, run against stage1
# instead of stage2. Added to close the gap PR #201 (merged on host-only CI)
# left: the selfhost test suite failed to even link under stage1/stage2 (G3),
# and stage2 panicked on two error-corpus cases (G5), and neither surfaced
# until the nightly ladder ran. Kept out of `run_gate_g3`/`run_gates` because
# the nightly ladder already covers stage2 through the promotion gates; this
# only needs to run once, on the cheaper stage1, before a PR merges.
#
# No host half here: ci.yml already runs `<host> test` in its own "Run the
# selfhost test suite" step and feeds that run into this comparison via
# `gate-g3-record-host`, rather than paying for the same suite twice.
run_gate_g3_stage1() {
  run_gate_g3_stage stage1
  run_gate_g3_compare_for stage1 G3-STAGE1 stage1
}

run_report() {
  mkdir -p "$GATES_DIR"

  python3 "${SCRIPT_DIR}/bootstrap_report.py" report \
    --bootstrap-root "$BOOTSTRAP_ROOT" \
    --project-root "$PROJECT_ROOT"
}

# The nightly's "Publish the promotion state" step's decision logic, factored
# out so it can be exercised without gh/network calls (see
# scripts/tests/test_stage0_fallback.sh). Pure function of the promotion
# verdict: prints one JSON line, never touches the filesystem or a release.
#
# Args: $1 candidate ("true"/"false"), $2 stage0 fallback ("true"/"false"),
#       $3 previous streak (integer, defaults to 0).
#
# Output: {"streak": N, "write_streak": bool, "publish_binary": bool, "reseed": bool}
#   streak         the streak value to persist (only meaningful when write_streak)
#   write_streak   whether promotion-streak.json should be (re)written/uploaded
#   publish_binary whether stage2 should be published as the official asset
#   reseed         whether this is a fallback run re-seeding the official
#                  lineage (for the log/::warning:: wording), never true
#                  together with publish_binary=false
#
# A run whose stage0 fell back from official to host proves the host still
# passes the ladder, which every run already assumes — it says nothing about
# the official asset itself, *unless* the run is a promotion candidate: every
# gate still passed against stage2 built the same way a manual
# `workflow_dispatch stage0=host` seed would build it. Treating that the same
# as a manual seed (publish stage2, reset the streak to 1) stops a stale
# official asset from wedging every following nightly into the same fallback
# until a maintainer notices and re-seeds it by hand. A fallback run with a
# failing gate proves nothing, so it keeps today's behavior: the streak is
# left exactly where it was and nothing is published.
promotion_decide() {
  local candidate="$1" fallback="$2" previous_streak="${3:-0}"

  if [[ "$fallback" == "true" ]]; then
    if [[ "$candidate" == "true" ]]; then
      jq -n '{streak: 1, write_streak: true, publish_binary: true, reseed: true}'
    else
      jq -n --argjson streak "$previous_streak" \
        '{streak: $streak, write_streak: false, publish_binary: false, reseed: false}'
    fi
    return
  fi

  if [[ "$candidate" == "true" ]]; then
    local streak=$((previous_streak + 1))
    local publish="false"
    [[ "$streak" -ge 3 ]] && publish="true"
    jq -n --argjson streak "$streak" --argjson publish "$publish" \
      '{streak: $streak, write_streak: true, publish_binary: $publish, reseed: false}'
  else
    jq -n '{streak: 0, write_streak: true, publish_binary: false, reseed: false}'
  fi
}

# A gate that produced no file at all is neither a pass nor a failure, and the
# report only decides a candidate from results it can read.
seal_missing_gates() {
  mkdir -p "$GATES_DIR"

  local gate
  for gate in "${GATE_IDS[@]}"; do
    [[ -f "${GATES_DIR}/${gate}.json" ]] ||
      write_gate "$gate" skipped "no ${gate} result was produced by this run"
  done
}

# The ladder on its own: stage1 and stage2 have to succeed because every gate
# runs against stage2's binary, while a stage3 failure is the G1 verdict and is
# left for the report rather than ending the run.
#
# G4 compares stage2's measurement with stage1's over the same corpus, so it
# belongs here rather than with the other gates: a ratio only means something
# when both runs were measured on the same machine.
run_stages() {
  build_stage1
  build_stage2

  run_gate_g4 || info "stages: gate-g4 exited non-zero, G4 holds the verdict"

  build_stage3 || info "stages: stage3 exited non-zero, G1 holds the verdict"
}

# Run every stage and gate, then the report. A failing step never stops the run:
# the report is the product and a missing gate result is recorded as skipped.
run_gates() {
  rm -rf "$GATES_DIR"
  mkdir -p "$GATES_DIR"

  local step
  for step in stage1 stage2 parity stage3 gate-g4 gate-g5 gate-g6 gate-g7 gate-g3; do
    info "gates: ${step}"
    "$SELF" "$step" || info "gates: ${step} exited non-zero, continuing"
  done

  seal_missing_gates

  run_report
}

# G5: the selfhost's diagnostics must be equal or better than the host's over
# the error corpus, so every diagnostic the host records has to appear.
#
#   $1  stage to replay the error corpus through ("stage2", "stage1")
#   $2  gate id to write ("G5" for stage2, "G5-STAGE1" for ci.yml's PR-only
#       `gate-g5-stage1`, added alongside gate-g3-stage1 to close the gap PR
#       #201 left: stage2 panicked on two error-corpus cases that only the
#       nightly ladder caught)
run_gate_g5_for() {
  local stage="$1" gate_id="$2"
  ensure_stage "$stage"

  # stage2 keeps its original, unsuffixed report/counts paths so nothing that
  # already reads build/bootstrap/parity-err.{md,json} breaks; any other stage
  # gets its own so a stage1 run never clobbers a concurrent or prior stage2
  # one.
  local suffix=""
  [[ "$stage" == "stage2" ]] || suffix="-${stage}"

  local gates_dir="${BOOTSTRAP_ROOT}/gates"
  local report="${BOOTSTRAP_ROOT}/parity-err${suffix}.md"
  local counts="${BOOTSTRAP_ROOT}/parity-err${suffix}.json"
  local gate_file="${gates_dir}/${gate_id}.json"

  mkdir -p "$gates_dir"
  rm -f "$counts"

  info "gate-g5: replaying the host error corpus through $(stage_bin "$stage")"

  local status="pass"

  if ! python3 "${SCRIPT_DIR}/selfhost_e2e_parity.py" \
    --compiler "$(stage_bin "$stage")" \
    --corpus err \
    --std "${PROJECT_ROOT}/std" \
    --work-dir "${BOOTSTRAP_ROOT}/parity-err${suffix}" \
    --counts-json "$counts" \
    --report "$report"; then
    status="fail"
  fi

  [[ -f "$counts" ]] || fail "gate-g5: the harness wrote no counts, see ${report}"

  GATE_ID="$gate_id" python3 - "$counts" "$gate_file" "$status" <<'PYTHON'
import json
import os
import sys

counts_path, gate_path, status = sys.argv[1:4]
gate_id = os.environ["GATE_ID"]

with open(counts_path, encoding="utf-8") as handle:
  data = json.load(handle)

counts = data["counts"]
gate = {
  "gate": gate_id,
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

run_gate_g5() { run_gate_g5_for stage2 G5; }

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

run_gate_g7() {
  ensure_stage stage2

  local report="${BOOTSTRAP_ROOT}/parity-drops.md"
  local counts="${BOOTSTRAP_ROOT}/parity-drops.json"
  local gate_file="${GATES_DIR}/G7.json"

  mkdir -p "$GATES_DIR"
  rm -f "$gate_file"

  info "gate-g7: diffing the drop schedules of $(stage_bin stage2) against ${STAGE0}'s"

  # A non-zero exit only means some cases diverge; the gate file is the product.
  python3 "${SCRIPT_DIR}/selfhost_drop_schedule_parity.py" \
    --compiler "$(stage_bin stage2)" \
    --host "$STAGE0" \
    --std "${PROJECT_ROOT}/std" \
    --project . \
    --counts-json "$counts" \
    --report "$report" \
    --gate-json "$gate_file" || true

  if [[ ! -f "$gate_file" ]]; then
    write_gate G7 fail "the drop-schedule parity run produced no gate result" \
      "$(json_object report "$report")"
    return 0
  fi

  info "gate-g7: result -> ${gate_file} (report ${report})"
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
    stages) run_stages ;;
    parity) run_parity ;;
    gate-g5) run_gate_g5 ;;
    gate-g5-stage1) run_gate_g5_for stage1 G5-STAGE1 ;;
    gate-g6) run_gate_g6 ;;
    gate-g4) run_gate_g4 ;;
    gate-g7) run_gate_g7 ;;
    gate-g3) run_gate_g3 ;;
    gate-g3-stage2) run_gate_g3_stage2 ;;
    gate-g3-host) run_gate_g3_host ;;
    gate-g3-compare) run_gate_g3_compare ;;
    gate-g3-stage1) run_gate_g3_stage1 ;;
    gate-g3-record-host) run_gate_g3_record_host "${2-}" "${3-}" "${4-}" ;;
    gates) run_gates ;;
    seal-gates) seal_missing_gates ;;
    report) run_report ;;
    promotion-decide) promotion_decide "${2-}" "${3-}" "${4-0}" ;;
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
