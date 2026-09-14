#!/usr/bin/env bash
#
# Exercises scripts/bootstrap.sh's stage staleness stamps (IGN-210):
# `ensure_stage` used to reuse an existing build/bootstrap/<stage>/ignis
# unconditionally, even when the selfhost sources, std, or the stage0/host
# compiler had changed since it was built, so a gate (gate-g7, gate-g5,
# gate-g3-*) could silently report results for stale code.
#
# Each test runs scripts/bootstrap.sh in an isolated project root (a temp
# directory with its own scripts/, ignis/, std/ and build/bootstrap), the
# same sandbox style scripts/tests/test_stage0_fallback.sh uses, so nothing
# here touches the real repository's build/ directory or does a real
# self-compilation. A fake "compiler" stands in for stage0/stage1/stage2: it
# copies itself to whatever `-o` names, so a stage it produces is itself a
# working fake compiler for the next stage, without ever running gcc or a
# real Ignis compile.
#
# Wired into ci.yml's selfhost job, next to test_stage0_fallback.sh — it
# takes a few seconds.
#
# Usage: scripts/tests/test_stage_stamps.sh

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(dirname "$SCRIPT_DIR")"
BOOTSTRAP_SH="${REPO_ROOT}/bootstrap.sh"
MEASURE_RUN_PY="${REPO_ROOT}/measure_run.py"

FAILURES=0
TESTS_RUN=0

pass() { echo "  ok: $1"; }
fail_test() {
  echo "  FAIL: $1"
  FAILURES=$((FAILURES + 1))
}

json_get() {
  local path="$1" key="$2"
  python3 -c 'import json,sys; v=json.load(open(sys.argv[1])).get(sys.argv[2]); print(v if v is not None else "")' "$path" "$key"
}

# A fake "compiler" invoked as every stage is: `compiler entry -o output`.
# Rather than compile anything, it copies itself to `-o`'s target, so the
# binary it produces is itself a working fake compiler the next stage can
# use the same way — enough to walk stage1 -> stage2 -> stage3 without a real
# self-compilation. `--version` fails, so `stage0_is_selfhost` classifies it
# as a selfhost-shaped stage0 and compiles it directly rather than through
# `<bin> build`.
write_fake_compiler() {
  local path="$1"
  cat >"$path" <<'EOF'
#!/usr/bin/env bash
if [[ "${1-}" == "--version" ]]; then
  exit 1
fi

out=""
prev=""
for arg in "$@"; do
  if [[ "$prev" == "-o" ]]; then
    out="$arg"
  fi
  prev="$arg"
done

if [[ -z "$out" ]]; then
  echo "fake compiler: no -o given" >&2
  exit 1
fi

mkdir -p "$(dirname "$out")"
cp "$0" "$out"
chmod +x "$out"
EOF
  chmod +x "$path"
}

# A throwaway project root with just enough shape for `scripts/bootstrap.sh
# stage1`/`stage2` to run against a fake compiler: its own scripts/,
# ignis/main.ign, std/ (both hashed by sources_hash), and bin/ for the fake
# stage0.
make_sandbox() {
  local root
  root="$(mktemp -d)"

  mkdir -p "$root/scripts" "$root/ignis" "$root/std" "$root/bin" "$root/build/bootstrap"
  cp "$BOOTSTRAP_SH" "$root/scripts/bootstrap.sh"
  cp "$MEASURE_RUN_PY" "$root/scripts/measure_run.py"
  printf 'function main(): i32 { return 0; }\n' >"$root/ignis/main.ign"
  printf 'namespace Std { }\n' >"$root/std/lib.ign"

  echo "$root"
}

run_stage() {
  local root="$1" stage="$2" stage0_bin="$3"
  shift 3
  (
    cd "$root"
    IGNIS_STAGE0="$stage0_bin" "$@" scripts/bootstrap.sh "$stage"
  )
}

# Test 1: a fresh stage1 build writes build/bootstrap/stage1/stamp.json with
# every field ensure_stage later reads back.
test_fresh_build_writes_stamp() {
  TESTS_RUN=$((TESTS_RUN + 1))
  echo "test: a fresh stage1 build writes a stamp"

  local root
  root="$(make_sandbox)"
  write_fake_compiler "$root/bin/ignis-stage0"

  local status=0
  run_stage "$root" stage1 "$root/bin/ignis-stage0" >"$root/run.log" 2>&1 || status=$?

  if [[ "$status" -ne 0 ]]; then
    fail_test "expected stage1 to succeed, exit ${status}, see ${root}/run.log"
    sed 's/^/    /' "$root/run.log"
    rm -rf "$root"
    return
  fi
  pass "stage1 exited 0"

  local stamp="$root/build/bootstrap/stage1/stamp.json"
  if [[ -f "$stamp" ]]; then
    pass "stage1/stamp.json was written"
  else
    fail_test "stage1/stamp.json is missing"
    rm -rf "$root"
    return
  fi

  local scheme_version compiler_identity stage0_identity sources_hash
  scheme_version="$(json_get "$stamp" scheme_version)"
  compiler_identity="$(json_get "$stamp" compiler_identity)"
  stage0_identity="$(json_get "$stamp" stage0_identity)"
  sources_hash="$(json_get "$stamp" sources_hash)"

  [[ -n "$scheme_version" ]] && pass "stamp carries scheme_version" || fail_test "stamp scheme_version is empty"
  [[ -n "$compiler_identity" && "$compiler_identity" != "unknown" ]] && pass "stamp carries a resolved compiler_identity" \
    || fail_test "stamp compiler_identity is '${compiler_identity}'"
  [[ "$stage0_identity" == "$compiler_identity" ]] && pass "stage0_identity matches compiler_identity on the direct (non-fallback) path" \
    || fail_test "stage0_identity ('${stage0_identity}') differs from compiler_identity ('${compiler_identity}')"
  [[ -n "$sources_hash" ]] && pass "stamp carries a sources_hash" || fail_test "stamp sources_hash is empty"

  rm -rf "$root"
}

# Test 2: an unchanged state reuses the existing stage1 binary — no rebuild
# is logged, and the stamp/binary are left untouched.
test_unchanged_state_reuses() {
  TESTS_RUN=$((TESTS_RUN + 1))
  echo "test: an unchanged state reuses the existing stage1 binary"

  local root
  root="$(make_sandbox)"
  write_fake_compiler "$root/bin/ignis-stage0"

  run_stage "$root" stage1 "$root/bin/ignis-stage0" >"$root/run1.log" 2>&1

  local stamp="$root/build/bootstrap/stage1/stamp.json"
  local before_mtime before_hash
  before_mtime="$(stat -c '%Y' "$stamp")"
  before_hash="$(sha256sum "$root/build/bootstrap/stage1/ignis" | cut -d' ' -f1)"

  # `ensure_stage` is only reached from build_stage2/build_stage3/the gates,
  # never from the `stage1` subcommand itself (which always calls
  # build_stage1 unconditionally, matching `all`/`stages`'s "never skip a
  # stale stage" contract) — so build_stage2 is what exercises the reuse
  # path here, with stage2 sharing the same fake-compiler behavior stage1
  # does.
  local status=0
  run_stage "$root" stage2 "$root/bin/ignis-stage0" >"$root/run2.log" 2>&1 || status=$?

  if [[ "$status" -ne 0 ]]; then
    fail_test "expected stage2 to succeed, exit ${status}, see ${root}/run2.log"
    sed 's/^/    /' "$root/run2.log"
    rm -rf "$root"
    return
  fi

  if grep -q 'stage1: rebuilding' "$root/run2.log"; then
    fail_test "stage1 was rebuilt on an unchanged state, see ${root}/run2.log"
  else
    pass "stage1 was not rebuilt"
  fi

  local after_mtime after_hash
  after_mtime="$(stat -c '%Y' "$stamp")"
  after_hash="$(sha256sum "$root/build/bootstrap/stage1/ignis" | cut -d' ' -f1)"

  [[ "$before_mtime" == "$after_mtime" ]] && pass "stage1/stamp.json was not rewritten" \
    || fail_test "stage1/stamp.json mtime changed (${before_mtime} -> ${after_mtime})"
  [[ "$before_hash" == "$after_hash" ]] && pass "stage1/ignis binary was not rewritten" \
    || fail_test "stage1/ignis binary content changed"

  rm -rf "$root"
}

# Test 3: touching a source file (ignis/ or std/) rebuilds stage1, with the
# "sources changed" reason logged.
test_source_change_rebuilds() {
  TESTS_RUN=$((TESTS_RUN + 1))
  echo "test: touching a source file rebuilds stage1"

  local root
  root="$(make_sandbox)"
  write_fake_compiler "$root/bin/ignis-stage0"

  run_stage "$root" stage1 "$root/bin/ignis-stage0" >"$root/run1.log" 2>&1
  local before_inode
  before_inode="$(stat -c '%i' "$root/build/bootstrap/stage1/ignis")"

  # mtime alone can be too coarse a clock on some filesystems; sources_hash
  # is content-based, so a plain content change (no sleep needed) is already
  # enough to flip it.
  printf 'namespace Std { function extra(): void {} }\n' >"$root/std/lib.ign"

  local status=0
  run_stage "$root" stage2 "$root/bin/ignis-stage0" >"$root/run2.log" 2>&1 || status=$?

  if [[ "$status" -ne 0 ]]; then
    fail_test "expected stage2 to succeed, exit ${status}, see ${root}/run2.log"
    sed 's/^/    /' "$root/run2.log"
    rm -rf "$root"
    return
  fi

  if grep -q 'stage1: rebuilding, sources changed' "$root/run2.log"; then
    pass "stage1 rebuilt with reason 'sources changed'"
  else
    fail_test "no 'stage1: rebuilding, sources changed' line, see ${root}/run2.log"
  fi

  # The fake compiler always re-emits byte-identical content (a self-copy),
  # so content alone cannot prove a rebuild happened; compile_stage's
  # `rm -rf "$dir"; mkdir -p "$dir"` before every build does give the new
  # file a fresh inode, which content equality would not.
  local after_inode
  after_inode="$(stat -c '%i' "$root/build/bootstrap/stage1/ignis")"
  [[ "$before_inode" != "$after_inode" ]] && pass "stage1/ignis binary was recreated" \
    || fail_test "stage1/ignis binary was not recreated after a source edit"

  rm -rf "$root"
}

# Test 4: swapping the binary at the stage0 path (same path, different
# content) rebuilds stage1, with the "stage0 identity changed" reason
# logged.
test_stage0_swap_rebuilds() {
  TESTS_RUN=$((TESTS_RUN + 1))
  echo "test: swapping the stage0 binary rebuilds stage1"

  local root
  root="$(make_sandbox)"
  write_fake_compiler "$root/bin/ignis-stage0"

  run_stage "$root" stage1 "$root/bin/ignis-stage0" >"$root/run1.log" 2>&1
  local before_hash
  before_hash="$(sha256sum "$root/build/bootstrap/stage1/ignis" | cut -d' ' -f1)"

  # A different fake compiler at the same $STAGE0 path: different size, so
  # the identity differs even if the filesystem's mtime resolution is coarse.
  cat >"$root/bin/ignis-stage0" <<'EOF'
#!/usr/bin/env bash
# A different stage0 binary than the one that built the existing stage1.
if [[ "${1-}" == "--version" ]]; then
  exit 1
fi

out=""
prev=""
for arg in "$@"; do
  if [[ "$prev" == "-o" ]]; then
    out="$arg"
  fi
  prev="$arg"
done

[[ -n "$out" ]] || exit 1
mkdir -p "$(dirname "$out")"
cp "$0" "$out"
chmod +x "$out"
EOF
  chmod +x "$root/bin/ignis-stage0"

  local status=0
  run_stage "$root" stage2 "$root/bin/ignis-stage0" >"$root/run2.log" 2>&1 || status=$?

  if [[ "$status" -ne 0 ]]; then
    fail_test "expected stage2 to succeed, exit ${status}, see ${root}/run2.log"
    sed 's/^/    /' "$root/run2.log"
    rm -rf "$root"
    return
  fi

  if grep -q 'stage1: rebuilding, stage0 identity changed' "$root/run2.log"; then
    pass "stage1 rebuilt with reason 'stage0 identity changed'"
  else
    fail_test "no 'stage1: rebuilding, stage0 identity changed' line, see ${root}/run2.log"
  fi

  local after_hash
  after_hash="$(sha256sum "$root/build/bootstrap/stage1/ignis" | cut -d' ' -f1)"
  [[ "$before_hash" != "$after_hash" ]] && pass "stage1/ignis binary was rewritten" \
    || fail_test "stage1/ignis binary is unchanged after swapping stage0"

  rm -rf "$root"
}

# Test 5: IGNIS_BOOTSTRAP_TRUST_STAGES=1 opts out of the staleness check
# entirely, reusing stage1 even though its sources changed.
test_trust_stages_skips_the_check() {
  TESTS_RUN=$((TESTS_RUN + 1))
  echo "test: IGNIS_BOOTSTRAP_TRUST_STAGES=1 skips the staleness check"

  local root
  root="$(make_sandbox)"
  write_fake_compiler "$root/bin/ignis-stage0"

  run_stage "$root" stage1 "$root/bin/ignis-stage0" >"$root/run1.log" 2>&1
  local before_hash
  before_hash="$(sha256sum "$root/build/bootstrap/stage1/ignis" | cut -d' ' -f1)"

  printf 'namespace Std { function extra(): void {} }\n' >"$root/std/lib.ign"

  local status=0
  IGNIS_BOOTSTRAP_TRUST_STAGES=1 \
    run_stage "$root" stage2 "$root/bin/ignis-stage0" >"$root/run2.log" 2>&1 || status=$?

  if [[ "$status" -ne 0 ]]; then
    fail_test "expected stage2 to succeed, exit ${status}, see ${root}/run2.log"
    sed 's/^/    /' "$root/run2.log"
    rm -rf "$root"
    return
  fi

  if grep -q 'stage1: rebuilding' "$root/run2.log"; then
    fail_test "stage1 was rebuilt despite IGNIS_BOOTSTRAP_TRUST_STAGES=1, see ${root}/run2.log"
  else
    pass "stage1 was not rebuilt"
  fi

  local after_hash
  after_hash="$(sha256sum "$root/build/bootstrap/stage1/ignis" | cut -d' ' -f1)"
  [[ "$before_hash" == "$after_hash" ]] && pass "stage1/ignis binary was reused despite the source change" \
    || fail_test "stage1/ignis binary changed even though IGNIS_BOOTSTRAP_TRUST_STAGES=1 was set"

  rm -rf "$root"
}

test_fresh_build_writes_stamp
test_unchanged_state_reuses
test_source_change_rebuilds
test_stage0_swap_rebuilds
test_trust_stages_skips_the_check

echo
echo "${TESTS_RUN} test(s) run, ${FAILURES} failure(s)"
[[ "$FAILURES" -eq 0 ]]
