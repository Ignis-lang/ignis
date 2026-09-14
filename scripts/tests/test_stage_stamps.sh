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
# Every real stage emits selfhost_emit.c next to its binary; build_stage3's
# G1 comparison reads it unconditionally, so stage3 needs one too, even from
# a fake compiler. Empty and identical every time is fine — G1 is not what
# these tests are checking.
touch "$(dirname "$out")/selfhost_emit.c"
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

# Extra environment for the run, if any, is set by prefixing the *call* to
# run_stage itself (e.g. `IGNIS_BOOTSTRAP_TRUST_STAGES=1 run_stage ...`) —
# not by passing it as a trailing argument here: an expanded "$@" word is
# never re-parsed as an assignment prefix, even when its text looks like
# one, so a trailing `IGNIS_STAGE0_KIND=selfhost` argument would run as a
# (nonexistent) command instead of exporting anything.
run_stage() {
  local root="$1" stage="$2" stage0_bin="$3"
  (
    cd "$root"
    IGNIS_STAGE0="$stage0_bin" scripts/bootstrap.sh "$stage"
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

# Test 6: an artifact round trip (copy the stage tree elsewhere, come back
# with fresh inodes/mtimes but identical content — exactly what
# actions/download-artifact does between the nightly's ladder job and its
# gate jobs) must not look like a compiler change. The stamp's compiler
# identity is a content hash (sha256 + size) precisely so this holds
# (IGN-210 follow-up, review blocker 2) — an earlier path/size/mtime/inode
# identity would misread every single gate job as "compiler identity
# changed" and rebuild there every time.
test_artifact_roundtrip_identity_is_stable() {
  TESTS_RUN=$((TESTS_RUN + 1))
  echo "test: an artifact round trip does not look like a compiler change"

  local root
  root="$(make_sandbox)"
  write_fake_compiler "$root/bin/ignis-stage0"

  run_stage "$root" stage1 "$root/bin/ignis-stage0" >"$root/run1.log" 2>&1
  run_stage "$root" stage2 "$root/bin/ignis-stage0" >"$root/run2.log" 2>&1

  local before_inode1 before_inode2
  before_inode1="$(stat -c '%i' "$root/build/bootstrap/stage1/ignis")"
  before_inode2="$(stat -c '%i' "$root/build/bootstrap/stage2/ignis")"

  # Simulate the round trip: copy each stage directory elsewhere and back
  # (fresh inodes throughout), then touch the binaries so mtime moves too.
  # Content is never touched.
  local elsewhere
  elsewhere="$(mktemp -d)"
  cp -a "$root/build/bootstrap/stage1" "$elsewhere/stage1"
  cp -a "$root/build/bootstrap/stage2" "$elsewhere/stage2"
  rm -rf "$root/build/bootstrap/stage1" "$root/build/bootstrap/stage2"
  cp -a "$elsewhere/stage1" "$root/build/bootstrap/stage1"
  cp -a "$elsewhere/stage2" "$root/build/bootstrap/stage2"
  touch "$root/build/bootstrap/stage1/ignis" "$root/build/bootstrap/stage2/ignis"
  rm -rf "$elsewhere"

  local after_inode1 after_inode2
  after_inode1="$(stat -c '%i' "$root/build/bootstrap/stage1/ignis")"
  after_inode2="$(stat -c '%i' "$root/build/bootstrap/stage2/ignis")"

  if [[ "$before_inode1" != "$after_inode1" && "$before_inode2" != "$after_inode2" ]]; then
    pass "the round trip did give the binaries fresh inodes (sanity check on the test itself)"
  else
    fail_test "the round trip left inodes unchanged — this test would not exercise the bug it targets"
  fi

  # stage3 always recompiles unconditionally once its own check runs, but it
  # gets there through `ensure_stage stage2` (chain-checking stage1 first),
  # the exact decision path a gate job's `ensure_stage stage2` call takes.
  local status=0
  run_stage "$root" stage3 "$root/bin/ignis-stage0" >"$root/run3.log" 2>&1 || status=$?

  if [[ "$status" -ne 0 ]]; then
    fail_test "expected stage3 to succeed, exit ${status}, see ${root}/run3.log"
    sed 's/^/    /' "$root/run3.log"
    rm -rf "$root"
    return
  fi

  if grep -qE 'stage[12]: rebuilding' "$root/run3.log"; then
    fail_test "a round trip alone triggered a rebuild, see ${root}/run3.log"
    sed 's/^/    /' "$root/run3.log"
  else
    pass "neither stage1 nor stage2 was rebuilt after the round trip"
  fi

  rm -rf "$root"
}

# Test 7: `ensure_stage stageN` verifies stageN-1 first (IGN-210 follow-up,
# review blocker 4). Without that chain check, a stage2 whose own stamp
# still matches stage1's *current* (not-yet-rebuilt) identity looks fresh
# even though stage1 itself is stale — and a gate command that calls
# `ensure_stage stage2` directly (gate-g3-stage2, gate-g5, gate-g6, gate-g7)
# never happens to rebuild stage2 unconditionally the way the plain "stage2"
# subcommand does, so it would never notice either.
test_chain_check_catches_a_stale_prior_stage() {
  TESTS_RUN=$((TESTS_RUN + 1))
  echo "test: ensure_stage stage2 verifies stage1 first"

  local root
  root="$(make_sandbox)"
  write_fake_compiler "$root/bin/ignis-stage0"

  run_stage "$root" stage1 "$root/bin/ignis-stage0" >"$root/run1.log" 2>&1
  run_stage "$root" stage2 "$root/bin/ignis-stage0" >"$root/run2.log" 2>&1

  local before_inode1 before_inode2
  before_inode1="$(stat -c '%i' "$root/build/bootstrap/stage1/ignis")"
  before_inode2="$(stat -c '%i' "$root/build/bootstrap/stage2/ignis")"

  # A different stage0 binary at the same path (same trigger as the existing
  # swap test), but — deliberately — neither "stage1" nor "stage2" is run
  # again here. Only a direct `ensure_stage stage2` call, via a gate
  # command, gets a chance to notice.
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
  run_stage "$root" gate-g3-stage2 "$root/bin/ignis-stage0" >"$root/run3.log" 2>&1 || status=$?

  if [[ "$status" -ne 0 ]]; then
    fail_test "expected gate-g3-stage2 to succeed, exit ${status}, see ${root}/run3.log"
    sed 's/^/    /' "$root/run3.log"
    rm -rf "$root"
    return
  fi

  if grep -q 'stage1: rebuilding, stage0 identity changed' "$root/run3.log"; then
    pass "a direct ensure_stage stage2 call caught stage1's own staleness first"
  else
    fail_test "no 'stage1: rebuilding, stage0 identity changed' line from gate-g3-stage2, see ${root}/run3.log"
  fi

  if grep -q 'stage2: rebuilding, compiler identity changed' "$root/run3.log"; then
    pass "stage2 was rebuilt in turn, against the freshly rebuilt stage1"
  else
    fail_test "no 'stage2: rebuilding, compiler identity changed' line, see ${root}/run3.log"
  fi

  local after_inode1 after_inode2
  after_inode1="$(stat -c '%i' "$root/build/bootstrap/stage1/ignis")"
  after_inode2="$(stat -c '%i' "$root/build/bootstrap/stage2/ignis")"

  [[ "$before_inode1" != "$after_inode1" ]] && pass "stage1/ignis was rebuilt" \
    || fail_test "stage1/ignis inode is unchanged"
  [[ "$before_inode2" != "$after_inode2" ]] && pass "stage2/ignis was rebuilt" \
    || fail_test "stage2/ignis inode is unchanged"

  rm -rf "$root"
}

# Test 8: a stage0 lineage that is not available here refuses the rebuild
# instead of silently switching to whatever stage0 happens to be resolvable
# (IGN-210 follow-up, review blocker 3) — the nightly gate-job scenario:
# IGNIS_STAGE0 is unset there, stage0.json still says kind=official from the
# separate ladder-build job, and the official asset itself (an ephemeral
# runner-temp path) is gone.
test_stage0_kind_mismatch_refuses() {
  TESTS_RUN=$((TESTS_RUN + 1))
  echo "test: a stage0 lineage mismatch refuses to rebuild rather than switching lineage"

  local root
  root="$(make_sandbox)"
  write_fake_compiler "$root/bin/ignis-stage0"

  printf '{"kind": "official", "source": "%s/does-not-exist/ignis-official", "sha256": "", "mode": "auto"}\n' "$root" \
    >"$root/build/bootstrap/stage0.json"

  # No stage1 binary exists at all here (never downloaded, or missing) —
  # ensure_stage's "missing binary" path alone is enough to reach the guard.
  local status=0
  run_stage "$root" stage2 "$root/bin/ignis-stage0" >"$root/run.log" 2>&1 || status=$?

  if [[ "$status" -eq 0 ]]; then
    fail_test "expected stage2 to fail on the lineage mismatch, it exited 0, see ${root}/run.log"
  else
    pass "stage2 exited non-zero (${status})"
  fi

  if grep -q 'ensure_stage: refusing to rebuild stage1' "$root/run.log" && grep -q 'kind=official' "$root/run.log"; then
    pass "the refusal names the stage0.json kind mismatch"
  else
    fail_test "no clear refusal message, see ${root}/run.log"
    sed 's/^/    /' "$root/run.log"
  fi

  if [[ -x "$root/build/bootstrap/stage1/ignis" ]]; then
    fail_test "stage1/ignis should not have been built against the wrong lineage"
  else
    pass "no stage1 binary was built"
  fi

  rm -rf "$root"
}

# Test 9: the same mismatch, but with an explicit IGNIS_STAGE0_KIND — a
# deliberate, informed override — proceeds instead of refusing.
test_stage0_kind_mismatch_override_proceeds() {
  TESTS_RUN=$((TESTS_RUN + 1))
  echo "test: IGNIS_STAGE0_KIND overrides the stage0 lineage mismatch guard"

  local root
  root="$(make_sandbox)"
  write_fake_compiler "$root/bin/ignis-stage0"

  printf '{"kind": "official", "source": "%s/does-not-exist/ignis-official", "sha256": "", "mode": "auto"}\n' "$root" \
    >"$root/build/bootstrap/stage0.json"

  local status=0
  IGNIS_STAGE0_KIND=selfhost \
    run_stage "$root" stage2 "$root/bin/ignis-stage0" >"$root/run.log" 2>&1 || status=$?

  if [[ "$status" -eq 0 ]]; then
    pass "stage2 exited 0 with the explicit override"
  else
    fail_test "expected stage2 to succeed with IGNIS_STAGE0_KIND=selfhost, exit ${status}, see ${root}/run.log"
    sed 's/^/    /' "$root/run.log"
  fi

  rm -rf "$root"
}

# Test 10: a source edit must be detected even when the sandbox happens to
# sit inside an unrelated outer git repository — this is what actually broke
# CI for test_source_change_rebuilds: on that runner the sandbox (a mktemp -d
# directory) was nested under a directory `git` already considered part of a
# work tree, so `git ls-files -z -- ignis std` resolved those pathspecs
# against the *outer* repo's index — where a random tmp subdirectory's
# ignis/std were never tracked — matched nothing, and produced a hash that
# never changed no matter what changed on disk in the sandbox. sources_hash
# now only trusts `git ls-files` when `$PROJECT_ROOT` is itself the
# discovered repository's toplevel, not merely nested inside one; otherwise
# it falls back to `find`, which is always correct here regardless of what
# surrounds the sandbox.
test_source_change_detected_inside_an_outer_git_repo() {
  TESTS_RUN=$((TESTS_RUN + 1))
  echo "test: a source edit is detected even when the sandbox sits inside an unrelated outer git repo"

  local outer
  outer="$(mktemp -d)"
  (
    cd "$outer" &&
      git init -q &&
      git config user.email test@example.com &&
      git config user.name test &&
      mkdir -p unrelated &&
      echo x >unrelated/file &&
      git add unrelated &&
      git commit -q -m outer
  )

  local root="$outer/sandbox"
  mkdir -p "$root/scripts" "$root/ignis" "$root/std" "$root/bin" "$root/build/bootstrap"
  cp "$BOOTSTRAP_SH" "$root/scripts/bootstrap.sh"
  cp "$MEASURE_RUN_PY" "$root/scripts/measure_run.py"
  printf 'function main(): i32 { return 0; }\n' >"$root/ignis/main.ign"
  printf 'namespace Std { }\n' >"$root/std/lib.ign"
  write_fake_compiler "$root/bin/ignis-stage0"

  # Sanity check on the reproduction itself: if the sandbox is not actually
  # inside the outer repo's work tree, this test proves nothing.
  local inside
  inside="$(cd "$root" && git rev-parse --is-inside-work-tree 2>/dev/null || echo false)"
  if [[ "$inside" != "true" ]]; then
    fail_test "the sandbox is not nested inside a git work tree — this reproduction did not set up what it targets"
    rm -rf "$outer"
    return
  fi

  run_stage "$root" stage1 "$root/bin/ignis-stage0" >"$root/run1.log" 2>&1
  local before_inode
  before_inode="$(stat -c '%i' "$root/build/bootstrap/stage1/ignis")"

  printf 'namespace Std { function extra(): void {} }\n' >"$root/std/lib.ign"

  local status=0
  run_stage "$root" stage2 "$root/bin/ignis-stage0" >"$root/run2.log" 2>&1 || status=$?

  if [[ "$status" -ne 0 ]]; then
    fail_test "expected stage2 to succeed, exit ${status}, see ${root}/run2.log"
    sed 's/^/    /' "$root/run2.log"
    rm -rf "$outer"
    return
  fi

  if grep -q 'stage1: rebuilding, sources changed' "$root/run2.log"; then
    pass "stage1 rebuilt with reason 'sources changed' despite the outer git repo"
  else
    fail_test "no 'stage1: rebuilding, sources changed' line, see ${root}/run2.log"
  fi

  local after_inode
  after_inode="$(stat -c '%i' "$root/build/bootstrap/stage1/ignis")"
  [[ "$before_inode" != "$after_inode" ]] && pass "stage1/ignis binary was recreated" \
    || fail_test "stage1/ignis binary was not recreated after a source edit"

  rm -rf "$outer"
}

test_fresh_build_writes_stamp
test_unchanged_state_reuses
test_source_change_rebuilds
test_stage0_swap_rebuilds
test_trust_stages_skips_the_check
test_artifact_roundtrip_identity_is_stable
test_chain_check_catches_a_stale_prior_stage
test_stage0_kind_mismatch_refuses
test_source_change_detected_inside_an_outer_git_repo
test_stage0_kind_mismatch_override_proceeds

echo
echo "${TESTS_RUN} test(s) run, ${FAILURES} failure(s)"
[[ "$FAILURES" -eq 0 ]]
