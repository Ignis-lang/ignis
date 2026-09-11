#!/usr/bin/env bash
#
# Exercises scripts/bootstrap.sh's stage1 stage0 fallback without running a
# real self-compilation: a fake "official" compiler that always fails stands
# in for a selfhost binary that has gone stale against a link/runtime
# contract change on main (the nightly-34644317672 case: PR #176 removed
# std/runtime/libignis_rt.a, and the official binary still passed it to
# `ld`), and a fake "host" compiler stands in for the Rust CLI.
#
# Each test runs scripts/bootstrap.sh in an isolated project root (a temp
# directory with its own build/bootstrap), so nothing here touches the real
# repository's build/ directory.
#
# Usage: scripts/tests/test_stage0_fallback.sh

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

# Build a throwaway project root with just enough shape for
# `scripts/bootstrap.sh stage1` to run: its own scripts/, ignis/main.ign
# (never actually read by the fake compilers) and bin/ for the fake
# compilers.
make_sandbox() {
  local root
  root="$(mktemp -d)"

  mkdir -p "$root/scripts" "$root/ignis" "$root/bin" "$root/build/bootstrap"
  cp "$BOOTSTRAP_SH" "$root/scripts/bootstrap.sh"
  cp "$MEASURE_RUN_PY" "$root/scripts/measure_run.py"
  printf 'function main(): i32 { return 0; }\n' >"$root/ignis/main.ign"

  echo "$root"
}

# A selfhost-shaped compiler invoked as every other stage is: `compiler
# entry -o output`. Always fails, with an error line compile_stage's log
# will carry through to first_error_line.
write_failing_official_compiler() {
  local path="$1"
  cat >"$path" <<'EOF'
#!/usr/bin/env bash
echo "error: undefined reference to symbol from libignis_rt.a" >&2
exit 1
EOF
  chmod +x "$path"
}

# A host-shaped compiler invoked as `compiler build` from the project root,
# which writes build/selfhost/bin/ignis the way `ignis build` does.
write_working_host_compiler() {
  local path="$1"
  cat >"$path" <<'EOF'
#!/usr/bin/env bash
if [[ "${1-}" == "build" ]]; then
  mkdir -p build/selfhost/bin
  printf '#!/usr/bin/env bash\nexit 0\n' >build/selfhost/bin/ignis
  chmod +x build/selfhost/bin/ignis
  exit 0
fi
echo "unexpected invocation: $*" >&2
exit 1
EOF
  chmod +x "$path"
}

write_stage0_json() {
  local root="$1" kind="$2" mode="$3"
  printf '{"kind": "%s", "source": "official", "sha256": "deadbeef", "mode": "%s"}\n' \
    "$kind" "$mode" >"$root/build/bootstrap/stage0.json"
}

# Test 1: `auto` resolved to an official stage0 that cannot build stage1 ->
# stage1 falls back to the host and succeeds, and stage0.json records the
# fallback for the report.
test_auto_falls_back_on_official_failure() {
  TESTS_RUN=$((TESTS_RUN + 1))
  echo "test: auto stage0=official falls back to the host on stage1 failure"

  local root
  root="$(make_sandbox)"
  write_failing_official_compiler "$root/bin/ignis-official"
  write_working_host_compiler "$root/bin/ignis-host"
  write_stage0_json "$root" official auto

  local status=0
  (
    cd "$root"
    IGNIS_STAGE0="$root/bin/ignis-official" \
      IGNIS_STAGE0_HOST_FALLBACK="$root/bin/ignis-host" \
      scripts/bootstrap.sh stage1
  ) >"$root/run.log" 2>&1 || status=$?

  if [[ "$status" -ne 0 ]]; then
    fail_test "expected stage1 to succeed via fallback, exit ${status}, see ${root}/run.log"
    sed 's/^/    /' "$root/run.log"
    return
  fi
  pass "stage1 exited 0"

  if [[ -x "$root/build/bootstrap/stage1/ignis" ]]; then
    pass "build/bootstrap/stage1/ignis was produced"
  else
    fail_test "build/bootstrap/stage1/ignis is missing"
  fi

  if grep -q 'falling back to the host' "$root/run.log"; then
    pass "the fallback was logged"
  else
    fail_test "no fallback log line, see ${root}/run.log"
  fi

  local stage0_json="$root/build/bootstrap/stage0.json"
  local kind fallback reason original_kind
  kind="$(python3 -c 'import json,sys; print(json.load(open(sys.argv[1])).get("kind"))' "$stage0_json")"
  fallback="$(python3 -c 'import json,sys; print(json.load(open(sys.argv[1])).get("fallback"))' "$stage0_json")"
  reason="$(python3 -c 'import json,sys; print(json.load(open(sys.argv[1])).get("fallback_reason") or "")' "$stage0_json")"
  original_kind="$(python3 -c 'import json,sys; print(json.load(open(sys.argv[1])).get("original_kind") or "")' "$stage0_json")"

  [[ "$kind" == "host" ]] && pass "stage0.json kind is host" || fail_test "stage0.json kind is '${kind}', expected host"
  [[ "$fallback" == "True" ]] && pass "stage0.json fallback is true" || fail_test "stage0.json fallback is '${fallback}', expected True"
  [[ -n "$reason" ]] && pass "stage0.json fallback_reason is set (${reason})" || fail_test "stage0.json fallback_reason is empty"
  [[ "$original_kind" == "official" ]] && pass "stage0.json original_kind is official" || fail_test "stage0.json original_kind is '${original_kind}'"

  rm -rf "$root"
}

# Test 2: an explicit `stage0=official` must fail outright on the same
# failure, never falling back.
test_explicit_official_does_not_fall_back() {
  TESTS_RUN=$((TESTS_RUN + 1))
  echo "test: explicit stage0=official fails outright, no fallback"

  local root
  root="$(make_sandbox)"
  write_failing_official_compiler "$root/bin/ignis-official"
  write_working_host_compiler "$root/bin/ignis-host"
  write_stage0_json "$root" official official

  local status=0
  (
    cd "$root"
    IGNIS_STAGE0="$root/bin/ignis-official" \
      IGNIS_STAGE0_HOST_FALLBACK="$root/bin/ignis-host" \
      scripts/bootstrap.sh stage1
  ) >"$root/run.log" 2>&1 || status=$?

  if [[ "$status" -eq 0 ]]; then
    fail_test "expected stage1 to fail, it exited 0, see ${root}/run.log"
  else
    pass "stage1 exited non-zero (${status})"
  fi

  if [[ -x "$root/build/bootstrap/stage1/ignis" ]]; then
    fail_test "build/bootstrap/stage1/ignis should not exist after a forced failure"
  else
    pass "no stage1 binary was produced"
  fi

  if grep -q 'falling back to the host' "$root/run.log"; then
    fail_test "an explicit stage0=official must not fall back, see ${root}/run.log"
  else
    pass "no fallback was attempted"
  fi

  local stage0_json="$root/build/bootstrap/stage0.json"
  local kind fallback
  kind="$(python3 -c 'import json,sys; print(json.load(open(sys.argv[1])).get("kind"))' "$stage0_json")"
  fallback="$(python3 -c 'import json,sys; print(json.load(open(sys.argv[1])).get("fallback") or False)' "$stage0_json")"

  [[ "$kind" == "official" ]] && pass "stage0.json kind is still official" || fail_test "stage0.json kind is '${kind}', expected official"
  [[ "$fallback" == "False" ]] && pass "stage0.json carries no fallback" || fail_test "stage0.json fallback is '${fallback}', expected none"

  rm -rf "$root"
}

# Test 3: stage0=host never touches the fallback path or stage0.json.
test_host_stage0_is_unaffected() {
  TESTS_RUN=$((TESTS_RUN + 1))
  echo "test: stage0=host builds stage1 directly, untouched by the fallback"

  local root
  root="$(make_sandbox)"
  write_working_host_compiler "$root/bin/ignis-host"
  write_stage0_json "$root" host host

  local status=0
  (
    cd "$root"
    IGNIS_STAGE0="$root/bin/ignis-host" \
      scripts/bootstrap.sh stage1
  ) >"$root/run.log" 2>&1 || status=$?

  if [[ "$status" -eq 0 ]]; then
    pass "stage1 exited 0"
  else
    fail_test "expected stage1 to succeed, exit ${status}, see ${root}/run.log"
    sed 's/^/    /' "$root/run.log"
  fi

  if [[ -x "$root/build/bootstrap/stage1/ignis" ]]; then
    pass "build/bootstrap/stage1/ignis was produced"
  else
    fail_test "build/bootstrap/stage1/ignis is missing"
  fi

  local stage0_json="$root/build/bootstrap/stage0.json"
  local fallback
  fallback="$(python3 -c 'import json,sys; print(json.load(open(sys.argv[1])).get("fallback") or False)' "$stage0_json")"
  [[ "$fallback" == "False" ]] && pass "stage0.json carries no fallback" || fail_test "stage0.json fallback is '${fallback}', expected none"

  rm -rf "$root"
}

test_auto_falls_back_on_official_failure
test_explicit_official_does_not_fall_back
test_host_stage0_is_unaffected

echo
echo "${TESTS_RUN} test(s) run, ${FAILURES} failure(s)"
[[ "$FAILURES" -eq 0 ]]
