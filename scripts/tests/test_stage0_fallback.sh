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
# repository's build/ directory. Wired into ci.yml's selfhost job — it takes
# a few seconds.
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

json_get() {
  local path="$1" key="$2"
  python3 -c 'import json,sys; v=json.load(open(sys.argv[1])).get(sys.argv[2]); print(v if v is not None else "")' "$path" "$key"
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
#
# Also recognizes the host's `<bin> build` invocation and fails differently
# and loudly there: a correct fallback never calls this binary that way (see
# stage0_is_selfhost / write_stage0_fallback's kind handling), so a run whose
# log contains that message caught a regression rather than the ladder's
# ordinary failure path.
write_failing_official_compiler() {
  local path="$1"
  cat >"$path" <<'EOF'
#!/usr/bin/env bash
if [[ "${1-}" == "build" ]]; then
  echo "error: REGRESSION official/selfhost stage0 was invoked in host build form" >&2
  exit 1
fi
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

# A host-shaped compiler that always fails, for the "fallback taken and the
# host build also fails" case.
write_failing_host_compiler() {
  local path="$1"
  cat >"$path" <<'EOF'
#!/usr/bin/env bash
echo "error: host compiler is broken too" >&2
exit 1
EOF
  chmod +x "$path"
}

write_stage0_json() {
  local root="$1" kind="$2" mode="$3"
  printf '{"kind": "%s", "source": "official", "sha256": "deadbeef", "mode": "%s"}\n' \
    "$kind" "$mode" >"$root/build/bootstrap/stage0.json"
}

run_stage1() {
  local root="$1" official_bin="$2" host_bin="$3"
  (
    cd "$root"
    IGNIS_STAGE0="$official_bin" \
      IGNIS_STAGE0_HOST_FALLBACK="$host_bin" \
      scripts/bootstrap.sh stage1
  )
}

assert_fallback_stage0_json() {
  local root="$1" expected_mode="$2"
  local stage0_json="$root/build/bootstrap/stage0.json"
  local kind mode fallback reason original_kind used_kind

  kind="$(json_get "$stage0_json" kind)"
  mode="$(json_get "$stage0_json" mode)"
  fallback="$(json_get "$stage0_json" fallback)"
  reason="$(json_get "$stage0_json" fallback_reason)"
  original_kind="$(json_get "$stage0_json" original_kind)"
  used_kind="$(json_get "$stage0_json" used_kind)"

  [[ "$kind" == "official" ]] && pass "stage0.json kind stays official (the resolved stage0, read back by stage0_is_selfhost)" \
    || fail_test "stage0.json kind is '${kind}', expected official"
  [[ "$mode" == "$expected_mode" ]] && pass "stage0.json mode stays ${expected_mode}" \
    || fail_test "stage0.json mode is '${mode}', expected ${expected_mode}"
  [[ "$fallback" == "True" ]] && pass "stage0.json fallback is true" || fail_test "stage0.json fallback is '${fallback}', expected True"
  [[ -n "$reason" ]] && pass "stage0.json fallback_reason is set (${reason})" || fail_test "stage0.json fallback_reason is empty"
  [[ "$original_kind" == "official" ]] && pass "stage0.json original_kind is official" || fail_test "stage0.json original_kind is '${original_kind}'"
  [[ "$used_kind" == "host" ]] && pass "stage0.json used_kind is host" || fail_test "stage0.json used_kind is '${used_kind}', expected host"
}

# Test 1: `auto` resolved to an official stage0 that cannot build stage1 ->
# stage1 falls back to the host and succeeds, and stage0.json records the
# fallback for the report without disturbing kind/mode.
test_auto_falls_back_on_official_failure() {
  TESTS_RUN=$((TESTS_RUN + 1))
  echo "test: auto stage0=official falls back to the host on stage1 failure"

  local root
  root="$(make_sandbox)"
  write_failing_official_compiler "$root/bin/ignis-official"
  write_working_host_compiler "$root/bin/ignis-host"
  write_stage0_json "$root" official auto

  local status=0
  run_stage1 "$root" "$root/bin/ignis-official" "$root/bin/ignis-host" >"$root/run.log" 2>&1 || status=$?

  if [[ "$status" -ne 0 ]]; then
    fail_test "expected stage1 to succeed via fallback, exit ${status}, see ${root}/run.log"
    sed 's/^/    /' "$root/run.log"
    rm -rf "$root"
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

  if grep -q 'REGRESSION' "$root/run.log"; then
    fail_test "the official binary was invoked in host build form, see ${root}/run.log"
  else
    pass "the official binary was never invoked in host build form"
  fi

  local preserved_log="$root/build/bootstrap/stage1/log-stage0-official.txt"
  if [[ -f "$preserved_log" ]] && grep -q 'undefined reference to symbol from libignis_rt.a' "$preserved_log"; then
    pass "the failing official build's log was preserved as log-stage0-official.txt"
  else
    fail_test "log-stage0-official.txt is missing or does not carry the official build's error"
  fi

  assert_fallback_stage0_json "$root" auto

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
  run_stage1 "$root" "$root/bin/ignis-official" "$root/bin/ignis-host" >"$root/run.log" 2>&1 || status=$?

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
  kind="$(json_get "$stage0_json" kind)"
  fallback="$(json_get "$stage0_json" fallback)"

  [[ "$kind" == "official" ]] && pass "stage0.json kind is still official" || fail_test "stage0.json kind is '${kind}', expected official"
  [[ -z "$fallback" ]] && pass "stage0.json carries no fallback" || fail_test "stage0.json fallback is '${fallback}', expected none"

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
  fallback="$(json_get "$stage0_json" fallback)"
  [[ -z "$fallback" ]] && pass "stage0.json carries no fallback" || fail_test "stage0.json fallback is '${fallback}', expected none"

  rm -rf "$root"
}

# Test 4 (the reviewer's case): running `stage1` twice against the same
# official binary must fall back both times. Before write_stage0_fallback
# stopped overwriting `kind`, the first run rewrote it to `host`, so the
# second run's stage0_is_selfhost() misread the still-official IGNIS_STAGE0
# binary as the host and invoked it as `<official binary> build`, which is
# the REGRESSION path write_failing_official_compiler flags above.
test_repeated_stage1_falls_back_every_time() {
  TESTS_RUN=$((TESTS_RUN + 1))
  echo "test: running stage1 twice against the same official binary falls back both times"

  local root
  root="$(make_sandbox)"
  write_failing_official_compiler "$root/bin/ignis-official"
  write_working_host_compiler "$root/bin/ignis-host"
  write_stage0_json "$root" official auto

  local run
  for run in 1 2; do
    local status=0
    run_stage1 "$root" "$root/bin/ignis-official" "$root/bin/ignis-host" >"$root/run-${run}.log" 2>&1 || status=$?

    if [[ "$status" -ne 0 ]]; then
      fail_test "run ${run}: expected stage1 to succeed via fallback, exit ${status}, see ${root}/run-${run}.log"
      sed 's/^/    /' "$root/run-${run}.log"
      continue
    fi
    pass "run ${run}: stage1 exited 0"

    if grep -q 'REGRESSION' "$root/run-${run}.log"; then
      fail_test "run ${run}: the official binary was invoked in host build form, see ${root}/run-${run}.log"
    else
      pass "run ${run}: the official binary was never invoked in host build form"
    fi

    if [[ -x "$root/build/bootstrap/stage1/ignis" ]]; then
      pass "run ${run}: build/bootstrap/stage1/ignis was produced"
    else
      fail_test "run ${run}: build/bootstrap/stage1/ignis is missing"
    fi
  done

  assert_fallback_stage0_json "$root" auto

  rm -rf "$root"
}

# Test 5: the fallback is taken, but the host build also fails -> the whole
# command must exit non-zero rather than silently succeeding or hanging.
test_fallback_and_host_both_fail() {
  TESTS_RUN=$((TESTS_RUN + 1))
  echo "test: fallback taken and the host build also fails exits non-zero"

  local root
  root="$(make_sandbox)"
  write_failing_official_compiler "$root/bin/ignis-official"
  write_failing_host_compiler "$root/bin/ignis-host"
  write_stage0_json "$root" official auto

  local status=0
  run_stage1 "$root" "$root/bin/ignis-official" "$root/bin/ignis-host" >"$root/run.log" 2>&1 || status=$?

  if [[ "$status" -eq 0 ]]; then
    fail_test "expected stage1 to fail (both official and host are broken), it exited 0, see ${root}/run.log"
  else
    pass "stage1 exited non-zero (${status})"
  fi

  if [[ -x "$root/build/bootstrap/stage1/ignis" ]]; then
    fail_test "build/bootstrap/stage1/ignis should not exist when the host build also fails"
  else
    pass "no stage1 binary was produced"
  fi

  if grep -q 'falling back to the host' "$root/run.log"; then
    pass "the fallback was attempted before failing"
  else
    fail_test "no fallback attempt was logged, see ${root}/run.log"
  fi

  rm -rf "$root"
}

test_auto_falls_back_on_official_failure
test_explicit_official_does_not_fall_back
test_host_stage0_is_unaffected
test_repeated_stage1_falls_back_every_time
test_fallback_and_host_both_fail

echo
echo "${TESTS_RUN} test(s) run, ${FAILURES} failure(s)"
[[ "$FAILURES" -eq 0 ]]
