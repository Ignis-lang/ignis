#!/usr/bin/env bash
#
# Exercises how scripts/bootstrap.sh picks and runs stage0, and how
# scripts/resolve_official_stage0.sh resolves it, without running a real
# self-compilation. Fake compilers stand in for the official selfhost binary:
# one that always fails, the way a published binary fails once ignis/ or std/
# uses something it cannot build yet, and one that succeeds.
#
# stage0 is the promoted official selfhost binary or a compiler built from the
# C seed, never the Rust host, and a stage0 that cannot build stage1 is
# terminal. A fake `ignis` on PATH records every invocation, so a test can
# prove the ladder never reaches for whatever compiler happens to be there.
#
# Each test runs scripts/bootstrap.sh in an isolated project root (a temp
# directory with its own build/bootstrap), so nothing here touches the real
# repository's build/ directory. Wired into ci.yml's selfhost job — it takes
# a few seconds.
#
# The last test, test_promotion_decide, covers the nightly's "Publish the
# promotion state" step's streak/publish decision (`scripts/bootstrap.sh
# promotion-decide`). It is a pure function with no filesystem or gh/release
# side effects, so it runs straight against the real scripts/bootstrap.sh
# rather than a sandbox copy.
#
# Usage: scripts/tests/test_stage0.sh

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(dirname "$SCRIPT_DIR")"
BOOTSTRAP_SH="${REPO_ROOT}/bootstrap.sh"
MEASURE_RUN_PY="${REPO_ROOT}/measure_run.py"
RESOLVE_STAGE0_SH="${REPO_ROOT}/resolve_official_stage0.sh"

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

# A throwaway project root with just enough shape for
# `scripts/bootstrap.sh stage1` to run: its own scripts/, ignis/main.ign
# (never actually read by the fake compilers), bin/ for the fake compilers,
# and a fake `ignis` in bin/path/ that logs any call to path-ignis.log.
make_sandbox() {
  local root
  root="$(mktemp -d)"

  mkdir -p "$root/scripts" "$root/ignis" "$root/bin/path" "$root/build/bootstrap"
  cp "$BOOTSTRAP_SH" "$root/scripts/bootstrap.sh"
  cp "$MEASURE_RUN_PY" "$root/scripts/measure_run.py"
  cp "$RESOLVE_STAGE0_SH" "$root/scripts/resolve_official_stage0.sh"
  chmod +x "$root/scripts/resolve_official_stage0.sh"
  printf 'function main(): i32 { return 0; }\n' >"$root/ignis/main.ign"

  cat >"$root/bin/path/ignis" <<EOF
#!/usr/bin/env bash
echo "\$*" >>"${root}/path-ignis.log"
exit 1
EOF
  chmod +x "$root/bin/path/ignis"

  echo "$root"
}

assert_path_ignis_unused() {
  local root="$1"

  if [[ -f "$root/path-ignis.log" ]]; then
    fail_test "the \`ignis\` on PATH was invoked ($(tr '\n' ';' <"$root/path-ignis.log"))"
  else
    pass "the \`ignis\` on PATH was never invoked"
  fi
}

# A selfhost-shaped compiler invoked as every other stage is: `compiler
# entry -o output`. Always fails, with an error line compile_stage's log
# will carry through to first_error_line. Any other invocation shape (the
# old host `<bin> build` form) fails with a REGRESSION marker instead.
write_failing_official_compiler() {
  local path="$1"
  cat >"$path" <<'EOF'
#!/usr/bin/env bash
if [[ "${1-}" == "build" ]]; then
  echo "error: REGRESSION stage0 was invoked in host build form" >&2
  exit 1
fi
echo "error: undefined reference to symbol from libignis_rt.a" >&2
exit 1
EOF
  chmod +x "$path"
}

# A selfhost-shaped compiler that succeeds: writes a working stub binary at
# the requested `-o` path. It also answers `--version` with exit 0, which is
# what the retired `--version` probe read as "the host": a stage0 is always
# run in the selfhost form now, whatever `--version` does.
write_working_official_compiler() {
  local path="$1"
  cat >"$path" <<'EOF'
#!/usr/bin/env bash
if [[ "${1-}" == "--version" ]]; then
  echo "ignis 0.0.0"
  exit 0
fi
if [[ "${1-}" == "build" ]]; then
  echo "error: REGRESSION stage0 was invoked in host build form" >&2
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
  echo "error: no -o output given" >&2
  exit 1
fi
printf '#!/usr/bin/env bash\nexit 0\n' >"$out"
chmod +x "$out"
exit 0
EOF
  chmod +x "$path"
}

# Stands in for `gh` when scripts/resolve_official_stage0.sh needs the
# download to fail the way it would with no official asset published yet
# (a fresh fork, or a promotion streak that has never reached 3).
write_failing_gh() {
  local path="$1"
  cat >"$path" <<'EOF'
#!/usr/bin/env bash
echo "error: release not found" >&2
exit 1
EOF
  chmod +x "$path"
}

# Stands in for `gh` succeeding at the download (asset, checksum, and streak
# file all present) but with a corrupt promotion-streak.json — an
# *unexpected* failure (a bug, a truncated download) rather than "no asset
# published", which resolve_official_stage0.sh's `unavailable()` path is
# for. Proves `set -e` is in effect: without it, `STREAK=$(jq ...)` failing
# would silently continue with $STREAK empty rather than aborting.
write_gh_with_corrupt_streak() {
  local path="$1"
  cat >"$path" <<'EOF'
#!/usr/bin/env bash
dir=""
prev=""
for arg in "$@"; do
  if [[ "$prev" == "--dir" ]]; then
    dir="$arg"
  fi
  prev="$arg"
done
mkdir -p "$dir"
printf '#!/usr/bin/env bash\nexit 0\n' >"$dir/ignis-selfhost-linux-amd64"
sha256sum "$dir/ignis-selfhost-linux-amd64" >"$dir/ignis-selfhost-linux-amd64.sha256"
echo "this is not valid json" >"$dir/promotion-streak.json"
exit 0
EOF
  chmod +x "$path"
}

write_stage0_json() {
  local root="$1" kind="$2" mode="$3" source="${4:-official}"
  printf '{"kind": "%s", "source": "%s", "sha256": "deadbeef", "mode": "%s"}\n' \
    "$kind" "$source" "$mode" >"$root/build/bootstrap/stage0.json"
}

# Runs `scripts/bootstrap.sh stage1` with the sandbox's fake `ignis` first on
# PATH. $2 is IGNIS_STAGE0; bootstrap.sh reads an empty one as unset.
run_stage1() {
  local root="$1" stage0_bin="$2"
  (
    cd "$root"
    PATH="$root/bin/path:$PATH" IGNIS_STAGE0="$stage0_bin" scripts/bootstrap.sh stage1
  )
}

# A failing official stage0 is terminal whether `auto` resolved it or
# `stage0=official` forced it: stage1 fails, the message names the two-step
# rule and the first compiler error, and no other compiler is tried.
run_official_failure_is_terminal_case() {
  local mode="$1"

  local root
  root="$(make_sandbox)"
  write_failing_official_compiler "$root/bin/ignis-official"
  write_stage0_json "$root" official "$mode"

  local status=0
  run_stage1 "$root" "$root/bin/ignis-official" >"$root/run.log" 2>&1 || status=$?

  if [[ "$status" -eq 0 ]]; then
    fail_test "mode=${mode}: expected stage1 to fail, it exited 0, see ${root}/run.log"
  else
    pass "mode=${mode}: stage1 exited non-zero (${status})"
  fi

  if [[ -x "$root/build/bootstrap/stage1/ignis" ]]; then
    fail_test "mode=${mode}: build/bootstrap/stage1/ignis should not exist"
  else
    pass "mode=${mode}: no stage1 binary was produced"
  fi

  if grep -qi 'falling back' "$root/run.log"; then
    fail_test "mode=${mode}: a fallback was attempted, see ${root}/run.log"
  else
    pass "mode=${mode}: no fallback was attempted"
  fi

  if grep -q 'REGRESSION' "$root/run.log"; then
    fail_test "mode=${mode}: stage0 was invoked in host build form, see ${root}/run.log"
  else
    pass "mode=${mode}: stage0 was only invoked in the selfhost form"
  fi

  if grep -q 'two-step rule' "$root/run.log"; then
    pass "mode=${mode}: the failure names the two-step rule"
  else
    fail_test "mode=${mode}: the failure message does not mention the two-step rule, see ${root}/run.log"
  fi

  if grep -q 'stage0-break-approved' "$root/run.log"; then
    pass "mode=${mode}: the failure names the override label"
  else
    fail_test "mode=${mode}: the failure message does not name the stage0-break-approved override, see ${root}/run.log"
  fi

  if grep -q 'undefined reference to symbol from libignis_rt.a' "$root/run.log"; then
    pass "mode=${mode}: the failure points at the first compiler error"
  else
    fail_test "mode=${mode}: the failure message does not include the first compiler error, see ${root}/run.log"
  fi

  local stage0_json="$root/build/bootstrap/stage0.json"
  local kind fallback used_kind
  kind="$(json_get "$stage0_json" kind)"
  fallback="$(json_get "$stage0_json" fallback)"
  used_kind="$(json_get "$stage0_json" used_kind)"

  [[ "$kind" == "official" ]] && pass "mode=${mode}: stage0.json kind is still official" \
    || fail_test "mode=${mode}: stage0.json kind is '${kind}', expected official"
  [[ -z "$fallback" && -z "$used_kind" ]] && pass "mode=${mode}: stage0.json records no fallback" \
    || fail_test "mode=${mode}: stage0.json records fallback='${fallback}' used_kind='${used_kind}'"

  assert_path_ignis_unused "$root"

  rm -rf "$root"
}

test_auto_official_failure_is_terminal() {
  TESTS_RUN=$((TESTS_RUN + 1))
  echo "test: an auto-resolved official stage0 that fails is terminal"
  run_official_failure_is_terminal_case auto
}

test_forced_official_failure_is_terminal() {
  TESTS_RUN=$((TESTS_RUN + 1))
  echo "test: a forced stage0=official that fails is terminal"
  run_official_failure_is_terminal_case official
}

# Running stage1 again against the same failing official binary fails the
# same way: nothing the first run wrote turns the second one into a fallback.
test_repeated_official_failure_stays_terminal() {
  TESTS_RUN=$((TESTS_RUN + 1))
  echo "test: running stage1 twice against a failing official stage0 fails both times"

  local root
  root="$(make_sandbox)"
  write_failing_official_compiler "$root/bin/ignis-official"
  write_stage0_json "$root" official auto

  local run
  for run in 1 2; do
    local status=0
    run_stage1 "$root" "$root/bin/ignis-official" >"$root/run-${run}.log" 2>&1 || status=$?

    if [[ "$status" -eq 0 ]]; then
      fail_test "run ${run}: expected stage1 to fail, it exited 0, see ${root}/run-${run}.log"
    else
      pass "run ${run}: stage1 exited non-zero (${status})"
    fi

    if grep -q 'two-step rule' "$root/run-${run}.log"; then
      pass "run ${run}: the failure names the two-step rule"
    else
      fail_test "run ${run}: the failure message does not mention the two-step rule, see ${root}/run-${run}.log"
    fi
  done

  assert_path_ignis_unused "$root"

  rm -rf "$root"
}

test_working_official_builds_stage1() {
  TESTS_RUN=$((TESTS_RUN + 1))
  echo "test: a working official stage0 builds stage1"

  local root
  root="$(make_sandbox)"
  write_working_official_compiler "$root/bin/ignis-official"
  write_stage0_json "$root" official auto

  local status=0
  run_stage1 "$root" "$root/bin/ignis-official" >"$root/run.log" 2>&1 || status=$?

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

  assert_path_ignis_unused "$root"

  rm -rf "$root"
}

# An explicit IGNIS_STAGE0 with no stage0.json is run in the selfhost form,
# even when it answers `--version` the way the retired probe read as "host".
test_explicit_stage0_runs_in_selfhost_form() {
  TESTS_RUN=$((TESTS_RUN + 1))
  echo "test: an explicit IGNIS_STAGE0 without stage0.json runs in the selfhost form"

  local root
  root="$(make_sandbox)"
  write_working_official_compiler "$root/bin/ignis-dev"
  rm -f "$root/build/bootstrap/stage0.json"

  local status=0
  run_stage1 "$root" "$root/bin/ignis-dev" >"$root/run.log" 2>&1 || status=$?

  if [[ "$status" -eq 0 ]]; then
    pass "stage1 exited 0"
  else
    fail_test "expected stage1 to succeed, exit ${status}, see ${root}/run.log"
    sed 's/^/    /' "$root/run.log"
  fi

  if grep -q 'REGRESSION' "$root/run.log"; then
    fail_test "stage0 was invoked in host build form, see ${root}/run.log"
  else
    pass "stage0 was only invoked in the selfhost form"
  fi

  [[ -x "$root/build/bootstrap/stage1/ignis" ]] && pass "build/bootstrap/stage1/ignis was produced" \
    || fail_test "build/bootstrap/stage1/ignis is missing"

  rm -rf "$root"
}

# A failing explicit IGNIS_STAGE0 of no recorded kind is terminal too.
test_explicit_stage0_failure_is_terminal() {
  TESTS_RUN=$((TESTS_RUN + 1))
  echo "test: a failing explicit IGNIS_STAGE0 is terminal"

  local root
  root="$(make_sandbox)"
  write_failing_official_compiler "$root/bin/ignis-dev"
  rm -f "$root/build/bootstrap/stage0.json"

  local status=0
  run_stage1 "$root" "$root/bin/ignis-dev" >"$root/run.log" 2>&1 || status=$?

  [[ "$status" -ne 0 ]] && pass "stage1 exited non-zero (${status})" \
    || fail_test "expected stage1 to fail, it exited 0, see ${root}/run.log"

  if grep -q 'undefined reference to symbol from libignis_rt.a' "$root/run.log"; then
    pass "the failure points at the first compiler error"
  else
    fail_test "the failure message does not include the first compiler error, see ${root}/run.log"
  fi

  assert_path_ignis_unused "$root"

  rm -rf "$root"
}

# With neither IGNIS_STAGE0 nor a stage0.json, stage0 comes from the C seed,
# never from `ignis` on PATH. The sandbox has no seed, so the run stops at the
# seed step, which is enough to show where the default leads.
test_default_stage0_is_the_seed() {
  TESTS_RUN=$((TESTS_RUN + 1))
  echo "test: with nothing resolved, stage0 is built from the C seed, not taken from PATH"

  local root
  root="$(make_sandbox)"
  rm -f "$root/build/bootstrap/stage0.json"

  local status=0
  run_stage1 "$root" "" >"$root/run.log" 2>&1 || status=$?

  [[ "$status" -ne 0 ]] && pass "stage1 exited non-zero without a seed (${status})" \
    || fail_test "expected stage1 to fail without a seed, it exited 0, see ${root}/run.log"

  if grep -q 'stage1-from-seed: no seed at' "$root/run.log"; then
    pass "stage1 went to the C seed"
  else
    fail_test "stage1 did not go to the C seed, see ${root}/run.log"
    sed 's/^/    /' "$root/run.log"
  fi

  assert_path_ignis_unused "$root"

  rm -rf "$root"
}

# stage0.json recording an official stage0 whose binary is here is adopted
# with IGNIS_STAGE0 unset, which is how a local `resolve_official_stage0.sh`
# followed by `bootstrap.sh all` runs.
test_recorded_official_stage0_is_adopted() {
  TESTS_RUN=$((TESTS_RUN + 1))
  echo "test: a recorded official stage0 whose binary exists is adopted without IGNIS_STAGE0"

  local root
  root="$(make_sandbox)"
  write_working_official_compiler "$root/bin/ignis-official"
  write_stage0_json "$root" official auto "$root/bin/ignis-official"

  local status=0
  run_stage1 "$root" "" >"$root/run.log" 2>&1 || status=$?

  if [[ "$status" -eq 0 ]]; then
    pass "stage1 exited 0"
  else
    fail_test "expected stage1 to succeed, exit ${status}, see ${root}/run.log"
    sed 's/^/    /' "$root/run.log"
  fi

  if grep -q "ignis-official" "$root/build/bootstrap/stage1/log.txt" 2>/dev/null || grep -q "ignis-official" "$root/run.log"; then
    pass "stage1 was built with the recorded official binary"
  else
    fail_test "stage1 was not built with the recorded official binary, see ${root}/run.log"
  fi

  assert_path_ignis_unused "$root"

  rm -rf "$root"
}

# With no official asset published (fresh fork, or a promotion streak that
# never reached 3), the two-step-rule gate's resolution skips rather than
# failing the PR: there is nothing to check yet.
test_no_official_asset_skips() {
  TESTS_RUN=$((TESTS_RUN + 1))
  echo "test: resolve_official_stage0.sh skips when no official asset is published"

  local root
  root="$(make_sandbox)"
  write_failing_gh "$root/bin/gh"

  local status=0
  (
    cd "$root"
    PATH="$root/bin:$PATH" \
      STAGE0_MODE=official \
      STAGE0_UNAVAILABLE_ACTION=skip \
      GH_TOKEN=fake \
      scripts/resolve_official_stage0.sh
  ) >"$root/run.log" 2>&1 || status=$?

  [[ "$status" -eq 2 ]] && pass "resolve_official_stage0.sh exits 2 on the skip path" \
    || fail_test "expected exit 2, got ${status}, see ${root}/run.log"

  if grep -q 'skipping' "$root/run.log"; then
    pass "the skip was logged"
  else
    fail_test "no skip notice, see ${root}/run.log"
  fi

  if [[ -f "$root/build/bootstrap/stage0.json" ]]; then
    fail_test "stage0.json should not be written on the skip path"
  else
    pass "no stage0.json was written on the skip path"
  fi

  rm -rf "$root"
}

# `auto` with no official asset resolves to the C seed: exit 0, kind=seed,
# and no stage0.json left behind for bootstrap.sh to adopt, so its own
# default builds stage0 from the seed. A stale stage0.json from an earlier
# resolution is removed rather than trusted.
test_auto_without_official_resolves_to_seed() {
  TESTS_RUN=$((TESTS_RUN + 1))
  echo "test: resolve_official_stage0.sh auto falls to the C seed when no official asset is published"

  local root
  root="$(make_sandbox)"
  write_failing_gh "$root/bin/gh"
  mkdir -p "$root/bootstrap/seed"
  printf '{}\n' >"$root/bootstrap/seed/manifest.json"
  write_stage0_json "$root" official auto "$root/bin/ignis-stale"

  local status=0
  local github_output="$root/github_output.txt"
  : >"$github_output"
  (
    cd "$root"
    PATH="$root/bin:$PATH" \
      STAGE0_MODE=auto \
      GH_TOKEN=fake \
      GITHUB_OUTPUT="$github_output" \
      scripts/resolve_official_stage0.sh
  ) >"$root/run.log" 2>&1 || status=$?

  [[ "$status" -eq 0 ]] && pass "resolve_official_stage0.sh exits 0" \
    || fail_test "expected exit 0, got ${status}, see ${root}/run.log"

  grep -q '^resolved=true$' "$github_output" && pass "resolved=true" \
    || fail_test "no resolved=true output, see ${github_output}"
  grep -q '^kind=seed$' "$github_output" && pass "kind=seed" \
    || fail_test "no kind=seed output, see ${github_output}"

  if [[ -f "$root/build/bootstrap/stage0.json" ]]; then
    fail_test "stage0.json should be removed so bootstrap.sh builds stage0 from the seed"
  else
    pass "no stage0.json is left behind"
  fi

  if grep -qiw 'host' "$root/run.log"; then
    fail_test "the resolution mentions the host, see ${root}/run.log"
  else
    pass "the resolution never mentions the host"
  fi

  rm -rf "$root"
}

# `auto` with neither an official asset nor a committed seed has nothing to
# build stage1 with, and says so.
test_auto_without_official_or_seed_fails() {
  TESTS_RUN=$((TESTS_RUN + 1))
  echo "test: resolve_official_stage0.sh auto fails with neither an official asset nor a seed"

  local root
  root="$(make_sandbox)"
  write_failing_gh "$root/bin/gh"

  local status=0
  local github_output="$root/github_output.txt"
  : >"$github_output"
  (
    cd "$root"
    PATH="$root/bin:$PATH" \
      STAGE0_MODE=auto \
      GH_TOKEN=fake \
      GITHUB_OUTPUT="$github_output" \
      scripts/resolve_official_stage0.sh
  ) >"$root/run.log" 2>&1 || status=$?

  [[ "$status" -eq 1 ]] && pass "resolve_official_stage0.sh exits 1" \
    || fail_test "expected exit 1, got ${status}, see ${root}/run.log"
  grep -q '^resolved=false$' "$github_output" && pass "resolved=false" \
    || fail_test "no resolved=false output, see ${github_output}"

  rm -rf "$root"
}

# A forced `official` with no asset fails outright instead of using the seed.
test_forced_official_without_asset_fails() {
  TESTS_RUN=$((TESTS_RUN + 1))
  echo "test: resolve_official_stage0.sh official fails when no official asset is published"

  local root
  root="$(make_sandbox)"
  write_failing_gh "$root/bin/gh"
  mkdir -p "$root/bootstrap/seed"
  printf '{}\n' >"$root/bootstrap/seed/manifest.json"

  local status=0
  (
    cd "$root"
    PATH="$root/bin:$PATH" \
      STAGE0_MODE=official \
      GH_TOKEN=fake \
      scripts/resolve_official_stage0.sh
  ) >"$root/run.log" 2>&1 || status=$?

  [[ "$status" -eq 1 ]] && pass "resolve_official_stage0.sh exits 1" \
    || fail_test "expected exit 1, got ${status}, see ${root}/run.log"

  rm -rf "$root"
}

# `host` is no longer a stage0 mode.
test_host_mode_is_rejected() {
  TESTS_RUN=$((TESTS_RUN + 1))
  echo "test: resolve_official_stage0.sh rejects STAGE0_MODE=host"

  local root
  root="$(make_sandbox)"
  write_failing_gh "$root/bin/gh"

  local status=0
  (
    cd "$root"
    PATH="$root/bin:$PATH" \
      STAGE0_MODE=host \
      GH_TOKEN=fake \
      scripts/resolve_official_stage0.sh
  ) >"$root/run.log" 2>&1 || status=$?

  [[ "$status" -eq 1 ]] && pass "resolve_official_stage0.sh exits 1" \
    || fail_test "expected exit 1, got ${status}, see ${root}/run.log"

  if grep -q 'unknown STAGE0_MODE' "$root/run.log"; then
    pass "the rejection names the unknown mode"
  else
    fail_test "no unknown-mode message, see ${root}/run.log"
  fi

  [[ ! -f "$root/build/bootstrap/stage0.json" ]] && pass "no stage0.json was written" \
    || fail_test "stage0.json was written for STAGE0_MODE=host"

  rm -rf "$root"
}

# An unexpected failure inside resolve_official_stage0.sh (here, a corrupt
# promotion-streak.json after a successful download) must abort under
# `set -e` before any `resolved=` output is written — ci.yml's gate treats an
# empty `resolved` output as an unexpected error (fails the job) and only a
# `resolved=false` line as the legitimate "no asset published" skip.
test_unexpected_failure_emits_no_resolved_output() {
  TESTS_RUN=$((TESTS_RUN + 1))
  echo "test: an unexpected resolve_official_stage0.sh failure emits no resolved= output"

  local root
  root="$(make_sandbox)"
  write_gh_with_corrupt_streak "$root/bin/gh"

  local status=0
  local github_output="$root/github_output.txt"
  : >"$github_output"
  (
    cd "$root"
    PATH="$root/bin:$PATH" \
      STAGE0_MODE=official \
      STAGE0_UNAVAILABLE_ACTION=skip \
      GH_TOKEN=fake \
      GITHUB_OUTPUT="$github_output" \
      scripts/resolve_official_stage0.sh
  ) >"$root/run.log" 2>&1 || status=$?

  if [[ "$status" -eq 0 || "$status" -eq 2 ]]; then
    fail_test "expected an unexpected-failure exit status (neither 0 nor the skip code 2), got ${status}, see ${root}/run.log"
  else
    pass "resolve_official_stage0.sh exited non-zero and not the skip code (${status})"
  fi

  if grep -q '^resolved=' "$github_output"; then
    fail_test "resolved= was written despite the unexpected failure, see ${github_output}"
  else
    pass "no resolved= output was written"
  fi

  rm -rf "$root"
}

# `scripts/bootstrap.sh promotion-decide` is the nightly's "Publish the
# promotion state" step's decision logic (see the "Promotion flow" comment in
# .github/workflows/nightly.yml), factored into a pure function so it can be
# exercised directly here without a gh/release round trip.
test_promotion_decide() {
  TESTS_RUN=$((TESTS_RUN + 1))
  echo "test: promotion-decide counts the streak and publishes at 3"

  local out

  out="$("$BOOTSTRAP_SH" promotion-decide true 2)"
  [[ "$(jq -r '.streak' <<<"$out")" == "3" ]] && pass "candidate: streak increments to 3" \
    || fail_test "candidate: streak is $(jq -r '.streak' <<<"$out"), expected 3"
  [[ "$(jq -r '.write_streak' <<<"$out")" == "true" ]] && pass "candidate: writes the streak file" \
    || fail_test "candidate: write_streak is $(jq -r '.write_streak' <<<"$out"), expected true"
  [[ "$(jq -r '.publish_binary' <<<"$out")" == "true" ]] && pass "candidate: publishes at streak 3" \
    || fail_test "candidate: publish_binary is $(jq -r '.publish_binary' <<<"$out"), expected true"
  [[ "$(jq -r 'has("reseed")' <<<"$out")" == "false" ]] && pass "candidate: no reseed field" \
    || fail_test "candidate: the decision still carries reseed: ${out}"

  out="$("$BOOTSTRAP_SH" promotion-decide true 1)"
  [[ "$(jq -r '.streak' <<<"$out")" == "2" ]] && pass "candidate: streak increments to 2" \
    || fail_test "candidate: streak is $(jq -r '.streak' <<<"$out"), expected 2"
  [[ "$(jq -r '.publish_binary' <<<"$out")" == "false" ]] && pass "candidate: does not publish below streak 3" \
    || fail_test "candidate: publish_binary is $(jq -r '.publish_binary' <<<"$out"), expected false"

  out="$("$BOOTSTRAP_SH" promotion-decide true)"
  [[ "$(jq -r '.streak' <<<"$out")" == "1" ]] && pass "candidate: a missing previous streak counts from 0" \
    || fail_test "candidate: streak is $(jq -r '.streak' <<<"$out"), expected 1"

  out="$("$BOOTSTRAP_SH" promotion-decide false 5)"
  [[ "$(jq -r '.streak' <<<"$out")" == "0" ]] && pass "not candidate: streak resets to 0" \
    || fail_test "not candidate: streak is $(jq -r '.streak' <<<"$out"), expected 0"
  [[ "$(jq -r '.write_streak' <<<"$out")" == "true" ]] && pass "not candidate: still writes the reset streak file" \
    || fail_test "not candidate: write_streak is $(jq -r '.write_streak' <<<"$out"), expected true"
  [[ "$(jq -r '.publish_binary' <<<"$out")" == "false" ]] && pass "not candidate: publishes nothing" \
    || fail_test "not candidate: publish_binary is $(jq -r '.publish_binary' <<<"$out"), expected false"
}

# A seed-built stage0 that cannot build stage1 says the seed needs a refresh,
# not that the two-step rule was broken.
test_seed_stage0_failure_asks_for_a_seed_refresh() {
  TESTS_RUN=$((TESTS_RUN + 1))
  echo "test: a failing seed stage0 asks for a seed refresh"

  local root
  root="$(make_sandbox)"
  write_failing_official_compiler "$root/bin/ignis-seed"
  write_stage0_json "$root" seed seed "$root/bin/ignis-seed"

  local status=0
  run_stage1 "$root" "" >"$root/run.log" 2>&1 || status=$?

  [[ "$status" -ne 0 ]] && pass "stage1 exited non-zero (${status})" \
    || fail_test "expected stage1 to fail, it exited 0, see ${root}/run.log"

  if grep -q 'the compiler built from the C seed reported errors' "$root/run.log" \
    && grep -q 'refresh the seed (scripts/bootstrap.sh seed' "$root/run.log"; then
    pass "the failure asks for a seed refresh"
  else
    fail_test "the failure does not ask for a seed refresh, see ${root}/run.log"
  fi

  if grep -q 'two-step rule' "$root/run.log"; then
    fail_test "a seed failure was reported as a two-step rule violation, see ${root}/run.log"
  else
    pass "the failure does not name the two-step rule"
  fi

  assert_path_ignis_unused "$root"

  rm -rf "$root"
}

# A stage0.json of kind selfhost (written by hand for a developer's own
# selfhost binary) is adopted like official and seed.
test_recorded_selfhost_stage0_is_adopted() {
  TESTS_RUN=$((TESTS_RUN + 1))
  echo "test: a recorded selfhost stage0 whose binary exists is adopted without IGNIS_STAGE0"

  local root
  root="$(make_sandbox)"
  write_working_official_compiler "$root/bin/ignis-dev"
  write_stage0_json "$root" selfhost manual "$root/bin/ignis-dev"

  local status=0
  run_stage1 "$root" "" >"$root/run.log" 2>&1 || status=$?

  [[ "$status" -eq 0 ]] && pass "stage1 exited 0" \
    || fail_test "expected stage1 to succeed, exit ${status}, see ${root}/run.log"

  if grep -q "with stage0 ${root}/bin/ignis-dev" "$root/run.log"; then
    pass "stage1 was built with the recorded selfhost binary"
  else
    fail_test "stage1 was not built with the recorded selfhost binary, see ${root}/run.log"
  fi

  rm -rf "$root"
}

# promotion-decide takes <candidate> [previous-streak]. The old three-argument
# form (<candidate> <fallback> <streak>) must fail instead of reading the
# fallback flag as the streak.
test_promotion_decide_rejects_bad_arguments() {
  TESTS_RUN=$((TESTS_RUN + 1))
  echo "test: promotion-decide rejects a wrong argument count or shape"

  local label status out
  for label in "old-three-args:true false 2" "no-args:" "streak-not-a-number:true false" "candidate-not-a-bool:yes 2"; do
    local name="${label%%:*}" arguments="${label#*:}"
    status=0
    # shellcheck disable=SC2086 # word splitting is the point: each case is an argument list.
    out="$("$BOOTSTRAP_SH" promotion-decide $arguments 2>&1)" || status=$?

    if [[ "$status" -eq 2 ]]; then
      pass "${name}: exit 2"
    else
      fail_test "${name}: expected exit 2, got ${status} (${out})"
    fi

    if grep -q 'promotion-decide <candidate> \[previous-streak\]' <<<"$out"; then
      pass "${name}: prints the usage"
    else
      fail_test "${name}: no usage line (${out})"
    fi
  done
}

test_auto_official_failure_is_terminal
test_forced_official_failure_is_terminal
test_repeated_official_failure_stays_terminal
test_working_official_builds_stage1
test_explicit_stage0_runs_in_selfhost_form
test_explicit_stage0_failure_is_terminal
test_default_stage0_is_the_seed
test_recorded_official_stage0_is_adopted
test_no_official_asset_skips
test_auto_without_official_resolves_to_seed
test_auto_without_official_or_seed_fails
test_forced_official_without_asset_fails
test_host_mode_is_rejected
test_unexpected_failure_emits_no_resolved_output
test_promotion_decide
test_seed_stage0_failure_asks_for_a_seed_refresh
test_recorded_selfhost_stage0_is_adopted
test_promotion_decide_rejects_bad_arguments

echo
echo "${TESTS_RUN} test(s) run, ${FAILURES} failure(s)"
[[ "$FAILURES" -eq 0 ]]
