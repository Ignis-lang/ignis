#!/usr/bin/env bash
#
# Exercises scripts/build_from_seed.sh, which rebuilds the compiler from the
# committed C seed (bootstrap/seed) with gcc alone.
#
# Each test runs the script in an isolated project root (a temp directory
# with its own scripts/ and seed directory), the same sandbox
# style scripts/tests/test_stage_stamps.sh uses. The seed is a tiny C program
# rather than the real compiler, so a test takes well under a second; what is
# under test is the manifest handling, the checksum checks and the toolchain
# invocation, not the compiler itself.
#
# Wired into ci.yml's selfhost job, next to test_stage_stamps.sh.
#
# Usage: scripts/tests/test_build_from_seed.sh

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(dirname "$SCRIPT_DIR")"
BUILD_FROM_SEED_SH="${REPO_ROOT}/build_from_seed.sh"

FAILURES=0
TESTS_RUN=0

pass() { echo "  ok: $1"; }
fail_test() {
  echo "  FAIL: $1"
  FAILURES=$((FAILURES + 1))
}

# Write a manifest in the shape `scripts/bootstrap.sh seed` writes: a flat
# JSON object, two-space indent, one string value per line.
#
#   $1  seed directory
#   $2  xz sha256 to record
#   $3  c sha256 to record
#   $4  sha256 to record for the bundled header, default: its real hash
write_manifest() {
  local seed_dir="$1" xz_sha256="$2" c_sha256="$3"
  local header_sha256="${4:-$(sha256sum "${seed_dir}/seed_test.h" | cut -d' ' -f1)}"

  cat >"${seed_dir}/manifest.json" <<EOF
{
  "format": "ignis-c-seed v1",
  "source_commit": "0000000000000000000000000000000000000000",
  "sources_hash": "test",
  "produced_by": "scripts/tests/test_build_from_seed.sh",
  "producer_identity": "test",
  "c_file": "selfhost_emit.c",
  "c_sha256": "${c_sha256}",
  "c_size": "0",
  "xz_file": "selfhost_emit.c.xz",
  "xz_sha256": "${xz_sha256}",
  "xz_size": "0",
  "cc": "gcc",
  "cc_version": "test",
  "compile_flags": "-O2",
  "headers": "seed_test.h:${header_sha256}",
  "link_flags": "-O2",
  "libs": "m",
  "created": "1970-01-01T00:00:00Z"
}
EOF
}

# A throwaway project root holding a valid seed for a program that prints
# "seed ok" and exits 0. It includes a header bundled in the seed directory
# and calls into libm, so a recipe that drops `-I <seed dir>` or `-lm` fails
# to build. The sandbox has no std/ at all: the seed must not need one.
make_sandbox() {
  local root seed_dir
  root="$(mktemp -d)"
  seed_dir="${root}/bootstrap/seed"

  mkdir -p "$root/scripts" "$seed_dir"
  cp "$BUILD_FROM_SEED_SH" "$root/scripts/build_from_seed.sh"

  printf '#define SEED_TEST_MESSAGE "seed ok"\n' >"$seed_dir/seed_test.h"

  cat >"${root}/selfhost_emit.c" <<'EOF'
#include <math.h>
#include <stdio.h>
#include "seed_test.h"

int main(int argc, char **argv) {
  volatile double base = (double)argc + 1.0;
  (void)argv;
  printf("%s %d\n", SEED_TEST_MESSAGE, (int)sqrt(base * base));
  return 0;
}
EOF

  xz -9 -c "${root}/selfhost_emit.c" >"${seed_dir}/selfhost_emit.c.xz"

  write_manifest "$seed_dir" \
    "$(sha256sum "${seed_dir}/selfhost_emit.c.xz" | cut -d' ' -f1)" \
    "$(sha256sum "${root}/selfhost_emit.c" | cut -d' ' -f1)"

  echo "$root"
}

test_valid_seed_builds() {
  TESTS_RUN=$((TESTS_RUN + 1))
  echo "test: a valid seed builds a binary and prints only its path"

  local root status=0
  root="$(make_sandbox)"

  (cd "$root" && scripts/build_from_seed.sh -o "$root/out/ignis") >"$root/stdout" 2>"$root/stderr" || status=$?

  if [[ "$status" -eq 0 ]]; then
    pass "exit 0"
  else
    fail_test "exit ${status}, see ${root}/stderr"
  fi

  if [[ "$(cat "$root/stdout")" == "$root/out/ignis" ]]; then
    pass "stdout is the binary path"
  else
    fail_test "stdout was '$(cat "$root/stdout")', expected '$root/out/ignis'"
  fi

  if [[ -x "$root/out/ignis" && "$("$root/out/ignis")" == "seed ok 2" ]]; then
    pass "the binary runs"
  else
    fail_test "the built binary is missing or printed the wrong output"
  fi

  rm -rf "$root"
}

test_verify_only_compiles_nothing() {
  TESTS_RUN=$((TESTS_RUN + 1))
  echo "test: --verify-only checks the seed and builds nothing"

  local root status=0
  root="$(make_sandbox)"

  (cd "$root" && scripts/build_from_seed.sh --verify-only -o "$root/out/ignis") >"$root/stdout" 2>"$root/stderr" || status=$?

  [[ "$status" -eq 0 ]] && pass "exit 0" || fail_test "exit ${status}, see ${root}/stderr"
  [[ ! -e "$root/out/ignis" ]] && pass "no binary written" || fail_test "a binary was written"

  rm -rf "$root"
}

test_xz_sha_mismatch_fails() {
  TESTS_RUN=$((TESTS_RUN + 1))
  echo "test: an archive whose sha256 differs from the manifest fails"

  local root status=0
  root="$(make_sandbox)"

  printf 'tampered' | xz -c >>"$root/bootstrap/seed/selfhost_emit.c.xz"

  (cd "$root" && scripts/build_from_seed.sh -o "$root/out/ignis") >"$root/stdout" 2>"$root/stderr" || status=$?

  [[ "$status" -ne 0 ]] && pass "non-zero exit" || fail_test "exit 0 on a tampered archive"
  grep -q 'sha256 mismatch for .*selfhost_emit.c.xz' "$root/stderr" && pass "the error names the archive" \
    || fail_test "no archive mismatch message, see ${root}/stderr"
  [[ ! -e "$root/out/ignis" ]] && pass "no binary written" || fail_test "a binary was written"

  rm -rf "$root"
}

test_c_sha_mismatch_fails() {
  TESTS_RUN=$((TESTS_RUN + 1))
  echo "test: decompressed C whose sha256 differs from the manifest fails"

  local root status=0
  root="$(make_sandbox)"

  write_manifest "$root/bootstrap/seed" \
    "$(sha256sum "$root/bootstrap/seed/selfhost_emit.c.xz" | cut -d' ' -f1)" \
    "$(printf 'other' | sha256sum | cut -d' ' -f1)"

  (cd "$root" && scripts/build_from_seed.sh -o "$root/out/ignis") >"$root/stdout" 2>"$root/stderr" || status=$?

  [[ "$status" -ne 0 ]] && pass "non-zero exit" || fail_test "exit 0 on a C checksum mismatch"
  grep -q 'sha256 mismatch for the decompressed selfhost_emit.c' "$root/stderr" && pass "the error names the C file" \
    || fail_test "no C mismatch message, see ${root}/stderr"

  rm -rf "$root"
}

test_header_sha_mismatch_fails() {
  TESTS_RUN=$((TESTS_RUN + 1))
  echo "test: a bundled header whose sha256 differs from the manifest fails"

  local root status=0
  root="$(make_sandbox)"

  printf '/* tampered */\n' >>"$root/bootstrap/seed/seed_test.h"

  (cd "$root" && scripts/build_from_seed.sh -o "$root/out/ignis") >"$root/stdout" 2>"$root/stderr" || status=$?

  [[ "$status" -ne 0 ]] && pass "non-zero exit" || fail_test "exit 0 on a tampered header"
  grep -q 'sha256 mismatch for .*seed_test.h' "$root/stderr" && pass "the error names the header" \
    || fail_test "no header mismatch message, see ${root}/stderr"
  [[ ! -e "$root/out/ignis" ]] && pass "no binary written" || fail_test "a binary was written"

  rm -rf "$root"
}

test_missing_manifest_fails() {
  TESTS_RUN=$((TESTS_RUN + 1))
  echo "test: a seed directory without a manifest fails"

  local root status=0
  root="$(make_sandbox)"

  rm "$root/bootstrap/seed/manifest.json"

  (cd "$root" && scripts/build_from_seed.sh -o "$root/out/ignis") >"$root/stdout" 2>"$root/stderr" || status=$?

  [[ "$status" -ne 0 ]] && pass "non-zero exit" || fail_test "exit 0 without a manifest"
  grep -q 'no manifest at' "$root/stderr" && pass "the error names the manifest" \
    || fail_test "no missing-manifest message, see ${root}/stderr"

  rm -rf "$root"
}

test_malformed_manifest_fails() {
  TESTS_RUN=$((TESTS_RUN + 1))
  echo "test: a manifest missing a key or repeating one fails"

  local root status=0
  root="$(make_sandbox)"

  grep -v '"compile_flags"' "$root/bootstrap/seed/manifest.json" >"$root/manifest.tmp"
  mv "$root/manifest.tmp" "$root/bootstrap/seed/manifest.json"

  (cd "$root" && scripts/build_from_seed.sh --verify-only) >"$root/stdout" 2>"$root/stderr" || status=$?

  [[ "$status" -ne 0 ]] && pass "missing key: non-zero exit" || fail_test "exit 0 with a missing key"
  grep -q 'missing key "compile_flags"' "$root/stderr" && pass "missing key: the error names it" \
    || fail_test "no missing-key message, see ${root}/stderr"

  rm -rf "$root"
  root="$(make_sandbox)"
  status=0

  sed -i 's/^  "cc": "gcc",$/  "cc": "gcc",\n  "cc": "clang",/' "$root/bootstrap/seed/manifest.json"

  (cd "$root" && scripts/build_from_seed.sh --verify-only) >"$root/stdout" 2>"$root/stderr" || status=$?

  [[ "$status" -ne 0 ]] && pass "repeated key: non-zero exit" || fail_test "exit 0 with a repeated key"
  grep -q 'key "cc" appears more than once' "$root/stderr" && pass "repeated key: the error names it" \
    || fail_test "no repeated-key message, see ${root}/stderr"

  rm -rf "$root"
}

test_valid_seed_builds
test_verify_only_compiles_nothing
test_xz_sha_mismatch_fails
test_c_sha_mismatch_fails
test_header_sha_mismatch_fails
test_missing_manifest_fails
test_malformed_manifest_fails

echo
echo "${TESTS_RUN} test(s) run, ${FAILURES} failure(s)"
[[ "$FAILURES" -eq 0 ]]
