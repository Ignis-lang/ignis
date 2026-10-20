#!/usr/bin/env bash
# Checks every example under example/ with a compiler, then builds and runs
# each one that is a program: every top-level .ign file that declares `main`,
# and every directory with an ignis.toml. A broken example fails CI here
# instead of waiting for someone to try it.
#
# Usage: scripts/tests/test_examples.sh <compiler>
set -euo pipefail

if [[ $# -ne 1 ]]; then
  echo "usage: $0 <compiler>" >&2
  exit 2
fi

compiler="$(realpath "$1")"
repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
# Single files are named relative to the repository root: an absolute path
# lets the upward search for ignis.toml reach the compiler's own project file,
# which then governs the build instead of the example.
cd "$repo_root"
# Examples are checked against this repository's standard library, not
# whatever IGNIS_STD_PATH the environment happens to carry.
export IGNIS_STD_PATH="$repo_root/std"
scratch="$(mktemp -d)"
trap 'rm -rf "$scratch"' EXIT

# The exit code a program returns on purpose. Every other program exits 0.
expected_exit() {
  case "$1" in
    qbe_benchmark) echo 206 ;;
    *) echo 0 ;;
  esac
}

failures=0

fail() {
  echo "FAIL $1" >&2
  if [[ -f "$2" ]]; then
    sed 's/^/  /' "$2" >&2
  fi
  failures=$((failures + 1))
}

run_program() {
  local name="$1" binary="$2" log="$3"
  local expected status=0
  expected="$(expected_exit "$name")"
  timeout 60 "$binary" < /dev/null > "$log" 2>&1 || status=$?
  if [[ "$status" -ne "$expected" ]]; then
    echo "exit $status, expected $expected" >> "$log"
    fail "$name: run" "$log"
    return
  fi
  echo "ok   $name (exit $status)"
}

for source in example/*.ign; do
  name="$(basename "$source" .ign)"
  log="$scratch/$name.log"

  if ! "$compiler" check --progress plain "$source" > "$log" 2>&1; then
    fail "$name: check" "$log"
    continue
  fi

  if ! grep -q '^function main(' "$source"; then
    echo "ok   $name (check only, no main)"
    continue
  fi

  if ! "$compiler" build --progress plain "$source" -o "$scratch/$name" > "$log" 2>&1; then
    fail "$name: build" "$log"
    continue
  fi

  run_program "$name" "$scratch/$name" "$scratch/$name.run.log"
done

for manifest in example/*/ignis.toml; do
  project="$(dirname "$manifest")"
  name="$(basename "$project")"
  log="$scratch/project-$name.log"

  if ! (cd "$project" && "$compiler" check --progress plain) > "$log" 2>&1; then
    fail "$name: check" "$log"
    continue
  fi

  if ! (cd "$project" && "$compiler" build --progress plain --out-dir "$scratch/out-$name" -o "$scratch/project-$name") > "$log" 2>&1; then
    fail "$name: build" "$log"
    continue
  fi

  run_program "$name" "$scratch/project-$name" "$scratch/project-$name.run.log"
done

if [[ "$failures" -ne 0 ]]; then
  echo "$failures example step(s) failed" >&2
  exit 1
fi

echo "all examples pass"
