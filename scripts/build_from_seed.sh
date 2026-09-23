#!/usr/bin/env bash
#
# Rebuild the Ignis compiler from the committed C seed (bootstrap/seed/), with
# nothing but bash, xz, sha256sum and gcc.
#
# The seed is the whole self-hosted compiler, standard library included, as
# the single C translation unit a fixed-point stage2 emits. Compiling it with
# the same recipe the selfhost driver uses for itself (ignis/main.ign's
# compileAndLinkUnits: `gcc -c <c> -o <o> <flags> -I <std>/runtime`, then
# `gcc <flags> <o> -o <bin> -lm`) yields a working compiler, which can then
# serve as stage0 for the bootstrap ladder (`scripts/bootstrap.sh
# stage1-from-seed`). See BOOTSTRAP.md, "C seed (A3)".
#
# Every toolchain flag is read from the seed's manifest.json, the only place
# they are recorded. The manifest is a flat JSON object of string values, one
# key per line, which is what lets this script read it without python or jq.
#
# Usage:
#   scripts/build_from_seed.sh [--seed <dir>] [-o <binary>]
#   scripts/build_from_seed.sh [--seed <dir>] --verify-only
#
#   --seed <dir>    seed directory (default: bootstrap/seed)
#   -o <binary>     output binary (default: build/bootstrap/stage0-seed/ignis)
#   --verify-only   check the manifest and both checksums, compile nothing
#
# On success the binary's path is the only line printed on stdout; progress
# goes to stderr. Any malformed manifest or checksum mismatch exits non-zero.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
SEED_FORMAT="ignis-c-seed v1"

info() { echo "[seed] $*" >&2; }
fail() { echo "[seed] error: $*" >&2; exit 1; }

usage() {
  sed -n '/^# Usage:/,/^# On success/p' "${BASH_SOURCE[0]}" | sed -e 's/^# \{0,1\}//' -e '$d' >&2
}

seed_dir="${PROJECT_ROOT}/bootstrap/seed"
output="${PROJECT_ROOT}/build/bootstrap/stage0-seed/ignis"
verify_only=""

while [[ $# -gt 0 ]]; do
  case "$1" in
    --seed)
      [[ $# -ge 2 ]] || fail "--seed needs a directory"
      seed_dir="$2"
      shift 2
      ;;
    -o)
      [[ $# -ge 2 ]] || fail "-o needs a path"
      output="$2"
      shift 2
      ;;
    --verify-only)
      verify_only="1"
      shift
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      usage
      fail "unknown argument: $1"
      ;;
  esac
done

manifest="${seed_dir}/manifest.json"
[[ -f "$manifest" ]] || fail "no manifest at ${manifest}"

# Print the string value of top-level key $1. The key has to appear exactly
# once, on its own line, as `"key": "value"`, and the value may not contain a
# quote or a backslash: anything else is a malformed manifest, not something
# to guess at.
manifest_get() {
  local key="$1"
  local matches

  matches="$(grep -E "^[[:space:]]*\"${key}\":" "$manifest" || true)"
  [[ -n "$matches" ]] || fail "manifest ${manifest}: missing key \"${key}\""
  [[ "$(wc -l <<<"$matches")" -eq 1 ]] || fail "manifest ${manifest}: key \"${key}\" appears more than once"

  local pattern='^[[:space:]]*"[^"]+":[[:space:]]*"([^"\\]*)",?[[:space:]]*$'
  [[ "$matches" =~ $pattern ]] || fail "manifest ${manifest}: \"${key}\" is not a plain string value"

  echo "${BASH_REMATCH[1]}"
}

require_sha256() {
  local key="$1" value="$2"
  [[ "$value" =~ ^[0-9a-f]{64}$ ]] || fail "manifest ${manifest}: \"${key}\" is not a sha256 hex digest: ${value}"
}

# A file name inside the seed directory, never a path that could leave it.
require_plain_name() {
  local key="$1" value="$2"
  [[ "$value" =~ ^[A-Za-z0-9._-]+$ && "$value" != "." && "$value" != ".." ]] ||
    fail "manifest ${manifest}: \"${key}\" must be a plain file name: ${value}"
}

format="$(manifest_get format)"
[[ "$format" == "$SEED_FORMAT" ]] || fail "manifest ${manifest}: unsupported format \"${format}\" (expected \"${SEED_FORMAT}\")"

xz_file="$(manifest_get xz_file)"
xz_sha256="$(manifest_get xz_sha256)"
c_file="$(manifest_get c_file)"
c_sha256="$(manifest_get c_sha256)"
cc="$(manifest_get cc)"
compile_flags="$(manifest_get compile_flags)"
include_dirs="$(manifest_get include_dirs)"
link_flags="$(manifest_get link_flags)"
libs="$(manifest_get libs)"

require_plain_name xz_file "$xz_file"
require_plain_name c_file "$c_file"
require_sha256 xz_sha256 "$xz_sha256"
require_sha256 c_sha256 "$c_sha256"
[[ -n "$cc" ]] || fail "manifest ${manifest}: \"cc\" is empty"

seed_xz="${seed_dir}/${xz_file}"
[[ -f "$seed_xz" ]] || fail "seed archive not found: ${seed_xz}"

actual_xz_sha256="$(sha256sum "$seed_xz" | cut -d' ' -f1)"
[[ "$actual_xz_sha256" == "$xz_sha256" ]] ||
  fail "sha256 mismatch for ${seed_xz}: manifest records ${xz_sha256}, file is ${actual_xz_sha256}"

work_dir="$(mktemp -d)"
trap 'rm -rf "$work_dir"' EXIT

c_path="${work_dir}/${c_file}"
xz -dc "$seed_xz" >"$c_path" || fail "could not decompress ${seed_xz}"

actual_c_sha256="$(sha256sum "$c_path" | cut -d' ' -f1)"
[[ "$actual_c_sha256" == "$c_sha256" ]] ||
  fail "sha256 mismatch for the decompressed ${c_file}: manifest records ${c_sha256}, content is ${actual_c_sha256}"

info "seed ok: ${seed_xz} (xz ${xz_sha256}, c ${c_sha256})"

if [[ -n "$verify_only" ]]; then
  exit 0
fi

command -v "$cc" >/dev/null 2>&1 || fail "C compiler not found: ${cc}"

read -r -a compile_flag_list <<<"$compile_flags"
read -r -a link_flag_list <<<"$link_flags"
read -r -a lib_list <<<"$libs"

# Include directories are recorded relative to the repository root, the way
# the selfhost driver passes `<std>/runtime`.
include_arguments=()
read -r -a include_dir_list <<<"$include_dirs"
for include_dir in "${include_dir_list[@]}"; do
  [[ -d "${PROJECT_ROOT}/${include_dir}" ]] || fail "include directory not found: ${PROJECT_ROOT}/${include_dir}"
  include_arguments+=(-I "${PROJECT_ROOT}/${include_dir}")
done

lib_arguments=()
for lib in "${lib_list[@]}"; do
  lib_arguments+=("-l${lib}")
done

object_path="${work_dir}/${c_file%.c}.o"

info "compiling ${c_file} with ${cc} ${compile_flags}"
"$cc" -c "$c_path" -o "$object_path" "${compile_flag_list[@]}" "${include_arguments[@]}" ||
  fail "${cc} could not compile the seed"

mkdir -p "$(dirname "$output")"

info "linking ${output}"
"$cc" "${link_flag_list[@]}" "$object_path" -o "$output" "${lib_arguments[@]}" ||
  fail "${cc} could not link the seed"

[[ -x "$output" ]] || fail "no binary produced at ${output}"

readlink -f "$output"
