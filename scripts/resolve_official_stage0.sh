#!/usr/bin/env bash
#
# Resolves stage0 for the bootstrap ladder: either the officially promoted
# selfhost binary published on the `nightly` release, or the host compiler.
# Writes build/bootstrap/stage0.json in the format `ensure_stage`'s
# `compiler_identity_of` / `stage0_is_selfhost` / `stage0_explicit_official`
# read back (see scripts/bootstrap.sh).
#
# This is the exact resolution nightly.yml's "Resolve stage0" step ran
# inline; it is factored out here so ci.yml's "Official stage0 gate" job can
# run the identical download/verify/streak logic without duplicating the
# YAML. Nightly's behaviour is unchanged: every knob below defaults to
# exactly what that step did before extraction.
#
# Env:
#   STAGE0_MODE               auto | host | official (default: auto). Mirrors
#                              nightly's workflow_dispatch `stage0` input.
#   STAGE0_UNAVAILABLE_ACTION What to do when the official asset cannot be
#                              resolved (missing, unverifiable, or below the
#                              promotion-streak threshold):
#                                (unset)  default per STAGE0_MODE, i.e. the
#                                         original nightly behaviour — `auto`
#                                         falls back to the host, `official`
#                                         fails outright.
#                                skip     print a notice and exit 2, without
#                                         attempting a host fallback. Used by
#                                         ci.yml's two-step-rule gate, which
#                                         has no host build to fall back to.
#   STAGE0_HOST_BIN            Path to the host-built compiler used for the
#                              `host` mode / fallback identity (default:
#                              "${GITHUB_WORKSPACE:-.}/target/ci/ignis").
#   GH_TOKEN                   Forwarded to `gh release download`.
#
# Outputs (when $GITHUB_OUTPUT is set):
#   resolved   true if a usable stage0 (official or host) was written, false
#              on the `skip` path.
#   kind       official | host | "" (on skip).
#
# Exit codes: 0 resolved (official or host); 1 official was required (mode
# official, or STAGE0_UNAVAILABLE_ACTION unset in official mode) and is
# unavailable; 2 the official asset is unavailable and
# STAGE0_UNAVAILABLE_ACTION=skip was requested.
#
# `-e` matters here specifically because this now runs as its own process
# (`scripts/resolve_official_stage0.sh` from the workflow YAML's `run:`)
# rather than inline in a `run:` block, where GitHub Actions' own `bash -e
# {0}` used to cover it: an unexpected failure (a missing jq, a corrupt
# promotion-streak.json) must abort before any `resolved=` output is
# written, not fall through to a stale/empty stage0.json with a false
# success. Every place that intentionally continues past a failing command
# already guards it with `if ! ...` / `||`.
set -euo pipefail

STAGE0_MODE="${STAGE0_MODE:-auto}"
WORKSPACE="${GITHUB_WORKSPACE:-$(pwd)}"

mkdir -p build/bootstrap
STAGE0_JSON="build/bootstrap/stage0.json"

# Same format `ensure_stage`'s `compiler_identity_of` computes and compares
# against (scripts/bootstrap.sh): "sha256:<hex>:<size>", or "unknown" if the
# file cannot be read.
compiler_identity_of() {
  local bin="$1"
  if [ ! -f "$bin" ]; then
    echo "unknown"
    return 0
  fi
  local size hash
  size=$(stat -c '%s' "$bin")
  hash=$(sha256sum "$bin" | cut -d' ' -f1)
  echo "sha256:${hash}:${size}"
}

emit_output() {
  local key="$1" value="$2"
  [ -n "${GITHUB_OUTPUT:-}" ] && echo "${key}=${value}" >>"$GITHUB_OUTPUT"
  return 0
}

write_host_stage0() {
  local host_bin="${STAGE0_HOST_BIN:-${WORKSPACE}/target/ci/ignis}"
  local identity
  identity=$(compiler_identity_of "$host_bin")
  jq -n --arg mode "$STAGE0_MODE" --arg identity "$identity" \
    '{kind: "host", source: "target/ci/ignis", sha256: "", mode: $mode, identity: $identity}' >"$STAGE0_JSON"
  emit_output resolved true
  emit_output kind host
}

# Called when the official asset cannot be resolved. `reason` is logged;
# what happens next depends on STAGE0_UNAVAILABLE_ACTION and STAGE0_MODE, as
# documented above.
unavailable() {
  local reason="$1"
  echo "stage0: ${reason}"

  case "${STAGE0_UNAVAILABLE_ACTION:-}" in
    skip)
      echo "stage0: skipping — no official asset to check"
      emit_output resolved false
      emit_output kind ""
      exit 2
      ;;
  esac

  if [ "$STAGE0_MODE" = "official" ]; then
    echo "stage0: official was forced (STAGE0_MODE=official) but is unavailable"
    emit_output resolved false
    emit_output kind ""
    exit 1
  fi

  echo "stage0: falling back to the host compiler"
  write_host_stage0
  exit 0
}

if [ "$STAGE0_MODE" = "host" ]; then
  echo "stage0: host forced (STAGE0_MODE=host)"
  write_host_stage0
  exit 0
fi

WORKDIR="${RUNNER_TEMP:-$(mktemp -d)}/stage0"
mkdir -p "$WORKDIR"

if ! gh release download nightly \
  --pattern 'ignis-selfhost-linux-amd64' \
  --pattern 'ignis-selfhost-linux-amd64.sha256' \
  --pattern 'promotion-streak.json' \
  --dir "$WORKDIR"; then
  unavailable "the official selfhost asset is not on the nightly release"
fi

if [ ! -f "$WORKDIR/ignis-selfhost-linux-amd64" ] || [ ! -f "$WORKDIR/ignis-selfhost-linux-amd64.sha256" ]; then
  unavailable "the official selfhost asset or its checksum is missing"
fi

if ! (cd "$WORKDIR" && sha256sum -c ignis-selfhost-linux-amd64.sha256); then
  unavailable "the official selfhost asset failed checksum verification"
fi

if [ ! -f "$WORKDIR/promotion-streak.json" ]; then
  unavailable "promotion-streak.json is missing; cannot confirm the asset is official"
fi

STREAK=$(jq -r '.streak // 0' "$WORKDIR/promotion-streak.json")

if [ "$STREAK" -lt 3 ]; then
  unavailable "the promotion streak is ${STREAK}, below the official threshold of 3"
fi

chmod +x "$WORKDIR/ignis-selfhost-linux-amd64"
SHA256=$(cut -d' ' -f1 "$WORKDIR/ignis-selfhost-linux-amd64.sha256")
IDENTITY=$(compiler_identity_of "${WORKDIR}/ignis-selfhost-linux-amd64")

if [ -n "${GITHUB_ENV:-}" ]; then
  echo "IGNIS_STAGE0=${WORKDIR}/ignis-selfhost-linux-amd64" >>"$GITHUB_ENV"
fi

jq -n --arg source "${WORKDIR}/ignis-selfhost-linux-amd64" --arg sha256 "$SHA256" --arg mode "$STAGE0_MODE" --arg identity "$IDENTITY" \
  '{kind: "official", source: $source, sha256: $sha256, mode: $mode, identity: $identity}' >"$STAGE0_JSON"

echo "stage0: official (sha ${SHA256})"
emit_output resolved true
emit_output kind official
