#!/usr/bin/env bash
#
# Resolves stage0 for the bootstrap ladder: the officially promoted selfhost
# binary published on the `nightly` release, or else the C seed committed
# under bootstrap/seed. Writes build/bootstrap/stage0.json for an official
# stage0 in the format scripts/bootstrap.sh reads back
# (`adopt_recorded_stage0`, `current_stage0_identity`,
# `stage0_rebuild_guard_reason`). For the seed it removes stage0.json instead:
# bootstrap.sh with no stage0 recorded builds one from the seed itself and
# records it (`build_stage1_from_seed`).
#
# Shared by nightly.yml's "Resolve stage0" step and ci.yml's "Official stage0
# gate" job, so the download/verify/streak logic is not duplicated between the
# two workflows.
#
# Env:
#   STAGE0_MODE               auto | official (default: auto). Mirrors
#                              nightly's workflow_dispatch `stage0` input.
#   STAGE0_UNAVAILABLE_ACTION What to do when the official asset cannot be
#                              resolved (missing, unverifiable, or below the
#                              promotion-streak threshold):
#                                (unset)  default per STAGE0_MODE: `auto`
#                                         resolves to the C seed (or fails
#                                         when there is no seed either),
#                                         `official` fails outright.
#                                skip     print a notice and exit 2. Used by
#                                         ci.yml's two-step-rule gate, which
#                                         has nothing to check without an
#                                         official asset.
#   GH_TOKEN                   Forwarded to `gh release download`.
#
# Outputs (when $GITHUB_OUTPUT is set):
#   resolved   true if a usable stage0 (official or seed) was resolved, false
#              when it was not (skip, or nothing available).
#   kind       official | seed | "" (when not resolved).
#
# Exit codes: 0 resolved (official or seed); 1 nothing usable (official was
# required and is unavailable, `auto` found neither an official asset nor a
# seed, or an unknown STAGE0_MODE); 2 the official asset is unavailable and
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

mkdir -p build/bootstrap
STAGE0_JSON="build/bootstrap/stage0.json"
SEED_MANIFEST="bootstrap/seed/manifest.json"

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

# No stage0.json is what tells scripts/bootstrap.sh to build stage0 from the
# seed; a stale one from an earlier resolution would be adopted instead.
use_seed_stage0() {
  rm -f "$STAGE0_JSON"
  echo "stage0: the C seed (${SEED_MANIFEST}); scripts/bootstrap.sh builds stage0 from it"
  emit_output resolved true
  emit_output kind seed
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

  if [ ! -f "$SEED_MANIFEST" ]; then
    echo "stage0: no official asset and no C seed at ${SEED_MANIFEST}; nothing can build stage1"
    emit_output resolved false
    emit_output kind ""
    exit 1
  fi

  use_seed_stage0
  exit 0
}

case "$STAGE0_MODE" in
  auto | official) : ;;
  *)
    echo "stage0: unknown STAGE0_MODE '${STAGE0_MODE}' (expected auto or official)" >&2
    emit_output resolved false
    emit_output kind ""
    exit 1
    ;;
esac

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
