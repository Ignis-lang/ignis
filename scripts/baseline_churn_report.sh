#!/usr/bin/env bash
#
# Report which committed baselines a pull request adds, changes or removes.
#
#   scripts/baseline_churn_report.sh <base-ref> <directory>...
#
# A baseline is a recorded answer, so a diff in one is a semantic change: the
# compiler now schedules a drop somewhere else, or accepts a program it used
# to reject. Regenerating is a single command, which makes it the easy way to
# turn a red gate green, and a reviewer scrolling past several hundred
# generated files will not notice. This prints the churn where the reviewer
# cannot miss it — the job summary — with the one line that says what it means.
#
# Written against directories rather than one gate's path so the G6 and G3
# conversions pass their own directory instead of retyping this.
#
# The report goes to $GITHUB_STEP_SUMMARY when CI sets it, and to stdout
# otherwise. It never fails the job: it reports, the gates decide.

set -euo pipefail

# A whole-corpus regeneration is hundreds of files. Listing every one would
# push the summary towards GitHub's size limit and tell the reviewer nothing
# the counts do not, so past this many the table stops and says so.
MAX_LISTED_FILES=60

if [[ $# -lt 2 ]]; then
  echo "Usage: $(basename "$0") <base-ref> <directory>..." >&2
  exit 2
fi

base_ref="$1"
shift

summary="${GITHUB_STEP_SUMMARY:-/dev/stdout}"

render_no_merge_base() {
  # Markdown backticks, not command substitution.
  # shellcheck disable=SC2016
  printf '### Baseline churn\n\nNo merge base against `%s` — skipping the baseline churn report.\n' "$base_ref"
}

render_unchanged() {
  printf '### Baseline churn\n\nNo committed baselines changed in this pull request.\n'
}

render_changes() {
  local changes="$1"
  local added modified removed listed

  added="$(grep -c '^A' <<<"$changes" || true)"
  modified="$(grep -c '^M' <<<"$changes" || true)"
  removed="$(grep -c '^D' <<<"$changes" || true)"

  printf '### Baseline churn\n\n'
  printf '**A baseline diff is a semantic change.** Each of these files records what\n'
  printf 'the compiler answered for one case. A changed line means the answer changed;\n'
  printf 'read it and agree with it. "Regenerated the baselines" is not a review.\n\n'
  printf '%s added, %s changed, %s removed.\n\n' "$added" "$modified" "$removed"
  printf '| change | file |\n| --- | --- |\n'

  listed=0

  # A rename or copy line is `R100<TAB>old<TAB>new`, two paths rather than
  # one. Read into `path` alone it would report the path that no longer
  # exists, which is the least useful half of the change.
  while IFS=$'\t' read -r status path destination; do
    [[ -n "$status" ]] || continue

    if [[ "$listed" -ge "$MAX_LISTED_FILES" ]]; then
      break
    fi

    local label rendered
    rendered="$path"

    case "$status" in
      A*) label="added" ;;
      M*) label="changed" ;;
      D*) label="removed" ;;
      R*)
        label="renamed"
        rendered="${path} -> ${destination}"
        ;;
      C*)
        label="copied"
        rendered="${path} -> ${destination}"
        ;;
      *) label="$status" ;;
    esac

    # Markdown backticks, not command substitution.
    # shellcheck disable=SC2016
    printf '| %s | `%s` |\n' "$label" "$rendered"
    listed=$((listed + 1))
  done <<<"$changes"

  local total
  total="$(grep -c '' <<<"$changes" || true)"

  if [[ "$total" -gt "$MAX_LISTED_FILES" ]]; then
    printf '\n_%s further files are in the diff._\n' "$((total - MAX_LISTED_FILES))"
  fi
}

# `git merge-base` rather than a plain two-dot diff: a branch that is simply
# behind the base must not report every baseline someone else changed in the
# meantime as its own churn.
if ! merge_base="$(git merge-base "$base_ref" HEAD 2>/dev/null)"; then
  render_no_merge_base >>"$summary"
  exit 0
fi

changes="$(git diff --name-status "$merge_base" HEAD -- "$@")"

if [[ -z "$changes" ]]; then
  render_unchanged >>"$summary"
else
  render_changes "$changes" >>"$summary"
fi
