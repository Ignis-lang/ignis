#!/usr/bin/env bash
#
# Exercises scripts/baseline_churn_report.sh against a throwaway repository.
#
# The report is the only place a reviewer is told that a regenerated baseline
# is a semantic change, so the cases that matter are: it names every kind of
# change, it says nothing when nothing changed, it compares against the merge
# base rather than the base branch tip (a branch that is merely behind must
# not claim someone else's baselines as its own churn), it shows both halves
# of a rename, and it never fails the job that runs it.
#
# Hermetic on purpose. The script under test appends to $GITHUB_STEP_SUMMARY
# whenever that is set, so a test that captured its stdout passed locally and
# failed on a CI runner, where the variable is already exported: every report
# went into the real job summary and the capture came back empty. Every
# invocation below is given its own summary file, and the sandbox repository
# carries its own identity and branch names rather than inheriting the
# runner's git configuration.
#
# Usage: scripts/tests/test_baseline_churn_report.sh

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPORT="${SCRIPT_DIR}/../baseline_churn_report.sh"

TESTS_RUN=0
FAILURES=0

pass() {
  TESTS_RUN=$((TESTS_RUN + 1))
  echo "  ok: $1"
}

fail() {
  TESTS_RUN=$((TESTS_RUN + 1))
  FAILURES=$((FAILURES + 1))
  echo "  FAILED: $1" >&2
}

check_contains() {
  local haystack="$1" needle="$2" description="$3"

  if [[ "$haystack" == *"$needle"* ]]; then
    pass "$description"
  else
    fail "$description (missing: ${needle})"
    echo "--- report ---" >&2
    echo "$haystack" >&2
    echo "--------------" >&2
  fi
}

check_absent() {
  local haystack="$1" needle="$2" description="$3"

  if [[ "$haystack" != *"$needle"* ]]; then
    pass "$description"
  else
    fail "$description (unexpectedly present: ${needle})"
  fi
}

WORK_DIR="$(mktemp -d)"
trap 'rm -rf "$WORK_DIR"' EXIT

SUMMARY_COUNT=0

# Runs the report with a summary file of its own and returns what it wrote,
# so nothing depends on whether the caller's environment already exports
# GITHUB_STEP_SUMMARY.
report_for() {
  SUMMARY_COUNT=$((SUMMARY_COUNT + 1))
  local summary="${WORK_DIR}/summary-${SUMMARY_COUNT}.md"
  local status=0

  # Pre-created so the usage-error path, which exits before writing anything,
  # still leaves something to read and reports its own exit status rather than
  # `cat`'s.
  : >"$summary"

  GITHUB_STEP_SUMMARY="$summary" "$REPORT" "$@" || status=$?

  cat "$summary"

  return "$status"
}

# A clean runner has no global git identity and may default to any initial
# branch name, so the sandbox repository supplies both itself.
export GIT_AUTHOR_NAME="Baseline Churn Test"
export GIT_AUTHOR_EMAIL="test@example.invalid"
export GIT_COMMITTER_NAME="$GIT_AUTHOR_NAME"
export GIT_COMMITTER_EMAIL="$GIT_AUTHOR_EMAIL"

REPOSITORY="${WORK_DIR}/repository"
mkdir -p "${REPOSITORY}/baselines"
cd "$REPOSITORY"

git init --quiet --initial-branch=main .
git config user.email "$GIT_AUTHOR_EMAIL"
git config user.name "$GIT_AUTHOR_NAME"
git config commit.gpgsign false

printf 'original\n' >baselines/kept.txt
printf 'original\n' >baselines/changed.txt
printf 'original\n' >baselines/removed.txt
printf 'unrelated\n' >source.txt
git add -A
git commit --quiet -m "base"

git checkout --quiet -b feature

# A commit on `main` after the branch point: the report must not attribute it
# to the branch, which is what a two-dot diff against the tip would do.
git checkout --quiet main
printf 'someone else\n' >baselines/other.txt
git add -A
git commit --quiet -m "another baseline on main"

git checkout --quiet feature
printf 'regenerated\n' >baselines/changed.txt
printf 'new\n' >baselines/added.txt
rm baselines/removed.txt
printf 'edited\n' >source.txt
git add -A
git commit --quiet -m "regenerate baselines"

echo "baseline_churn_report.sh"

report="$(report_for main baselines)"

check_contains "$report" "1 added, 1 changed, 1 removed." "counts every kind of change"
check_contains "$report" "baselines/added.txt" "names the added baseline"
check_contains "$report" "baselines/changed.txt" "names the changed baseline"
check_contains "$report" "baselines/removed.txt" "names the removed baseline"
check_contains "$report" "semantic change" "carries the reviewer reminder"
check_absent "$report" "source.txt" "ignores files outside the given directories"
check_absent "$report" "baselines/other.txt" "compares against the merge base, not the base tip"

git checkout --quiet -b untouched main
report="$(report_for main baselines)"
check_contains "$report" "No committed baselines changed" "says so when nothing changed"

git checkout --quiet feature

status=0
report="$(report_for definitely-not-a-ref baselines)" || status=$?
if [[ "$status" -eq 0 ]]; then
  pass "an unknown base ref is reported, not a job failure"
else
  fail "an unknown base ref exited ${status}"
fi
check_contains "$report" "No merge base" "says why it could not compare"

# The script's whole job is appending to $GITHUB_STEP_SUMMARY when CI sets it.
summary_file="${WORK_DIR}/explicit-summary.md"
GITHUB_STEP_SUMMARY="$summary_file" "$REPORT" main baselines
check_contains "$(cat "$summary_file")" "1 added, 1 changed, 1 removed." "writes to \$GITHUB_STEP_SUMMARY"

# With no summary variable at all the report goes to stdout, which is what a
# developer running it by hand gets.
report="$(env -u GITHUB_STEP_SUMMARY "$REPORT" main baselines)"
check_contains "$report" "1 added, 1 changed, 1 removed." "falls back to stdout"

status=0
report_for main >/dev/null 2>&1 || status=$?
if [[ "$status" -eq 2 ]]; then
  pass "a missing directory argument is a usage error"
else
  fail "a missing directory argument exited ${status}, expected 2"
fi

# `git diff --name-status` renders a rename as `R100<TAB>old<TAB>new`. Both
# halves matter: the old path says what went away, the new one says where the
# recorded answer lives now.
git checkout --quiet -b renames main
git mv baselines/kept.txt baselines/kept_under_a_new_name.txt
git commit --quiet -m "rename a baseline"

report="$(report_for main baselines)"

check_contains "$report" "renamed" "labels a rename as such"
check_contains "$report" "baselines/kept.txt -> baselines/kept_under_a_new_name.txt" \
  "shows both halves of a rename"

echo
echo "${TESTS_RUN} test(s) run, ${FAILURES} failure(s)"

exit $((FAILURES > 0 ? 1 : 0))
