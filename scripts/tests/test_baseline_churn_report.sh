#!/usr/bin/env bash
#
# Exercises scripts/baseline_churn_report.sh against a throwaway repository.
#
# The report is the only place a reviewer is told that a regenerated baseline
# is a semantic change, so the cases that matter are: it names every kind of
# change, it says nothing when nothing changed, it compares against the merge
# base rather than the base branch tip (a branch that is merely behind must
# not claim someone else's baselines as its own churn), and it never fails the
# job that runs it.
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

REPOSITORY="${WORK_DIR}/repository"
mkdir -p "${REPOSITORY}/baselines"
cd "$REPOSITORY"

git init --quiet --initial-branch=main .
git config user.email "test@example.invalid"
git config user.name "Test"

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

report="$("$REPORT" main baselines)"

check_contains "$report" "1 added, 1 changed, 1 removed." "counts every kind of change"
check_contains "$report" "baselines/added.txt" "names the added baseline"
check_contains "$report" "baselines/changed.txt" "names the changed baseline"
check_contains "$report" "baselines/removed.txt" "names the removed baseline"
check_contains "$report" "semantic change" "carries the reviewer reminder"
check_absent "$report" "source.txt" "ignores files outside the given directories"
check_absent "$report" "baselines/other.txt" "compares against the merge base, not the base tip"

git checkout --quiet -b untouched main
report="$("$REPORT" main baselines)"
check_contains "$report" "No committed baselines changed" "says so when nothing changed"

git checkout --quiet feature

status=0
"$REPORT" definitely-not-a-ref baselines >/dev/null || status=$?
if [[ "$status" -eq 0 ]]; then
  pass "an unknown base ref is reported, not a job failure"
else
  fail "an unknown base ref exited ${status}"
fi

summary_file="${WORK_DIR}/summary.md"
GITHUB_STEP_SUMMARY="$summary_file" "$REPORT" main baselines
check_contains "$(cat "$summary_file")" "1 added, 1 changed, 1 removed." "writes to \$GITHUB_STEP_SUMMARY"

status=0
"$REPORT" main >/dev/null 2>&1 || status=$?
if [[ "$status" -eq 2 ]]; then
  pass "a missing directory argument is a usage error"
else
  fail "a missing directory argument exited ${status}, expected 2"
fi

echo
echo "${TESTS_RUN} test(s) run, ${FAILURES} failure(s)"

exit $((FAILURES > 0 ? 1 : 0))
