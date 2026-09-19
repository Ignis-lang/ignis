# The bootstrap ladder

The self-hosted Ignis compiler (`ignis/`, written in Ignis) is built by itself
through a chain of stages, driven by `scripts/bootstrap.sh`. This document
describes how the ladder works today, before the Rust host compiler is
frozen, and the rule that keeps a PR from silently breaking the official
selfhost lineage.

## Stages

| Stage | Built by | Command |
| --- | --- | --- |
| stage0 | never built here | the Rust host compiler, or the currently promoted official selfhost binary |
| stage1 | stage0 | `scripts/bootstrap.sh stage1` |
| stage2 | stage1 | `scripts/bootstrap.sh stage2` |
| stage3 | stage2 | `scripts/bootstrap.sh stage3` |

stage3 exists only to compare its emitted C against stage2's, byte for byte
(gate G1, the fixed point). Every gate below runs against stage2.

### stage0: official or host

stage0 is resolved once, by `scripts/resolve_official_stage0.sh`:

- If a selfhost binary has been promoted (three consecutive candidate nightly
  runs — see below), that binary (`ignis-selfhost-linux-amd64` on the
  `nightly` release) becomes stage0.
- Otherwise, or if that binary cannot build stage1, stage0 falls back to the
  Rust host compiler (`build_stage1_with_host` in `scripts/bootstrap.sh`).
  The fallback exists so a selfhost binary that has gone stale against a
  link/runtime contract change on `main` does not take the whole nightly
  ladder down with it — the host is already known to pass it.

An explicit `stage0=official` (a `workflow_dispatch` input, or
`IGNIS_STAGE0_MODE=official` locally) disables the fallback and fails outright
instead, so a maintainer can force and inspect that path.

Once the Rust host is frozen, this fallback goes away: stage0 will always be
the official binary, and a build failure there is terminal, not a fallback.

## Gates

Gate | Command | What it compares
--- | --- | ---
G1 | (part of `stage3`, no separate subcommand) | stage3's emitted C against stage2's — the fixed point
G2 | `parity` | the host e2e corpus, run through stage2
G3 | `gate-g3` | the selfhost test suite, run under stage2 vs. under the host
G4 | `gate-g4` | stage2's resource use (RSS, wall time) against stage1's, within 1.25x
G5 | `gate-g5` | diagnostics: stage2's error corpus output against the host's
G6 | `gate-g6` | syntax: which programs stage2 accepts/rejects, against the host's parser
G7 | `gate-g7` | `--dump-drop-schedule` output, stage2 against committed baselines (and, until the cut, stage0 against the same baselines)

G7 is the first gate whose verdict does not depend on the host. Its baselines
were generated from the host while it still existed, verified byte-identical
against the selfhost compiler, and committed under
`test_cases/e2e/ok/__drop_schedules__/`. Until the host is removed the nightly
run still cross-checks stage0 against those same baselines, so a regeneration
cannot quietly turn a red gate green; the pull-request run (`gate-g7-stage1`)
is already host-free, which is the shape the gate keeps after the cut. See
"Drop-schedule baselines" below.

`scripts/bootstrap.sh gates` runs every stage and gate locally, then
`report` turns `build/bootstrap/gates/*.json` into `report.md` and
`promotion.json`. The nightly workflow (`.github/workflows/nightly.yml`)
splits the same work across runners: `stages` builds stage1..stage3 and G1/G4,
a matrix job runs G2/G3/G5/G6/G7, and `report` collects the results.

A run where every gate passes is a *promotion candidate*. Three consecutive
candidate nightly runs promote that run's stage2 binary to
`ignis-selfhost-linux-amd64` on the `nightly` release — the new official
stage0 for every following build.

## Drop-schedule baselines (G7)

Each `ok` end-to-end fixture has a committed dump of its drop schedule at
`test_cases/e2e/ok/__drop_schedules__/<case>.txt` — the `--dump-drop-schedule`
output for that fixture, with the standard-library functions removed, and
every path repo-relative so the file is the same on every machine. G7 compiles
each fixture with the compiler under test and compares byte for byte.

Two cases deliberately have no baseline: `--project` roots (the selfhost
compiler compiling itself) and `--extra` entry points. The compiler's own
drop schedule changes with almost every commit to `ignis/`, so a baseline for
it would be churn rather than evidence. `gate-g7` compares that case against
stage1 instead — the same sources built by a different compiler, so equal
dumps mean stage0 did not change what `ignis/` means to its own ownership
analysis.

### Adding a fixture

Add the `.ign` file under `test_cases/e2e/ok/` as usual, then record its
baseline and commit both together:

```bash
scripts/bootstrap.sh gate-g7-baselines               # from $IGNIS_STAGE0
# or, with any compiler:
python3 scripts/selfhost_drop_schedule_parity.py --compiler <bin> --write-baselines
```

A fixture with no baseline fails G7 with `baseline-missing`, and pull-request
CI rejects it in under a second (`--check-coverage`, which runs no compiler).
A baseline whose fixture is gone fails the same way, as `stale`.

### Updating baselines

Regenerating is the same command. It rewrites every baseline and deletes the
ones whose fixture is gone, so an unrelated ownership change shows up as a
diff in the cases it touched.

A filtered regeneration (`--filter`) rewrites only the cases it matched and
prunes nothing, and a run where any case failed to produce a dump writes
nothing at all rather than leaving the directory half old and half new.

**A baseline diff in a pull request is a semantic change.** It says the
compiler now drops something somewhere else, or at a different time, or not at
all. A reviewer has to read those lines and agree with them; "regenerated the
baselines" is not a reason to approve one.

### What stops a regeneration from hiding a bug

Baselines are the gate's own expectations, so regenerating them is the easy
way to make a failing gate pass. Three things stand against that:

1. **An independent oracle, until the cut.** The nightly's `gate-g7` passes
   `--host "$IGNIS_STAGE0"` and cross-checks stage0 against the same
   baselines. A baseline changed to match a broken stage2 then fails against
   stage0, and the gate reports `host cross-check N-1/N` and fails. This flag
   goes away when the host does.
2. **The churn is reported where it is read.** Pull-request CI runs
   `scripts/baseline_churn_report.sh` and writes the added, changed and
   removed baselines into the job summary with the line above. Nobody reads
   700 generated files in a diff; everybody reads the summary.
3. **`.github/CODEOWNERS`** assigns the directory to the repository owner.
   This has no effect until branch protection on `main` requires review from
   code owners — worth enabling, and worth knowing it is not enabled yet.

### Cross-checking against the host by hand

```bash
python3 scripts/selfhost_drop_schedule_parity.py \
  --compiler build/bootstrap/stage2/ignis --host ignis
```

`--host` also cross-checks the `--project`/`--extra` cases, which otherwise
only ever see one selfhost stage compared against another: a bug in `ignis/`
itself sits identically in both stages and is invisible without a third
opinion. `--host-compare` still performs the original direct
host-vs-selfhost diff, so the move to baselines is reversible.

## The two-step rule

A language feature is not safe to use in the compiler's own sources
(`ignis/`) or the standard library (`std/`) the moment support for it lands in
`crates/`. It is only safe once that support has been **promoted to the
official selfhost binary** — otherwise the official binary cannot build its
own sources, and the only thing standing between that and a broken nightly is
the host fallback described above.

**Rule:** a language change may only be *used* in `ignis/` or `std/` after
compiler support for it has been promoted to the official binary. Landing the
change and its first use in the same PR violates this, even if the PR itself
is green — the host builds it either way.

### The incident this rule exists for

A language change landed together with its first use in the compiler's own
sources. The PR was green: the host compiler built everything. The nightly
ladder was not — the still-unpromoted official binary could not compile
`main` (`Error[A0014] ...`), and only the host fallback kept the ladder from
failing outright. The violation was invisible until the nightly ran, hours
after the PR merged.

### The PR gate

`.github/workflows/ci.yml`'s `Official stage0 gate` job catches this on the
PR itself: it resolves the current official asset
(`scripts/resolve_official_stage0.sh`) and builds stage1 with it alone,
fallback disabled (`IGNIS_STAGE0_NO_FALLBACK=1`). If no official asset exists
yet (a fresh fork, or a promotion streak that has never reached 3), the job
skips — there is nothing to check yet.

On failure, split the PR: land the compiler support first, wait for it to
promote to the official binary (three green nightlies), then use the feature
in a follow-up PR.

### Override

If the change is a bug fix the official binary genuinely cannot express (for
example, the fix's own correctness depends on the very compiler change that
has not promoted yet), apply the `stage0-break-approved` label to the PR. The
gate still runs and still reports the failure, but as a warning instead of a
blocking check.
