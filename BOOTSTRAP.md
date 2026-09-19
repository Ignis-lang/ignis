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
G1 | `gate-g1` (part of `stage3`) | stage3's emitted C against stage2's — the fixed point
G2 | `gate-g2` / `parity` | the host e2e corpus, run through stage2
G3 | `gate-g3` | the selfhost test suite, run under stage2 vs. under the host
G4 | `gate-g4` | stage2's resource use (RSS, wall time) against stage1's, within 1.25x
G5 | `gate-g5` | diagnostics: stage2's error corpus output against the host's
G6 | `gate-g6` | syntax: which programs stage2 accepts/rejects, against the host's parser
G7 | `gate-g7` | `--dump-drop-schedule` output, stage2 against the host

`scripts/bootstrap.sh gates` runs every stage and gate locally, then
`report` turns `build/bootstrap/gates/*.json` into `report.md` and
`promotion.json`. The nightly workflow (`.github/workflows/nightly.yml`)
splits the same work across runners: `stages` builds stage1..stage3 and G1/G4,
a matrix job runs G2/G3/G5/G6/G7, and `report` collects the results.

A run where every gate passes is a *promotion candidate*. Three consecutive
candidate nightly runs promote that run's stage2 binary to
`ignis-selfhost-linux-amd64` on the `nightly` release — the new official
stage0 for every following build.

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
