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

## C seed (A3)

`bootstrap/seed/` holds the whole compiler, standard library included, as the
single C file a fixed-point stage2 emits (`selfhost_emit.c`, xz-compressed),
a copy of every std/runtime header that C includes (today only
`ignis_rt.h`), and `manifest.json`. It rebuilds the compiler with gcc alone,
reading nothing else from the checkout, so the ladder can start without Rust,
and without any release asset, once the host is frozen and deleted.

The manifest records the source commit, the sha256 and size of both the
archive and the uncompressed C (`xz_size`, `c_size`), the sha256 of each
bundled header, the compiler that emitted the C (stage2 of that commit), the
gcc recipe, the gcc it was generated next to, and the date. Read sizes and
hashes there; this document does not copy them. The recipe is the one the
selfhost driver runs on itself for `ignis.toml`'s `[build]` profile, with the
bundled headers standing in for std/runtime, and the manifest is the only
place it is written down:

```bash
gcc -c selfhost_emit.c -o selfhost_emit.o -O2 -I bootstrap/seed
gcc -O2 selfhost_emit.o -o ignis -lm
```

Besides the bundled headers, the C includes only standard C and POSIX
headers (libc, libm, `unistd.h`, `sys/*.h`), and holds no absolute path, so
it does not depend on the machine that generated it. `scripts/bootstrap.sh
seed` refuses to write a seed whose C contains the repository's own path, or
includes a quoted header that is not in std/runtime.

### Rebuilding from it

```bash
scripts/build_from_seed.sh [--seed bootstrap/seed] [-o build/bootstrap/stage0-seed/ignis]
scripts/bootstrap.sh all-from-seed      # the ladder with no Rust at all
```

`build_from_seed.sh` needs bash, xz, sha256sum and gcc, and nothing else. It
checks the archive's and each bundled header's sha256 against the manifest,
decompresses the archive, checks the C's sha256, compiles and links, and
prints the binary's path. A mismatch or a
malformed manifest stops it with a non-zero exit. `--verify-only` runs the
checks and compiles nothing; pull-request CI runs that on every change.

`stage1-from-seed` builds that binary (reused while the seed is unchanged),
records it in `build/bootstrap/stage0.json` as kind `seed`, and builds
stage1 with it. A seed-built stage0 never falls back to the host: the point is
to prove the ladder needs no Rust. Later subcommands (`stage2`, `stage3`, the
gates) keep using that stage0 until `clean` or an explicit `IGNIS_STAGE0`.
`all-from-seed` continues through stage3, so it ends with G1.

The nightly job "Rebuild from the C seed (no Rust)" installs no Rust
toolchain, removes every cargo and rustup directory from PATH, fails unless
cargo, rustc, rustup and ignis are all absent, then runs `all-from-seed` and
requires G1. It is independent of the
promotion flow: its verdict never touches the streak.

### Refreshing it

```bash
scripts/bootstrap.sh seed
```

It builds stage3, requires G1, and writes the C stage2 emitted, which G1 has
just shown equal to stage1's, with the std/runtime headers it includes and a
new manifest. ignis/ and std/ must be committed first, so
the recorded source commit describes the seed. Commit the new
`bootstrap/seed/` on its own.

Refreshing is deliberate, never automatic. The seed only has to build the
current sources' stage1, the same contract the official binary has under the
two-step rule below. Refresh it:

- before the host is frozen, so the cut starts from a current seed;
- after any change to ignis/ or std/ that must stay bootstrappable without
  the host, typically when the nightly seed job fails because the sources
  use something the seed's compiler cannot build.

### Why it is committed

The official selfhost binary lives only as a release asset. A deleted
release, a lost account or an expired artifact would leave no way back to a
working compiler once the host is gone. The seed travels with every clone,
is verified by checksum, and needs only a C compiler to come back to life.

## Gates

Gate | Command | What it compares
--- | --- | ---
G1 | (part of `stage3`, no separate subcommand) | stage3's emitted C against stage2's — the fixed point
G2 | `parity` | the host e2e corpus, run through stage2
G3 | `gate-g3` | the selfhost test suite, run under stage2 vs. under the host
G4 | `gate-g4` | stage2's resource use (RSS, wall time) against stage1's, within 1.25x
G5 | `gate-g5` | diagnostics: stage2's error corpus output against the host's
G6 | `gate-g6` | syntax: which programs stage2 accepts/rejects, against committed baselines (and, until the cut, the host against the same baselines)
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

## Parse-verdict baselines (G6)

G6 follows the G7 model below; this section only covers what differs. Its
corpus is every `.ign` file under `test_cases/`, `example/` and `std/`, and
each case has a committed verdict at `test_cases/__parse_verdicts__/<case>.txt`:
`parse-verdict v1` followed by `accepted` or `rejected`. The case name is the
repo-relative path with every run of non-alphanumeric characters flattened to
`_`. Only lexer and parser diagnostics against the case file decide a verdict.

A new `.ign` file anywhere in that corpus needs its baseline in the same
commit, or pull-request CI fails its `--check-coverage` step (no compiler, a
few seconds). A missing, malformed or orphaned baseline, and two sources that
flatten to the same case name, all fail it. To record or refresh them:

```bash
scripts/bootstrap.sh gate-g6-baselines               # from stage2
# or, with any selfhost binary:
python3 scripts/selfhost_syntax_parity.py --compiler <bin> --write-baselines [--host ignis]
```

The compiler is run with the selfhost CLI, which is why the default is stage2
and not `$IGNIS_STAGE0`. The write rules are G7's (all or nothing, no pruning
under `--filter`), plus one: with `--host`, which `gate-g6-baselines` passes
whenever the host resolves, nothing is written if the host disagrees with the
compiler on any case. Read a verdict diff as a parser change.

### Parser unit-test snippets

About two hundred cases are the source strings the host parser's unit tests
parse, wrapped the way each test helper wraps them. Their Rust sources go away
at the cut, so they are committed byte for byte under
`test_cases/parser/host_unit_tests/`. After adding or changing a parser unit
test, re-materialize them and commit the result with its baselines:

```bash
python3 scripts/selfhost_syntax_parity.py --materialize-parser-tests
```

Pull-request CI runs `--check-parser-tests`, which reports any drift between
the committed snippets and the Rust sources without running a compiler.

### Host cross-check

The nightly's `gate-g6` passes `--host` the Rust host
(`$IGNIS_STAGE0_HOST_FALLBACK`) for the reasons given for G7 below. The host's
verdict on each case is compared with the baseline, and `host-drift` or
`host-error` is reported apart from the selfhost's result and fails the gate.
A host that cannot be found fails the gate too. `--host-compare` still runs the
original direct host-vs-selfhost comparison, and the baseline churn report and
`.github/CODEOWNERS` cover both directories.

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
   `--host` the Rust host (`$IGNIS_STAGE0_HOST_FALLBACK`, default `ignis` on
   PATH) and cross-checks it against the same baselines. A baseline changed to
   match a broken stage2 then fails there, and the gate reports
   `host cross-check N-1/N` and fails. Deliberately not `$IGNIS_STAGE0`: that
   can resolve to the promoted official selfhost asset, and asking a selfhost
   binary to confirm baselines one of its own ancestors produced proves
   nothing. This flag goes away when the host does.
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
