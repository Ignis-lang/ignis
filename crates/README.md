# Frozen Rust compiler

This directory holds the Rust implementation of the Ignis compiler that
bootstrapped the self-hosted one. It is frozen and kept for reference only.

| Question | Answer |
| --- | --- |
| Is it built or tested? | No. CI, the nightly and releases never build or run it. |
| Which compiler is canonical? | The selfhost compiler in `ignis/`. It defines the language; where the two differ, the selfhost wins. |
| Can I change it? | No. Fix behavior in `ignis/`. Changes here need review from the code owner (`.github/CODEOWNERS`). |
| Is it a stage0 or a fallback? | No. stage0 is the promoted official selfhost binary or a compiler built from the C seed. |

## Getting a compiler without it

The committed C seed rebuilds the compiler with gcc alone:

```bash
scripts/build_from_seed.sh              # bootstrap/seed -> build/bootstrap/stage0-seed/ignis
scripts/bootstrap.sh all-from-seed      # the full ladder from the seed, ending with the fixed-point gate
```

See `BOOTSTRAP.md`, "Recovering a compiler", for the order in which stage0
sources are tried and how the seed is refreshed.
