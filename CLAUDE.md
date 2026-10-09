# CLAUDE.md — agent entry

**Read and follow [`AGENTS.md`](AGENTS.md).** It is the canonical, full guide for this repository: project overview, build and test commands, compiler pipeline, conventions, testing workflow, and pitfalls. Do not duplicate its content here.

The boundaries below are the ones that get violated most often. `AGENTS.md` is authoritative whenever this summary and it disagree.

- The selfhost compiler in `ignis/` (written in Ignis) is the only compiler that builds, and it defines the language. `BOOTSTRAP.md` and the bootstrap ladder (`scripts/bootstrap.sh`) are the build system; there is no other build.
- **`crates/` is frozen.** The former Rust compiler is kept for reference only. Never edit it, never add Rust code anywhere, and never use it as the authority for what the language means.
- **The two-step rule:** a language feature may be used in `ignis/` or `std/` only after compiler support for it has landed in the official stage0 binary — never in the same PR that adds the support.
- Format every changed `.ign` file (`ignis fmt --check` runs in CI over `std/`, `ignis/` and `example/`), and keep every write to stdout/stderr inside `ignis/output/`.

Minimum verification for compiler changes: `scripts/bootstrap.sh stage1` and `scripts/bootstrap.sh gate-g3-stage1`.
