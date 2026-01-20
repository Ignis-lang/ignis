## Naming Conventions
- Rust types: PascalCase (`crates/ignis_ast/src/statements/function.rs`).
- Rust fields/vars/functions: snake_case (`crates/ignis_token/src/token_types.rs`).
- Ignis types/enums: PascalCase, enum variants PascalCase (`example/allocator/src/allocator.ign`).
- Ignis functions/fields/methods/variables: camelCase (`example/allocator/src/allocator.ign`).
- Ignis files: lower_snake_case (`example/allocator/src/block.ign`, `test_cases/analyzer/mutability/mut_self_test.ign`).
- Constants: UPPER_SNAKE_CASE in Ignis (`crates/ignis_driver/tests/e2e_ok.rs`).

## File Organization
- Rust workspace crates live in `crates/<name>/src` with a `lib.rs` or `main.rs` root.
- Compiler pipeline flows through `crates/ignis_driver`, with parser/analyzer/codegen in dedicated crates.
- Tests live under `crates/<crate>/tests` with shared helpers in `tests/common/mod.rs`.
- Ignis standard library is under `std/` and referenced via `std/manifest.toml`.

## Import Style
- Imports are grouped with std/external/internal blocks and separated by blank lines (see `crates/ignis_driver/src/pipeline.rs`).
- Rustfmt keeps import order as written (`.rustfmt.toml` sets `reorder_imports = false`).

## Code Patterns
- CLI wiring uses `clap` with `#[derive(Parser)]` and typed subcommands (`crates/ignis/src/cli.rs`).
- Pipeline-style functions return `Result<(), ()>` and emit user-facing errors via `eprintln!` + `colored` (`crates/ignis_driver/src/pipeline.rs`).
- Analysis output is passed as structs and reused across phases (driver/analyzer interaction in `crates/ignis_driver/src/context.rs`).

## Error Handling
- Driver/CLI functions typically return `Result<(), ()>` and print errors directly (`crates/ignis/src/main.rs`, `crates/ignis_driver/src/pipeline.rs`).
- Diagnostics are rendered via `ignis_diagnostics::render` (driver and LSP layers).
- LSP wraps analysis in `catch_unwind` to avoid crashing on panics (`crates/ignis_lsp/src/server.rs`).

## Logging
- CLI/build logs use `ignis_log` macros like `cmd_header!`, `phase_log!`, `cmd_ok!` (`crates/ignis_driver/src/pipeline.rs`).
- LSP logs to a file via `completion::log` (`crates/ignis_lsp/src/lib.rs`).

## Testing
- Snapshot tests use `insta::assert_snapshot` (`crates/ignis_analyzer/tests/golden_ok.rs`, `crates/ignis_codegen_c/tests/golden_c.rs`).
- E2E tests compile and run Ignis code snippets (`crates/ignis_driver/tests/e2e_ok.rs`).
- Snapshots are stored in `crates/*/tests/snapshots`.

## Formatting
- Rustfmt configuration: 2-space indentation, `max_width = 120`, no import reordering (`.rustfmt.toml`).
- Function params layout is vertical (`.rustfmt.toml`).

## Do's and Don'ts
- Do keep Ignis language naming: PascalCase types, camelCase members (`example/allocator/src/allocator.ign`).
- Do keep Rust imports in intentional order (no automatic reorder) (`.rustfmt.toml`).
- Do use `ignis_log` macros for user-facing build output (`crates/ignis_driver/src/pipeline.rs`).
- Don't rely on implicit std path; prefer `IGNIS_STD_PATH` or `ignis.toml` (`crates/ignis/src/main.rs`).
