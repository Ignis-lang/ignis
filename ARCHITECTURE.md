## Overview
- Ignis is a general-purpose, statically typed language that compiles Ignis source to C and links native binaries via GCC.
- The repository is a Rust workspace containing compiler crates, LSP support, standard library sources, and C runtime.

## Tech Stack
- Languages: Rust (compiler/LSP), Ignis (.ign sources), C (runtime, generated output).
- Tooling: Cargo workspace (`Cargo.toml`), GCC for C compilation/linking, `ar` for archives, `make` for runtime rebuilds.
- Key crates: `crates/ignis` (CLI), `crates/ignis_driver` (pipeline), `crates/ignis_parser`, `crates/ignis_analyzer`, `crates/ignis_codegen_c`, `crates/ignis_lsp`.

## Directory Structure
- `crates/` - Rust workspace crates for compiler/LSP.
- `crates/ignis/src` - CLI entry points (`crates/ignis/src/main.rs`, `crates/ignis/src/cli.rs`).
- `crates/ignis_driver/src` - Build pipeline orchestration, module discovery, linking (`crates/ignis_driver/src/pipeline.rs`, `crates/ignis_driver/src/context.rs`).
- `crates/ignis_parser/src` - Lexer/parser pipeline.
- `crates/ignis_analyzer/src` - Analyzer phases (binding, typechecking, borrow checking, etc.).
- `crates/ignis_codegen_c/src` - C emission.
- `crates/ignis_lsp/src` - Language Server implementation.
- `std/` - Ignis standard library sources and runtime (`std/manifest.toml`, `std/runtime`).
- `example/` - Example Ignis programs.
- `test_cases/`, `crates/*/tests` - Ignis test cases and Rust test harnesses with snapshots.
- `docs/` - Language reference and ABI docs.

## Core Components
- **CLI** (`crates/ignis/src/main.rs`) - Parses commands, resolves input (project vs single file), and builds `IgnisConfig`.
- **Driver Pipeline** (`crates/ignis_driver/src/pipeline.rs`) - Orchestrates scanning/parsing, analysis, monomorphization, ownership checks, LIR lowering, C emission, and linking.
- **Compilation Context** (`crates/ignis_driver/src/context.rs`) - Module discovery, import resolution, and per-module analysis ordering.
- **Parser/Lexer** (`crates/ignis_parser/src`) - Tokenizes and parses Ignis source into AST.
- **Analyzer** (`crates/ignis_analyzer/src/lib.rs`) - Runs binding, resolve, typecheck, borrowcheck, const eval, and lowers to HIR.
- **LIR + Codegen** (`crates/ignis_lir/src`, `crates/ignis_codegen_c/src`) - Lowers HIR to LIR and emits C output.
- **LSP** (`crates/ignis_lsp/src`) - Runs project-aware analysis and publishes diagnostics/completions.
- **Std Runtime** (`std/runtime`) - C runtime linked into compiled binaries.

## Data Flow
- CLI resolves compile input and builds config (`crates/ignis/src/main.rs`).
- `compile_project` discovers modules and parses files into AST (`crates/ignis_driver/src/context.rs`).
- Analyzer runs per-module in topological order to build shared HIR/defs/types (`crates/ignis_driver/src/context.rs`).
- Driver monomorphizes, runs ownership checking, lowers to LIR, and verifies (`crates/ignis_driver/src/pipeline.rs`).
- Codegen emits C (module-based when std archive is available) and writes headers/C files (`crates/ignis_driver/src/pipeline.rs`, `crates/ignis_codegen_c/src/emit.rs`).
- External tools compile/link: `gcc`, `ar`, and `make` (`crates/ignis_driver/src/link.rs`).

## External Integrations
- GCC for compiling generated C and linking binaries (`crates/ignis_driver/src/link.rs`).
- `ar` for building static archives (`crates/ignis_driver/src/pipeline.rs`).
- `make` for std runtime rebuilds (`crates/ignis_driver/src/link.rs`, `std/runtime/Makefile`).
- LSP uses `tower_lsp` for protocol implementation (`crates/ignis_lsp/src/lib.rs`).

## Configuration
- Workspace config: `Cargo.toml`.
- Compiler config (project): `ignis.toml`.
- Standard library manifest: `std/manifest.toml`.
- Environment: `IGNIS_STD_PATH` sets standard library root (`crates/ignis/src/main.rs`).
- Formatting: `.rustfmt.toml`, `.editorconfig`.

## Build & Deploy
- Build compiler: `cargo build --release` (workspace root `Cargo.toml`).
- Install CLI: `cargo install --path crates/ignis`.
- Build std library: `ignis build-std` (uses `std/manifest.toml`).
- Compile project: `ignis build` (uses `ignis.toml`).
- Compile single file: `ignis build path/to/file.ign`.

## Notes / Gaps
- Some deep data-layer modules (notably `crates/ignis_lir/src/lowering/mod.rs`) exceed read tool limits, so details there are summarized at a higher level.
