# Code Style

Conventions for the selfhost compiler (`ignis/`) and the standard library (`std/`). The Rust sources under `crates/` are frozen and kept for reference only: nothing in this document governs them, and they must not be edited. When the selfhost and `crates/` disagree, the selfhost is the authority.

## Naming Conventions

| Element | Convention | Examples | References |
| --- | --- | --- | --- |
| Types (record/enum) | PascalCase | `Vector`, `Option`, `Layout` | `std/vector/mod.ign` |
| Functions/methods/fields/params | camelCase | `getLength`, `pushBack`, `myVariable` | `std/string/mod.ign` |
| Constants, enum members | UPPER_SNAKE_CASE | `MAX_SIZE`, `PI`, `TAU` | `std/math/mod.ign` |
| Modules/namespaces | PascalCase | `Math`, `Memory`, `Io` | `std/manifest.toml` |
| Test namespaces | PascalCase | `Test` | `std/test/mod.ign` |
| Files | lower_snake_case | `my_module.ign`, `mut_self_test.ign` | `test_cases/` |

Do not mix naming styles within the same construct or scope.

## File Organization

- One logical module per directory; the module entry file is `mod.ign` (see `ignis/diagnostics/mod.ign`, `std/vector/mod.ign`). Submodules are flat `lower_snake_case.ign` files next to it.
- Prefer implementing in existing files unless it is a new logical component.
- Use full words for names; no abbreviations like `q` for `queue`.
- Document public declarations with `///` doc comments. Comments explain non-obvious "why", not what the code does.
- Unit tests live next to the code they cover, in a `tests.ign` or `*_tests.ign` module in the same directory (see `ignis/lexer/tests.ign`, `ignis/format/api_tests.ign`, `ignis/diagnostics/model_tests.ign`).

## Compiler-Wide Patterns

### Output discipline

Every write to stdout or stderr goes through `ignis/output/` (`Output::message` for stderr, `Output::data` for stdout), never `Io::print*` directly: the output module draws the live progress line and erases it around other output.

### Diagnostics

- Diagnostics are collected in a `DiagnosticBag` (`ignis/diagnostics/model.ign`) and rendered by `ignis/diagnostics/render.ign`; the analyzer continues after errors instead of returning early.
- Every diagnostic has a code from `ignis/diagnostics/codes.ign`; add a new code there when introducing a diagnostic.
- Avoid panicking paths in compiler code; prefer `Option`/`Result` propagation and diagnostic reporting.

### Ids, stores, and interning

Core data is referenced by small id handles (`ignis/ids/mod.ign`) into stores, with interning for cheap symbol comparison (`ignis/interning/mod.ign`, `ignis/symbols/mod.ign`). Ids are cheap to copy and compare; stores own the data. New arena-allocated data should follow the same id/store shape.

## Testing

- Unit tests: `@test` functions next to the code (see File Organization); run with `ignis test --filter <name>` from the repository root.
- E2E fixtures: `.ign` files under `test_cases/e2e/{ok,err}` with `// e2e:` headers, baselines in their `__snapshots__/`; see `AGENTS.md`, "Adding an E2E Test".
- Standard library tests: `@test` functions under `std/`, run with `ignis test-std --std-path std`.
- Snapshot assertions (`std::test::Test::assertSnapshot`) write to the `__snapshots__/` directory next to the module under test; recreate with `ignis test --update-snapshots`.
- Prefer `std::test::Test` namespace helpers over ad hoc `@panic(...)` assertions.
- Canonical `std::hash::Eq` is the equality contract behind generic test assertions (`Test::assertEq<T>` / `assertNe<T>`).

## Formatting

Default formatter configuration (`ignis/format/config.ign`): `indent_width = 2`, `line_width = 100`, `use_tabs = false`, `sort_imports = false`. Format every changed `.ign` file with `ignis fmt`; CI runs `ignis fmt --check` over `std/`, `ignis/` and `example/`.

### Ignis Formatter Canonical Rules

- Empty high-level blocks (`namespace`, `record`, `enum`, `trait`, `extern`) canonicalize to inline `{}`.
- Single-line callable signatures and single-line record initializers do not keep a trailing comma.
- Multiline callable signatures and multiline record initializers emit a trailing comma on the final item.
- Import sorting is opt-in only; default formatting preserves source order.
- A formatter safety failure indicates a formatter bug or invalid input, not a style lint.

## Do's and Don'ts

- Do keep Ignis naming: PascalCase types, camelCase members, UPPER_SNAKE_CASE constants.
- Do route all compiler output through `ignis/output/`.
- Do collect diagnostics instead of returning early on errors, and give each one a code from `ignis/diagnostics/codes.ign`.
- Do add `@test` functions next to new code and e2e fixtures for observable behavior, including their committed baselines (snapshots, G6 parse verdicts, G7 drop schedules).
- Do use `@noescape` on closure parameters that must not outlive their call site.
- Do respect the two-step rule: never use a new language feature in `ignis/` or `std/` in the same PR that adds it.
- Don't edit anything under `crates/`, and don't add Rust code anywhere.
- Don't assume Ignis behaves like Rust, TypeScript, or any other language; check the selfhost source, and state uncertainty explicitly instead of guessing by analogy.
- Don't bypass the bootstrap ladder; build and test through `scripts/bootstrap.sh`.
