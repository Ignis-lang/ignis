Guidelines for AI agents working on the Ignis compiler.

## Project Overview

Ignis is a statically typed, general-purpose language that compiles to C and links native binaries via GCC. The compiler is written in Ignis itself (`ignis/`) and builds itself through the bootstrap ladder described in `BOOTSTRAP.md`. It is the only compiler that scripts, CI, the nightly and releases build or run, and it defines the language.

**`crates/` is frozen.** It holds the former Rust compiler, kept for reference only. Nothing builds, tests or runs it, and it must not be changed. See `crates/README.md`.

**Repository layout:**

```
ignis/                # The compiler (Ignis sources, project file: ignis.toml)
  main.ign            # Driver: command dispatch, compile pipeline, test runs, fmt, doc, lsp entry
  cli.ign             # Command-line parsing and help text
  lexer/              # Lexer
  syntax/             # Tokens, token stream, cursor
  parser/             # Recursive-descent parser with error recovery
  ast/                # AST node types
  analyzer/           # Binding, resolution, typechecking, const eval, checks, lints,
                      # HIR lowering, capture/escape analysis, monomorphization, borrow/ownership checking
  hir/                # High-level IR (typed, tree-based) and drop schedules
  lir/                # Low-level IR (basic blocks), HIR->LIR lowering, verification
  codegen/            # LIR -> C emission
  build/              # Module discovery, build cache, C compiler/linker/archiver calls, test runner, fixtures
  config/             # ignis.toml loading
  diagnostics/        # Diagnostic codes, model, rendering
  format/             # Formatter (ignis fmt)
  doc/                # API documentation extraction (ignis doc)
  lsp/                # Language server (ignis lsp)
  ids/ indexing/ interning/ symbols/   # Shared id, index, interning and symbol utilities
std/                  # Ignis standard library sources (.ign), runtime included
  manifest.toml       # Module registry and linking configuration
  runtime/            # ignis_rt.h: the emitted type prelude guard (the runtime itself is Ignis)
  test/               # Test assertions and snapshot helpers (`std::test::Test`)
bootstrap/seed/       # The committed C seed: the whole compiler as one C file, rebuilt with gcc alone
scripts/              # Bootstrap ladder, gates, parity harnesses, installer
test_cases/           # Fixtures and committed baselines (e2e, parse verdicts, doc baselines, parser cases)
example/              # Example Ignis programs
crates/               # Frozen Rust compiler, reference only
```

## Build & Run Commands

The ladder builds the compiler; there is no other build. Build outputs land in `build/bootstrap/<stage>/ignis`.

```bash
# Build the compiler
scripts/bootstrap.sh stage1-from-seed          # C seed -> stage0 -> stage1 (no prebuilt compiler needed)
scripts/bootstrap.sh stage1                    # stage1 from the recorded stage0 (official binary or seed)
scripts/bootstrap.sh stage2                    # stage2 = ignis/ compiled by stage1 (what a release ships)
scripts/bootstrap.sh all                       # stage1 -> stage2 -> stage3 fixed-point gate (G1)
scripts/bootstrap.sh all-from-seed             # the same, starting from the C seed
scripts/bootstrap.sh status                    # which build/bootstrap/<stage>/ignis exist
scripts/build_from_seed.sh                     # only rebuild stage0 from bootstrap/seed
scripts/build_from_seed.sh --verify-only       # check the seed against its manifest, compile nothing

# Checks CI runs on every pull request (with stage1 on PATH as `ignis`)
ignis check                                    # Analyze the standard library and the compiler, no codegen
ignis fmt --check <files...>                   # Canonical formatting of std/, ignis/ and example/
ignis test-std --std-path std                  # Standard library test suite
scripts/bootstrap.sh gate-g3-stage1            # The compiler's own test suite, fixtures included, under stage1

# Gates (see BOOTSTRAP.md for what each compares)
scripts/bootstrap.sh parity                    # G2: e2e corpus through stage2 -> build/bootstrap/parity.md
scripts/bootstrap.sh gate-g3                   # G3: test suite under stage2, compared with stage1's run
scripts/bootstrap.sh gate-g4                   # G4: stage2 vs stage1 RSS/wall budget
scripts/bootstrap.sh gate-g5                   # G5: error corpus through stage2 against committed snapshots
scripts/bootstrap.sh gate-g6                   # G6: stage2 parse verdicts vs committed baselines
scripts/bootstrap.sh gate-g6-baselines [bin]   # regenerate test_cases/__parse_verdicts__ (review the diff)
scripts/bootstrap.sh gate-g7                   # G7: stage2 drop schedules vs committed baselines
scripts/bootstrap.sh gate-g7-stage1            # same, against stage1 (what PR CI runs)
scripts/bootstrap.sh gate-g7-baselines [bin]   # regenerate test_cases/e2e/ok/__drop_schedules__ (review the diff)
scripts/bootstrap.sh stages                    # stage1 -> stage2 -> gate-g4 -> stage3 (the nightly's ladder job)
scripts/bootstrap.sh gates                     # every stage and gate (G1..G7), then report
scripts/bootstrap.sh seal-gates                # write a skipped placeholder for every gate that has no result yet
scripts/bootstrap.sh report                    # gates/*.json -> build/bootstrap/report.md + promotion.json
scripts/bootstrap.sh seed                      # refresh bootstrap/seed (deliberate, see BOOTSTRAP.md)
scripts/selfhost_e2e_parity.py --compiler <bin> --report parity.md                 # G2 harness, any compiler binary
scripts/selfhost_e2e_parity.py --compiler <bin> --corpus err --report parity-err.md  # G5 harness
scripts/selfhost_syntax_parity.py --check-coverage          # every G6 case has a baseline (no compiler)
scripts/selfhost_drop_schedule_parity.py --check-coverage   # every ok fixture has a G7 baseline (no compiler)

# Compiling Ignis code
ignis build                                    # Compile project (reads ignis.toml)
ignis build path/to/file.ign -o out            # Compile single file
ignis check                                    # Type-check only (no codegen or linking)
ignis build-std                                # Build standard library archive
ignis --help                                   # Every command and flag

# Language-level tests
ignis test                                     # Run project tests and fixtures
ignis test path/to/file.ign                    # Run tests from a single file
ignis test --filter <substring>                # Only tests whose name contains the substring
ignis test --update-snapshots                  # Recreate selected snapshots

# Formatter
ignis fmt src/main.ign                         # Format one file
ignis fmt a.ign b.ign c.ign                    # Format multiple explicit files
ignis fmt --check src/main.ign                 # Check without rewriting
ignis fmt --emit diff src/main.ign             # Print unified diff
ignis fmt --stdin-json                         # Batch stdin protocol (NDJSON)
```

Formatter defaults: `indent_width = 2`, `line_width = 100`, `use_tabs = false`, `sort_imports = false`.
Canonical formatter rules now include inline empty high-level blocks (`namespace Foo {}`), multiline trailing commas, and no trailing comma on single-line callable signatures or record initializers.

Commands that compile the compiler itself take minutes: one stage is about two minutes of a single core, and a full `ignis test` of the compiler is longer.

## Working on `ignis/`

### General Principles

- Prioritize correctness and clarity over speed.
- Do not write comments that summarize code; only explain non-obvious "why". Document declarations with `///`.
- Prefer implementing in existing files unless it's a new logical component.
- Use full words for variable names (no abbreviations like `q` for `queue`).
- Format every changed `.ign` file with `ignis fmt`; CI runs `ignis fmt --check` over `std/`, `ignis/` and `example/`.

### The two-step rule

A language feature is safe to use in `ignis/` or `std/` only after compiler support for it has been promoted to the official selfhost binary. Landing support and its first use in the same PR breaks the official stage0. See `BOOTSTRAP.md`, "The two-step rule".

### The frozen Rust compiler

- Do not edit anything under `crates/`, and do not add Rust code anywhere.
- Do not use `crates/` as the reference for what a program means. When the selfhost and `crates/` disagree, the selfhost is right unless it is wrong by its own rules; fix it in `ignis/`.
- Comments in `ignis/` that mention "the host" describe behavior the selfhost was ported from. They are history, not a contract to keep in sync.

## Ignis Language Conventions

### Naming

| Element                             | Convention       | Example                         |
| ----------------------------------- | ---------------- | ------------------------------- |
| Variables, functions, params, fields | camelCase        | `myVariable`, `getLength`       |
| Constants, enum members              | UPPER_SNAKE_CASE | `MAX_SIZE`, `RED`               |
| Modules, structs, records, enums     | PascalCase       | `Math`, `Vector`, `Option`      |
| Files                                | lower_snake_case | `my_module.ign`                 |

### Semantics

- Do not assume Ignis behaves like Rust, TypeScript, or any other language.
- Do not infer features or semantics by analogy. Always rely on the codebase as the source of truth.
- If a language feature or behavior is unclear or undocumented, state the uncertainty explicitly.

## Compiler Pipeline

The phase lines a build prints (`lex`, `parse`, `analyze`, `capture`, `mono`, `ownership`, `lower`, `lir`, `codegen`, `emit`, `link`) map to these steps, in execution order:

```
CLI (ignis/cli.ign)
  → parseArguments(): command and options

Driver (ignis/main.ign)
  → discovery (ignis/build/resolver.ign, module_graph.ign)   # Project/single-file module graph, ignis.toml
  → build cache check (ignis/build/stamp.ign, fingerprint.ign)
  → lex + parse each module (ignis/lexer/, ignis/parser/)
  → Analyzer::analyzeProgram (ignis/analyzer/mod.ign), per module:
      1. binder.ign          predeclareRoots(), then completeRoots()   # two-pass binding
      2. resolver.ign        type-level name resolution
      3. resolver_bodies.ign body-level name resolution
      4. typecheck*.ign      typechecking (bidirectional inference)
      5. const_eval.ign      compile-time constant values
      6. checks.ign          control flow and other post-typecheck checks
      7. lint.ign            unused variables/imports/mut, deprecated calls
  → lowering.ign         AST → HIR (ignis/hir/)
  → capture.ign          closure captures (escape.ign: closure escape analysis)
  → mono.ign             monomorphization; countRemainingGenerics() must reach 0
  → borrowck.ign         fused borrow and ownership check, produces drop schedules
  → ignis/lir/lowering.ign  HIR → LIR, then ignis/lir/verify.ign verifyProgram()
  → ignis/codegen/mod.ign   emitProgram(): LIR → one C translation unit (selfhost_emit.c for the compiler itself)

Build (ignis/build/)
  → c_compiler.ign   gcc -c → object files
  → archive.ign      ar → archives (build-std)
  → linker.ign       gcc link → executable
```

## Extending the Compiler

Every step below is an edit under `ignis/`. After a change, build stage1 and run the checks in "Build & Run Commands"; `scripts/bootstrap.sh stage1` and `gate-g3-stage1` are the minimum.

### Adding a New AST Node

1. Define the node in `ignis/ast/` (`items.ign`, `statements.ign`, `expressions.ign`, `types.ign` or `patterns.ign`), including its span.
2. Parse it in `ignis/parser/` (`declarations.ign`, `statements.ign`, `expressions.ign`, `types.ign`, `patterns.ign`).
3. Handle it in the analyzer phases that apply: `binder.ign`, `resolver.ign`/`resolver_bodies.ign`, `typecheck_items.ign`/`typecheck_stmts.ign`/`typecheck_exprs.ign`, `lowering.ign`, and `borrowck.ign` if it affects ownership.
4. Handle it in `ignis/lir/lowering.ign` and `ignis/codegen/mod.ign` if it reaches them.
5. Add it to the formatter (`ignis/format/`) and to `ignis/ast/serialize.ign` if they need to print it.
6. Add tests: `@test` functions next to the code, and an e2e fixture under `test_cases/e2e/`.

### Adding a New Builtin

Builtins use `@name(args)` or `@name<Type>(args)` syntax.

1. Typecheck it in `checkBuiltinCall()` (`ignis/analyzer/typecheck_exprs.ign`).
2. Lower it to HIR in `lowerBuiltinCall()` (`ignis/analyzer/lowering.ign`), adding a `HirKind` variant (`ignis/hir/node.ign`) if needed.
3. Lower the HIR to LIR in `ignis/lir/lowering.ign`, and emit C in `ignis/codegen/mod.ign`.
4. Register it for the language server in `ignis/lsp/at_items.ign`.

Existing builtins: `typeOf`, `sizeOf`, `alignOf`, `typeName`, `bitCast`, `pointerCast`, `integerFromPointer`, `pointerFromInteger`, `sliceFromParts`, `read`, `write`, `dropInPlace`, `dropGlue`, `hash`, `eq`, `maxOf`, `minOf`, `compileError`, `panic`, `trap`, `unreachable`. `configFlag` is a directive, not a builtin, even though it is written like one in expression position; the parser handles it (`ignis/parser/expressions.ign`).

### Adding a New Pattern Form

Patterns are used by `match`, `if let`, `while let`, and `let else`.

1. Add the AST form in `ignis/ast/patterns.ign` and parse it in `ignis/parser/patterns.ign`.
2. Typecheck it against the scrutinee type (`ignis/analyzer/typecheck*.ign`; exhaustiveness lives in `typecheck_exhaustive.ign`).
3. Add the `HirPattern` variant in `ignis/hir/pattern.ign` and lower to it in `ignis/analyzer/lowering.ign`.
4. Generate the condition checks and bindings in `ignis/lir/lowering.ign`.

### Adding a New Lint

1. Add a `LintKind` member in `ignis/analyzer/lint.ign`.
2. Implement the check there and call it from `lintModule()`.
3. Add its diagnostic code in `ignis/diagnostics/codes.ign`.
4. `@allow`/`@warn`/`@deny` map attribute names to lints in the same file.

Existing lints: unused variable, unused import, unused `mut`, deprecated call.

### Adding a New Type

1. Add the variant to `Type` in `ignis/analyzer/types.ign`, with its creation, copy/owned classification, substitution and name formatting.
2. Handle it in typechecking (inference, unification, casts).
3. Handle it in `ignis/codegen/mod.ign` (C representation).

## Testing

### Test Types

| Type | Where | Run with |
| --- | --- | --- |
| Compiler unit tests | `@test` functions in `tests.ign` / `*_tests.ign` modules next to the code in `ignis/` | `ignis test --filter <name>` from the repository root, or `scripts/bootstrap.sh gate-g3-stage1` for all of them |
| E2E fixtures (ok/err/warn) | `test_cases/e2e/{ok,err}`, baselines in their `__snapshots__/` | the same `ignis test` run (`[test] fixtures` in `ignis.toml`), names `e2e::<path>` |
| Standard library tests | `@test` functions under `std/` | `ignis test-std --std-path std` |
| Drop-schedule baselines (G7) | `test_cases/e2e/ok/__drop_schedules__/` | `scripts/bootstrap.sh gate-g7-stage1` |
| Parse-verdict baselines (G6) | `test_cases/__parse_verdicts__/` | `scripts/bootstrap.sh gate-g6` |
| Doc baselines | `test_cases/doc/__doc_baselines__/` | `python3 scripts/selfhost_doc_parity.py --compiler <bin>` |
| Language server protocol | `scripts/tests/test_selfhost_lsp.py` | `IGNIS_LSP_COMPILER=<bin> python3 scripts/tests/test_selfhost_lsp.py -v` |
| Script tests | `scripts/tests/` | each script directly (see `.github/workflows/ci.yml`) |

A test run of the compiler compiles the whole compiler with its test modules, so it takes minutes. `-o` names the test binary, and its directory has to exist.

### Adding an E2E Test

The end-to-end corpus lives in Ignis source under `test_cases/e2e/{ok,err}`, one
`.ign` file per case, run by `ignis test` as fixtures. A fixture's leading
`// e2e: <option>` comment lines select its mode (see
`ignis/build/fixture_tests.ign`):

| Header | Mode |
| --- | --- |
| (none) | Compile, link and run; the baseline holds exit code and streams. |
| `// e2e: std` | Same as above, forcing the standard library on. |
| `// e2e: allow-leak` | Same as above, skipping leak checking. |
| `// e2e: err` | Compile only; expects failure. The baseline holds the reported error diagnostics. |
| `// e2e: warn` | Compile only; expects success with warnings. The baseline holds the reported warning diagnostics. |
| `// e2e: skip <reason>` | Expected to fail for the stated reason; the run fails once the case passes, so the header gets removed. |

```
// In test_cases/e2e/ok/my_feature.ign
function main(): i32 {
    return 42;
}
```

With a built compiler at `build/bootstrap/stage1/ignis`, from the repository root:

```bash
mkdir -p build/tests
build/bootstrap/stage1/ignis test --filter e2e::my_feature -o build/tests/ignis-tests                     # run it
build/bootstrap/stage1/ignis test --filter e2e::my_feature --update-snapshots -o build/tests/ignis-tests  # record its baseline
scripts/bootstrap.sh gate-g7-baselines build/bootstrap/stage1/ignis                                        # an ok fixture also needs its G7 baseline
```

A new `.ign` file under `test_cases/`, `example/` or `std/` also needs its G6 parse-verdict baseline (`scripts/bootstrap.sh gate-g6-baselines`). PR CI rejects a missing one. Baseline diffs are semantic changes: read them before committing.

### Adding a Compiler Unit Test

Add a `@test` function to the module's `tests.ign` or `*_tests.ign` file, next to the code it covers. For example, in `ignis/lexer/tests.ign`, which defines the `lexText` helper:

```ignis
@test
function lexesAnEmptyFile(): void {
  let result: LexResult = lexText("empty.ign", "");
  Test::assertEq<u64>(result.diagnostics.length(), 0);
}
```

Snapshot assertions (`std::test::Test::assertSnapshot`) write to the `__snapshots__/` directory next to the module under test; `--update-snapshots` creates or replaces them.

## Common Pitfalls

1. **Forgetting to handle a new AST variant in all analyzer phases.** A catch-all `_` arm hides the missing case. Search for an existing variant name to find every match site. The same goes for new `HirKind` variants in LIR lowering and codegen.

2. **Forgetting to offset a new `HirKind` variant's ids.** `offsetKind()` in `ignis/hir/node.ign` rewrites `HirId` fields when HIR stores are merged. A variant it does not handle keeps stale ids.

3. **Type invariants after monomorphization.** Post-mono, no `Param` or `Instance` type may remain. The driver reports `countRemainingGenerics()` on the `mono:` line.

4. **LIR verification failures.** `verifyProgram()` checks LIR well-formedness after lowering. New instructions have to satisfy it.

5. **Everything needs GCC.** Building a stage, running fixtures and running tests all compile C with `gcc`.

6. **Language-level snapshots are source-adjacent.** `std::test::Test::assertSnapshot` and `assertFileSnapshot` write to `__snapshots__/` next to the module under test. Project mode and single-file mode use different roots.

7. **Canonical Eq is test-critical.** Generic `Test::assertEq<T>` / `assertNe<T>` route through canonical `std::hash::Eq` and builtin `@eq<T>`. Unsupported equality must be rejected in analysis; supported paths must not rely on codegen panics.

8. **Two-pass binding.** Records, enums and type aliases are predeclared in the first pass (`predeclareRoots()`), then fully bound (`completeRoots()`). A new declaration type that can be referenced before its definition belongs in the first pass.

9. **Bidirectional type inference.** The typechecker propagates expected types downward. New expression forms have to decide whether they propagate or consume the expectation.

10. **Drop schedules.** The ownership check produces the schedules LIR lowering uses to emit drops. New control flow has to schedule drops at every exit (normal exit, break, continue, return), and the G7 baselines will show the change.

11. **The two-step rule.** Using a language feature in `ignis/` or `std/` in the same PR that adds it breaks the official stage0.

## Key Files

| File | Purpose |
| --- | --- |
| `ignis.toml` | The compiler's own project file (entry, std path, test fixtures) |
| `ignis/main.ign` | Driver: commands, compile pipeline, test and fixture runs, fmt/doc/lsp entry |
| `ignis/cli.ign` | Command-line parsing, help and version text |
| `ignis/build/resolver.ign` | Project and module discovery, test module selection |
| `ignis/build/pipeline.ign` | Build pipeline helpers |
| `ignis/build/test_runner.ign` | Test planning, partitioning, execution, reporting |
| `ignis/build/fixture_tests.ign` | E2E fixture headers and baselines |
| `ignis/build/c_compiler.ign` | C compilation |
| `ignis/build/linker.ign` | Linking |
| `ignis/config/project.ign` | `ignis.toml` loading |
| `ignis/lexer/mod.ign` | Lexer |
| `ignis/parser/declarations.ign` | Top-level parsing (functions, records, enums, traits, imports) |
| `ignis/parser/expressions.ign` | Expression parsing |
| `ignis/parser/statements.ign` | Statement parsing |
| `ignis/parser/types.ign` | Type annotation parsing |
| `ignis/analyzer/mod.ign` | Analyzer entry, phase order (`analyzeProgram`) |
| `ignis/analyzer/binder.ign` | Binding (two-pass) |
| `ignis/analyzer/resolver.ign` | Name resolution |
| `ignis/analyzer/typecheck_exprs.ign` | Expression typechecking, builtins |
| `ignis/analyzer/types.ign` | Semantic types and the type store |
| `ignis/analyzer/definitions.ign` | Definitions and the definition store |
| `ignis/analyzer/lowering.ign` | AST → HIR lowering |
| `ignis/analyzer/capture.ign` | Closure capture analysis |
| `ignis/analyzer/escape.ign` | Closure escape analysis |
| `ignis/analyzer/mono.ign` | Monomorphization |
| `ignis/analyzer/borrowck.ign` | Borrow and ownership checking, drop schedules |
| `ignis/analyzer/lint.ign` | Lints |
| `ignis/hir/node.ign` | HIR node kinds and store |
| `ignis/hir/pattern.ign` | HIR patterns |
| `ignis/hir/drop_schedule.ign` | Drop schedules |
| `ignis/lir/instr.ign` | LIR instructions |
| `ignis/lir/block.ign` | Basic blocks and terminators |
| `ignis/lir/lowering.ign` | HIR → LIR lowering |
| `ignis/lir/verify.ign` | LIR verification |
| `ignis/codegen/mod.ign` | C emission |
| `ignis/diagnostics/codes.ign` | Diagnostic codes |
| `ignis/format/api.ign` | Formatter entry |
| `ignis/doc/mod.ign` | `ignis doc` extraction |
| `ignis/lsp/server.ign` | Language server request handling |
| `ignis/lsp/at_items.ign` | Registry of `@`-prefixed builtins and directives for the language server |
| `scripts/bootstrap.sh` | Bootstrap ladder and gates |
| `scripts/build_from_seed.sh` | Rebuild stage0 from the C seed |
| `scripts/resolve_official_stage0.sh` | Resolve the official stage0 binary |
| `bootstrap/seed/manifest.json` | The C seed's source commit, checksums and gcc recipe |
| `std/manifest.toml` | Std module registry and linking config |
| `std/test/mod.ign` | `std::test::Test` namespace: assertions and snapshots |
| `std/runtime/ignis_rt.h` | Runtime type prelude guard; the runtime is implemented in std (memory, string, process, fs) |
| `std/string/mod.ign` | String runtime: buffer, UTF-8, search, conversions |

## UTF-8 String/Char Semantics (v0.4)

Ignis v0.4 defines string types as UTF-8 byte-backed:

| Type | Representation | C equivalent |
| --- | --- | --- |
| `char` | One Unicode scalar value | `ignis_char_t` |
| `str` | UTF-8 NUL-terminated byte slice | `const char*` |
| `String` | Heap-backed UTF-8 byte buffer (data + len + cap) | `IgnisString` |

Key rules:
- Char literals must resolve to exactly one Unicode scalar.
- Empty literals, multi-scalar literals, and surrogate escapes are rejected.
- `String::forEach` and `map` iterate over `char` by default; `forEachByte`/`mapBytes` for explicit `u8`.
- `String → str` via `toStr()` (zero-copy view); `str → String` via `String::create(s)` (copy).

## Main Wrapper

The compiler generates a C `main()` wrapper around the user's `main` function:

- User `main` is emitted as `__ignis_user_main`.
- The wrapper calls it and handles the return value.

Supported signatures:
- `main(): i32` — exit code returned directly.
- `main(): void` — wrapper returns 0.
- `main(): Result<i32, E>` — OK unwraps the exit code; ERROR prints a panic message and calls `exit(101)`.
- `main(argc: i32, argv: *str)` — argc/argv forwarded from C main.

## Closures

Closures compile through a multi-stage pipeline:

1. **Capture analysis** (`ignis/analyzer/capture.ign`) — determines which outer variables a closure captures and the capture mode (by ref, by move, by ref-mut).
2. **Escape analysis** (`ignis/analyzer/escape.ign`) — determines if a closure outlives its defining scope. `@noescape` on parameters prevents escape propagation.
3. **HIR lowering** — the closure node carries its captures, thunk/drop definitions, and whether it escapes.
4. **LIR lowering** — emits `MakeClosure` (captures → env struct), `CallClosure` (indirect call through thunk), `DropClosure` (cleanup), defined in `ignis/lir/instr.ign`.
5. **C codegen** — non-escaping closures use stack-allocated env; escaping closures use heap-allocated env. Closure values are structs with `call` (thunk fn ptr), `drop` (optional drop fn ptr), and `env` (opaque `*u8`).
