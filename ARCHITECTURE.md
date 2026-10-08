## Overview

Ignis is a general-purpose, statically typed language that compiles Ignis source to C and links native binaries via GCC. The compiler is written in Ignis (`ignis/`) and builds itself through the bootstrap ladder described in `BOOTSTRAP.md`, starting from the promoted official binary or from the C seed committed under `bootstrap/seed/`.

`crates/` holds the former Rust compiler. It is frozen, kept for reference only, and nothing builds or runs it (see `crates/README.md`).

## Tech Stack

- **Languages:** Ignis (compiler, language server, standard library), C (generated output, the `ignis_rt.h` type header).
- **Tooling:** GCC for C compilation/linking, `ar` for static archives, `scripts/bootstrap.sh` (bash, python3) for the bootstrap ladder and its gates.
- **Testing:** native `ignis test` for the compiler's own `@test` functions and the e2e fixtures, `ignis test-std` for the standard library, committed baselines checked by the ladder's gates.

## Directory Structure

```
ignis/                            # The compiler (project file: ignis.toml)
  main.ign                        # Driver: command dispatch, compile pipeline, test runs, fmt/doc/lsp entry
  cli.ign                         # Command-line parsing and help text
  init.ign                        # ignis init: project scaffolding
  lexer/                          # Lexer
  syntax/                         # Tokens, token stream, cursor
  parser/                         # Recursive-descent parser with binding-power expression parsing
    declarations.ign              # Functions, records, enums, traits, imports, attributes
    expressions.ign               # Expression parsing
    statements.ign                # Statement parsing
    types.ign                     # Type annotation parsing
    patterns.ign                  # Pattern parsing
    recovery.ign                  # Error recovery
  ast/                            # AST node types (items, statements, expressions, types, patterns, metadata)
  analyzer/                       # Semantic analysis
    mod.ign                       # Analyzer entry and phase order
    binder.ign                    # Definition creation (two-pass)
    resolver.ign                  # Type-level name resolution
    resolver_bodies.ign           # Body-level name resolution
    typecheck*.ign                # Type inference and checking, exhaustiveness
    const_eval.ign                # Compile-time constant evaluation
    checks.ign                    # Post-typecheck checks (control flow, etc.)
    lint.ign                      # Lint warnings
    imports.ign                   # Module import/export handling
    types.ign                     # Semantic types and the type store
    definitions.ign               # Definitions and the definition store
    lowering.ign                  # AST → HIR conversion
    capture.ign                   # Closure capture analysis
    escape.ign                    # Closure escape analysis
    mono.ign                      # Monomorphization
    borrowck.ign                  # Borrow and ownership checking, drop schedules
  hir/                            # High-level IR: node kinds, patterns, drop schedules, display
  lir/                            # Low-level IR: instructions, blocks, program, HIR → LIR lowering, verification
  codegen/                        # LIR → C emission
  build/                          # Discovery, build cache, C compiler/archiver/linker calls, test runner, fixtures
  config/                         # ignis.toml loading
  diagnostics/                    # Diagnostic codes, model, rendering
  output/                         # The only writer of stdout/stderr: messages (stderr) and data (stdout)
  format/                         # Canonical source formatter
  doc/                            # API documentation extraction
  lsp/                            # Language server
std/                              # Ignis standard library
  manifest.toml                   # Module registry and linking configuration
  io/mod.ign                      # Print functions (println, print, eprintln, eprint) + IoError
  string/mod.ign                  # String utilities (length, concat, forEach, map, etc.)
  memory/mod.ign                  # Allocation (allocate, free, reallocate, copy, move)
  vector/mod.ign                  # Vector<T> (growable array with push, pop, at, etc.)
  math/mod.ign                    # Math functions (sin, cos, sqrt, pow, floor, ceil, etc.)
  number/mod.ign                  # Numeric helpers (abs, toFixed, round, floor, ceil)
  types/mod.ign                   # Runtime type IDs
  option/mod.ign                  # Option<S> (SOME/NONE)
  result/mod.ign                  # Result<T, E> (OK/ERROR)
  rc/mod.ign                      # Rc<T> and Weak<T> (reference-counted pointers)
  libc/mod.ign                    # C standard library wrappers
  ptr/mod.ign                     # Pointer utilities
  ffi/mod.ign                     # FFI utilities (CString)
  fs/mod.ign                      # Filesystem (readToString, writeString, Dir, File, Metadata)
  path/mod.ign                    # Path manipulation utilities
  test/mod.ign                    # `std::test::Test` assertions and snapshot helpers
  runtime/
    ignis_rt.h                    # Base type header; no C is compiled any more
bootstrap/seed/                   # The compiler as one C file (xz), its header and manifest
scripts/                          # Bootstrap ladder, gates, parity harnesses, installer
test_cases/                       # Fixtures and committed baselines
  e2e/{ok,err}/                   # End-to-end fixtures and their snapshots
  __parse_verdicts__/             # G6 parse-verdict baselines
  doc/                            # ignis doc baselines
  analyzer/ parser/ selfhost/     # Inputs read by the compiler's own tests
example/                          # Example Ignis programs
docs/                             # Language reference and ABI docs
crates/                           # Frozen Rust compiler, reference only
```

## Core Components

### CLI

**Entry:** `ignis/cli.ign` → `parseArguments()`, dispatched by `ignis/main.ign`

Parses the command line, then resolves the input (project `ignis.toml` or single file) and applies CLI overrides (`opt_level`, `debug`, `out_dir`, `std_path`, `cc`, target triple, features).

Commands: `build`, `check`, `build-std`, `check-std`, `test`, `test-std`, `fmt`, `lsp`, `doc`, `init`. `ignis --help` lists every flag.

### Formatter

**Entry:** `ignis/format/api.ign`, called from `ignis/main.ign` for `ignis fmt`

The formatter is a parser-aware, safety-validated canonical rewriter for Ignis source.

- Input modes: project, single file, multiple explicit files, and NDJSON batch stdin.
- Canonical empty high-level blocks emit inline (`namespace Foo {}`).
- Trailing commas are layout-driven: multiline call/initializer lists gain a final comma, single-line canonical output drops it.
- Safety validation (`ignis/format/safety.ign`) checks that the output parses back and is stable.
- Import sorting is opt-in only via formatter config or `--sort-imports`.

### Driver Pipeline

**Entry:** `ignis/main.ign`

Orchestrates discovery, parsing, analysis, the post-analysis passes, codegen and linking. A program is emitted as one C translation unit; `build-std` emits the standard library as one unit (`ignis_std.c`) and archives it.

Caching uses stamp files (`ignis/build/stamp.ign`) with a build fingerprint (`ignis/build/fingerprint.ign`) to skip rebuilding unchanged artifacts. `--force` bypasses it.

### Native Test Runner

**Entries:** `ignis/build/test_runner.ign` and `ignis/build/fixture_tests.ign`, driven by `ignis/main.ign`

The native test runner reuses the normal analysis → mono → LIR → codegen → link pipeline, but swaps the normal entry wrapper for a generated test harness. The harness executes discovered `@test` functions one by one, preserves deterministic ordering, continues after failures, and forwards snapshot context through environment variables. Test modules are `tests.ign` and files ending in `_tests.ign` (and a few other suffixes, see `ignis/build/resolver.ign`); a build leaves them out.

Runner responsibilities:

- discover `@test` functions from analyzed modules
- build deterministic fully-qualified test names
- support project-mode and single-file mode
- run the e2e fixtures listed under `[test] fixtures` in `ignis.toml`
- execute tests through a harness binary, optionally sharded (`--partition`)
- expose snapshot context (`IGNIS_TEST_NAME`, `IGNIS_TEST_SNAPSHOT_DIR`, `IGNIS_TEST_UPDATE_SNAPSHOTS`)
- report failures with bounded stderr/stdout detail

### Module Discovery

**Entry:** `ignis/build/resolver.ign`, `ignis/build/module_graph.ign`

- Builds the module graph starting from the entry file, following imports transitively.
- Resolves `std::module_name` through `std/manifest.toml`.
- Modules are analyzed in topological order (dependencies first).

### Parser

**Entry:** `ignis/parser/`

Recursive-descent parser. Expressions use binding powers (`bindingPower()` in `ignis/parser/expressions.ign`) for operator precedence. Errors are recovered from (`ignis/parser/recovery.ign`) so a file with parse errors still yields diagnostics for the rest.

### Analyzer

**Entry:** `ignis/analyzer/mod.ign` → `analyzeProgram()`

Sequential phases over each module's AST:

| Phase | File | Purpose |
| --- | --- | --- |
| 1. Binding | `binder.ign` | Two-pass: predeclare types (`predeclareRoots()`), then complete all definitions (`completeRoots()`). Enables forward references. |
| 2. Type resolution | `resolver.ign` | Resolve names in type positions and declarations. |
| 3. Body resolution | `resolver_bodies.ign` | Resolve identifiers in function bodies. |
| 4. Type checking | `typecheck*.ign` | Bidirectional type inference, overload resolution, exhaustiveness. |
| 5. Const eval | `const_eval.ign` | Evaluate constant expressions at compile time. |
| 6. Checks | `checks.ign` | Control flow analysis (missing returns, unreachable code) and other post-typecheck checks. |
| 7. Lints | `lint.ign` | Unused variable, unused import, unused `mut`, deprecated call. Respects `@allow`/`@warn`/`@deny`. |

After analysis, `lowering.ign` converts the typed AST into HIR, and **capture analysis** (`capture.ign`) and **escape analysis** (`escape.ign`) run for closures.

### Monomorphization

**Entry:** `ignis/analyzer/mono.ign`

Transforms generic HIR into concrete HIR with all type parameters resolved, repeating until no new instantiation is discovered.

**Invariant:** post-mono, no `Param` or `Instance` type may remain. The driver reports `countRemainingGenerics()` on its `mono:` line.

### Borrow and Ownership Checking

**Entry:** `ignis/analyzer/borrowck.ign`

Runs on monomorphized HIR. One fused pass validates borrows and moves (exclusive mutable borrows, use-after-move, conflicting borrows) and produces the drop schedules (`ignis/hir/drop_schedule.ign`) that tell LIR lowering where cleanup code goes (end of block, break, continue, return). `--dump-drop-schedule` prints them, and `--dump-ownership-report` prints the checker's decisions as JSON Lines.

### HIR

**Location:** `ignis/hir/`

Tree-based intermediate representation preserving program structure. Each HIR node has a kind, a source span and a type id, and refers to definitions by id instead of by name.

Key `HirKind` categories (`ignis/hir/node.ign`):
- **Expressions:** `Literal`, `Unit`, `Variable`, `Binary`, `Unary`, `Call`, `CallClosure`, `Closure`, `Cast`, `BitCast`, `Reference`, `Dereference`, `Index`, `VectorLiteral`, `TupleLiteral`, `MakeSlice`, `FieldAccess`, `MethodCall`, `EnumVariant`, `RecordInit`, `Match`, `StaticAccess`.
- **Statements:** `Let`, `LetElse`, `Assign`, `Block`, `If`, `Loop`, `Break`, `Continue`, `Return`, `Defer`, `ExpressionStatement`.
- **Patterns:** `HirPattern` (`Wildcard`, `Literal`, `Binding`, `Variant`, `Tuple`, `Or`, `Constant`) used by `Match`, `LetElse`, and let-conditions in `If`/`Loop`.
- **Builtins:** `TypeOf`, `SizeOf`, `AlignOf`, `MaxOf`, `MinOf`, `Panic`, `Trap`, `BuiltinUnreachable`, `BuiltinLoad`, `BuiltinStore`, `BuiltinHash`, `BuiltinEq`, `BuiltinDropInPlace`, `BuiltinDropGlue`.

### LIR

**Location:** `ignis/lir/`

Three-address code with basic block structure. Each instruction performs at most one operation, with results stored in temporaries or locals.

- **Instructions** (`instr.ign`): `Load`, `Store`, `LoadPtr`, `StorePtr`, `BuiltinLoad`, `BuiltinStore`, `BuiltinHash`, `BuiltinEq`, `Copy`, `BinOp`, `UnaryOp`, `Call`, `Cast`, `BitCast`, `AddrOfLocal`, `AddrOfGlobal`, `GetElementPtr`, `MakeSlice`, `InitVector`, `Nop`, `RuntimeCall`, `TypeIdOf`, `SizeOf`, `AlignOf`, `MaxOf`, `MinOf`, `Trap`, `PanicMessage`, `Drop`, `GetFieldPtr`, `InitRecord`, `InitEnumVariant`, `EnumGetTag`, `EnumGetPayloadField`, `EnumGetPayloadFieldPtr`, `DropInPlace`, `MarkMoved`, `DropGlue`, `MakeClosure`, `CallClosure`, `DropClosure`, `FreeEnv`.
- **Terminators** (`block.ign`): `Goto`, `Branch`, `Return`, `Unreachable`.

**Lowering:** `lowering.ign` converts HIR to LIR, managing block creation, control flow, drop scheduling, and temporary allocation.

**Verification:** `verify.ign` → `verifyProgram()` checks LIR well-formedness after lowering.

### C Codegen

**Entry:** `ignis/codegen/mod.ign` → `emitProgram()`

Type representation in C:
- **Records** → named structs with mangled names.
- **Enums** → tagged unions (tag field + payload union).
- **Generic types** → only fully monomorphized instances emitted.

Attribute mapping to C:
- `@packed` → `__attribute__((packed))`
- `@aligned(N)` → `__attribute__((aligned(N)))`
- `@externName("name")` → uses the specified C symbol name.

### Linking

**Entry:** `ignis/build/c_compiler.ign`, `ignis/build/archive.ign`, `ignis/build/linker.ign`

1. **Object compilation:** `gcc -c <input.c> -o <output.o> -I <std>/runtime` with the profile's flags.
2. **Archive creation** (`build-std`): `ar` over the standard library object.
3. **Executable linking:** `gcc <flags> <objects> -o <binary> -l<libs>`.

### LSP

**Entry:** `ignis/lsp/server.ign`, started by `ignis lsp` over stdin/stdout

Requests handled: diagnostics (`publishDiagnostics`), hover, go-to-definition, find references, rename, completions, document symbols, semantic tokens, inlay hints, formatting, code actions.

- **Completion** (`completion*.ign`) detects its context from tokens, so it works while the file has parse errors.
- **Documents** (`documents.ign`) hold open buffers, which override disk content for analysis.

### Standard Library

**Registry:** `std/manifest.toml` maps module names to `.ign` files and declares linking requirements (headers, archives, `-l` flags).

Key modules:

| Module | Provides |
| --- | --- |
| `io` | `println`, `print`, `eprintln`, `eprint`, `IoError`, `ErrorKind` |
| `string` | `length`, `concat`, `substring`, `contains`, `forEach`, `map`, `toUpperCase`, `toLowerCase`, `toString` overloads |
| `memory` | `allocate<T>`, `free<T>`, `reallocate<T>`, `copy<T>`, `move<T>`, `Layout`, `Align` |
| `vector` | `Vector<T>` with `init`, `push`, `pop`, `at`, `clear`, `shrink` (implements `Drop`) |
| `math` | `sin`, `cos`, `sqrt`, `pow`, `floor`, `ceil`, `round`, constants (`PI`, `E`, `TAU`) |
| `number` | Numeric helpers (`abs`, `toFixed`, rounding wrappers via extensions) |
| `types` | Runtime type IDs |
| `option` | `Option<S>` with `SOME`/`NONE`, helpers (`isSome`, `isNone`, `unwrap`, `unwrapOr`) |
| `result` | `Result<T, E>` with `OK`/`ERROR`, helpers (`isOk`, `isError`, `unwrap`, `unwrapOr`) |
| `rc` | `Rc<T>` (shared ownership), `Weak<T>` (non-owning observer) |
| `test` | `std::test::Test` namespace: generic assertions and snapshot helpers |
| `libc` | C standard library wrappers (memory, string, process, io, stdio, errno, misc, primitives) |
| `ptr` | Pointer utilities |
| `ffi` | FFI utilities: `CString` (owned NUL-terminated C string) |
| `fs` | Filesystem: `readToString`, `writeString`, `Dir`, `File`, `Metadata` (returns `Result<T, Io::IoError>`) |
| `path` | Path manipulation utilities |

**Auto-loaded modules** (always available without explicit import, `[auto_load]` in the manifest): `string`, `number`, `vector`, `types`, `option`, `result`, `format`, `process_runtime`.

Std modules reach the operating system through `extern` blocks bound directly to libc.

The canonical equality contract behind generic test assertions is `std::hash::Eq`. Generic `Test::assertEq<T>` / `assertNe<T>` route through builtin `@eq<T>` after analyzer validation, and unsupported equality must be rejected before codegen.

### Runtime

**Location:** `std/runtime/`

There is no C runtime. Memory, strings, number formatting, reference counting,
I/O and the filesystem syscall layer are Ignis, in `std/`, and link out of
`libignis_std.a`. What remains is `ignis_rt.h`, the base type header the
manifest names: the runtime type definitions under the `IGNIS_RT_TYPES_H`
guard, plus the declaration of `ignis_runtime_init`.

The compiler emits that same guarded block into every translation unit it
produces, so a unit that also includes the header keeps exactly one definition
of each name. `CodegenC::emitTypePrelude` in `ignis/codegen/mod.ign` must stay
byte-identical to it.

## Data Flow

```
CLI args (ignis/cli.ign) → options
             ↓
     discovery (ignis/build/resolver.ign)   → module graph, project settings
             ↓
     lex + parse (ignis/lexer/, ignis/parser/) → AST per module
             ↓
     Analyzer (per module, topological order, ignis/analyzer/mod.ign)
       binding → type resolution → body resolution → typecheck → const eval → checks → lints
             ↓
     lowering.ign                  → HIR (+ capture and escape analysis)
             ↓
     mono.ign                      → concrete HIR (no Param/Instance types)
             ↓
     borrowck.ign                  → borrow/ownership diagnostics, drop schedules
             ↓
     ignis/lir/lowering.ign + verify.ign → LIR program (basic blocks, TAC)
             ↓
     ignis/codegen/mod.ign         → one C translation unit
             ↓
     gcc -c                        → object file (.o)
     ar                            → archive (.a, build-std)
     gcc link                      → executable binary
```

### Build Layout

```
<out_dir>/                  # [build] out_dir, `build` by default
  bin/<name>                # Linked executable (project mode)
  user/src/<name>.c         # Generated C
  user/obj/<name>.o         # Object file, with its build stamp and report

build/std/                  # Standard library (ignis build-std)
  src/ignis_std.c           # The whole library as one C file
  obj/ignis_std.o
  lib/libignis_std.a
```

A single-file build (`ignis build file.ign`) writes `selfhost_emit.c` in the working directory and links `selfhost_out` unless `-o` names the binary.

### Main Wrapper

The compiler generates a C `main()` wrapper around the user's `main` function:

- User `main` is emitted as `__ignis_user_main`.
- The wrapper calls it and handles the return value.

Supported signatures:
- `main(): i32` — exit code returned directly.
- `main(): void` — wrapper returns 0.
- `main(): Result<i32, E>` — OK unwraps the exit code; ERROR prints a panic message and calls `exit(101)`.
- `main(argc: i32, argv: *str)` — argc/argv forwarded from C main.

### UTF-8 String/Char Semantics (v0.4)

| Type | Representation | C equivalent |
| --- | --- | --- |
| `char` | One Unicode scalar value | `ignis_char_t` |
| `str` | UTF-8 NUL-terminated byte slice | `const char*` |
| `String` | Heap-backed UTF-8 byte buffer (data + len + cap) | `IgnisString` |

Char literals must resolve to exactly one Unicode scalar. Empty literals, multi-scalar literals, and surrogate escapes are rejected.

### Closures

Closures compile through a multi-stage pipeline:

1. **Capture analysis** (`capture.ign`) — determines which outer variables a closure captures and the capture mode (by ref, by move, by ref-mut).
2. **Escape analysis** (`escape.ign`) — determines if a closure outlives its defining scope. `@noescape` on parameters prevents escape propagation.
3. **HIR** — the `Closure` node carries captures, thunk/drop definitions, and whether it escapes.
4. **LIR** — `MakeClosure` (captures → env struct), `CallClosure` (indirect call through thunk), `DropClosure` (cleanup), `FreeEnv`.
5. **C codegen** — non-escaping closures use stack-allocated env; escaping closures use heap-allocated env. Closure values are structs with `call` (thunk fn ptr), `drop` (optional drop fn ptr), and `env` (opaque `*u8`).

## Configuration

| File | Purpose |
| --- | --- |
| `ignis.toml` | Project config: `[package]`, `[build]` (`source_dir`, `entry`, `out_dir`, `opt_level`, `cc`), `[ignis]` (`std`, `std_path`), `[test]` |
| `std/manifest.toml` | Std module registry, linking config (headers, archives, `-l` flags) |
| `IGNIS_STD_PATH` env | Overrides standard library root path |
| `bootstrap/seed/manifest.json` | The C seed's source commit, checksums and gcc recipe |
| `.editorconfig` | LF line endings, 2-space indent |
