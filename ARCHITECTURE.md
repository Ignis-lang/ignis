## Overview

Ignis is a general-purpose, statically typed language that compiles Ignis source to C and links native binaries via GCC. The repository is a Rust workspace containing compiler crates, LSP support, standard library sources, and a C runtime.

## Tech Stack

- **Languages:** Rust (compiler, LSP), Ignis (`.ign` sources, standard library), C (runtime, generated output).
- **Tooling:** Cargo workspace, GCC for C compilation/linking, `ar` for static archives, `make` for runtime rebuilds.
- **Testing:** `insta` for snapshot tests, `proptest` for property-based tests, `tempfile` for compilation sandboxes.
- **LSP:** `tower-lsp` with Tokio async runtime.
- **CLI:** `clap` with derive macros and typed subcommands.

## Directory Structure

```
crates/
  ignis/                          # CLI entry point
    src/main.rs                   # Subcommand routing, config resolution
    src/cli.rs                    # Clap CLI definition
  ignis_driver/                   # Build pipeline orchestration
    src/pipeline.rs               # Full pipeline: analysis → mono → LIR → codegen → link
    src/context.rs                # Module discovery, import resolution, per-module parsing
    src/link.rs                   # GCC compilation, archive creation, executable linking
    tests/e2e_ok.rs               # E2E tests (compile + run)
    tests/e2e_err.rs              # E2E error tests (runtime failures)
    tests/common/mod.rs           # Shared test helpers
  ignis_parser/                   # Lexer + parser
    src/lexer/                    # Token scanner
    src/parser/mod.rs             # Parser core, delimited list helpers
    src/parser/declarations.rs    # Functions, records, enums, imports, attributes
    src/parser/expression.rs      # Pratt-precedence expression parsing
    src/parser/statement.rs       # Statement parsing
    src/parser/type_syntax.rs     # Type annotation parsing
    src/parser/recovery.rs        # Error recovery strategies
  ignis_analyzer/                 # Semantic analysis (7 phases)
    src/lib.rs                    # Analyzer struct, phase dispatch, entry points
    src/binder.rs                 # Phase 1: definition creation (two-pass)
    src/resolver.rs               # Phase 2: name resolution, scope building
    src/typeck.rs                 # Phase 3: type inference and checking
    src/borrowck.rs               # Phase 4: ownership and borrowing analysis
    src/const_eval.rs             # Phase 5: compile-time constant evaluation
    src/checks.rs                 # Phase 6: extra semantic checks (control flow, etc.)
    src/lint.rs                   # Phase 7: lint warnings
    src/lowering.rs               # AST → HIR conversion
    src/mono.rs                   # Monomorphization (generic specialization)
    src/scope.rs                  # Scope tree (Global, Function, Block, Generic, Loop)
    src/imports.rs                # Module import/export handling
    src/ownership_hir.rs          # HirOwnershipChecker, drop schedule generation
    tests/golden_ok.rs            # Analyzer snapshot tests (valid programs)
    tests/golden_err.rs           # Analyzer snapshot tests (error programs)
    tests/golden_hir.rs           # HIR output snapshots
    tests/fixtures.rs             # Tests from test_cases/ .ign files
    tests/diagnostics.rs          # Error code + line number assertions
    tests/properties.rs           # Property-based fuzz tests (proptest)
  ignis_hir/                      # High-level IR
    src/lib.rs                    # HIRNode, HIRKind, HIR store
    src/statement.rs              # Loop condition types
    src/operation.rs              # BinaryOperation, UnaryOperation
    src/drop_schedule.rs          # DropSchedules, ExitKey
    src/display.rs                # HIR pretty printer
  ignis_lir/                      # Low-level IR (TAC / basic blocks)
    src/instr.rs                  # Instruction types (Load, Store, BinOp, Call, etc.)
    src/operand.rs                # Operand types (Immediate, Temporary, Local, FunctionRef)
    src/block.rs                  # Block, Terminator (Jump, CondJump, Return, Unreachable)
    src/program.rs                # FunctionLir, LirProgram, LocalData, TempData
    src/lowering/mod.rs           # LoweringContext: HIR → LIR conversion
    src/lowering/builder.rs       # FunctionBuilder: block/instruction emission
    src/verify.rs                 # LIR verification
  ignis_codegen_c/                # C code generation
    src/emit.rs                   # CEmitter: LIR → C source code
    src/classify.rs               # Definition classification (User/Std/Runtime)
    tests/golden_c.rs             # C codegen snapshot tests
  ignis_ast/                      # AST node types
    src/statements/mod.rs         # ASTStatement enum (Variable, Function, Record, Enum, etc.)
    src/expressions/mod.rs        # ASTExpression enum (Binary, Call, MemberAccess, etc.)
    src/type_.rs                  # IgnisTypeSyntax (parsed type annotations)
    src/attribute.rs              # ASTAttribute, ASTAttributeArg
    src/metadata.rs               # ASTMetadata bitflags (STATIC, MUTABLE, EXPORT, etc.)
    src/generics.rs               # ASTGenericParams, ASTGenericParam
  ignis_type/                     # Type system and definitions
    src/types.rs                  # Type enum, TypeId, TypeStore, Substitution
    src/definition.rs             # Definition, DefinitionKind, DefinitionId, DefinitionStore
    src/symbol.rs                 # SymbolId, SymbolTable (interned identifiers)
    src/attribute.rs              # RecordAttr, FunctionAttr, FieldAttr
    src/lint.rs                   # LintId, LintLevel
    src/span.rs                   # Source locations
    src/module.rs                 # ModuleId, module metadata
    src/namespace.rs              # NamespaceId, NamespaceStore
    src/value.rs                  # Literal values (IgnisLiteralValue)
  ignis_token/                    # Token types
    src/token_types.rs            # Token enum (keywords, punctuation, literals)
  ignis_diagnostics/              # Error reporting
    src/message.rs                # DiagnosticMessage enum (450+ variants)
    src/diagnostic_report.rs      # Diagnostic, Severity, Label
  ignis_log/                      # Build output formatting
    src/lib.rs                    # cmd_header!, phase_log!, cmd_ok!, cmd_fail! macros
  ignis_lsp/                      # Language Server
    src/lib.rs                    # Async server setup (Tokio + tower-lsp)
    src/server.rs                 # LanguageServer trait impl (hover, goto-def, refs, etc.)
    src/state.rs                  # Document state, project caching (RwLock)
    src/completion.rs             # Token-based completion (works without valid AST)
    src/convert.rs                # UTF-16 position conversion
    src/semantic.rs               # Semantic token classification
    src/inlay.rs                  # Parameter name inlay hints
    src/project.rs                # ignis.toml discovery and project caching
std/                              # Ignis standard library
  manifest.toml                   # Module registry and linking configuration
  io/mod.ign                      # Print functions (println, print, eprintln, eprint)
  string/mod.ign                  # String utilities (length, concat, substring, contains, etc.)
  memory/mod.ign                  # Allocation (allocate, free, reallocate, copy, move)
  memory/align.ign                # Alignment utilities
  memory/layout.ign               # Memory layout
  vector/mod.ign                  # Vector<T> (growable array with push, pop, at, etc.)
  math/mod.ign                    # Math functions (sin, cos, sqrt, pow, floor, ceil, etc.)
  types/mod.ign                   # Option<T>, Result<T, E>
  libc/mod.ign                    # C standard library wrappers
  ptr/mod.ign                     # Pointer utilities
  runtime/                        # C runtime implementation
    ignis_rt.h                    # Runtime API header
    libignis_rt.a                 # Precompiled runtime archive
    Makefile                      # Runtime build system
    internal/rt_memory.c          # malloc/free/realloc wrappers
    internal/rt_string.c          # IgnisString operations
    internal/rt_io.c              # stdout/stderr printing
test_cases/analyzer/              # Ignis fixture files organized by feature
  borrows/                        # Borrow checking tests
  casts/                          # Cast validation tests
  generics/                       # Generic type tests
  missing_return/                 # Return path analysis tests
  mutability/                     # Mutability checks
  ownership/                      # Ownership transfer tests
  unreachable/                    # Unreachable code tests
example/                          # Example Ignis programs
docs/                             # Language reference and ABI docs
```

## Core Components

### CLI

**Entry:** `crates/ignis/src/main.rs`

Parses commands via `clap`, resolves compile input (project `ignis.toml` or single file), and builds `IgnisConfig` with CLI overrides (`opt_level`, `debug`, `out_dir`, `std_path`, `cc`, `emit`).

Subcommands: `build`, `check`, `build-std`, `check-std`, `check-runtime`, `lsp`.

### Driver Pipeline

**Entry:** `crates/ignis_driver/src/pipeline.rs` → `compile_project()`

Orchestrates the full compilation pipeline. Two codegen paths:

- **Module-based** (when precompiled std archive exists): per-module header generation, per-module C emission with fingerprint-based caching, user archive creation, final linking.
- **Legacy single-file**: all code emitted to one C file, compiled directly.

Caching uses stamp files with `BuildFingerprint` (compiler version + ABI version + source hashes) to skip recompilation of unchanged modules.

### Module Discovery

**Entry:** `crates/ignis_driver/src/context.rs` → `CompilationContext`

- `discover_modules()` builds a module dependency graph starting from the entry file.
- `discover_recursive()` follows imports to discover transitive dependencies.
- `discover_std_module()` resolves `std::module_name` via `manifest.toml`.
- `parse_file()` runs lexer → parser for each discovered file.
- Modules are analyzed in topological order (dependencies first).

### Parser

**Entry:** `crates/ignis_parser/src/parser/`

Recursive-descent parser with Pratt-style operator precedence for expressions.

- **Pratt parsing:** uses binding power tuples `(lbp, rbp)` where low = low precedence.
- **Type argument disambiguation:** lookahead heuristic to distinguish `<` as comparison vs generic args.
- **Attribute collection:** `@name` and `@name(args)` parsed into `pending_attrs`, consumed by the next declaration.
- **Error recovery:** synchronizes to declaration boundaries on parse errors.
- **Recursion safety:** `MAX_RECURSION_DEPTH = 500` prevents stack overflow.

### Analyzer

**Entry:** `crates/ignis_analyzer/src/lib.rs` → `Analyzer::analyze_with_shared_stores()`

Seven sequential phases over the AST:

| Phase | File | Purpose |
| --- | --- | --- |
| 1. Binding | `binder.rs` | Two-pass: predeclare types (pass 1), then complete all definitions (pass 2). Enables forward references. |
| 2. Resolution | `resolver.rs` | Resolve identifiers to `DefinitionId` via scope lookup. Build scope tree. |
| 3. Type Checking | `typeck.rs` | Bidirectional type inference. Propagates expected types downward via `InferContext`. Handles overload resolution. |
| 4. Borrow Checking | `borrowck.rs` | Validate ownership, moves, borrows. Detect use-after-move and conflicting borrows. |
| 5. Const Eval | `const_eval.rs` | Evaluate constant expressions at compile time. Populate `ConstantDefinition::value`. |
| 6. Extra Checks | `checks.rs` | Control flow analysis (missing returns, unreachable code, never-typed expressions). |
| 7. Lints | `lint.rs` | `UnusedVariable`, `UnusedImport`, `Deprecated`. Respects `@allow`/`@warn`/`@deny` directives. |

After all phases, `lower_to_hir()` converts the typed AST into HIR.

**Output:** `AnalyzerOutput` containing `TypeStore`, `DefinitionStore`, `HIR`, diagnostics, and lookup maps (`node_defs`, `node_types`, `node_spans`, `resolved_calls`) used by the LSP.

### Monomorphization

**Entry:** `crates/ignis_analyzer/src/mono.rs` → `Monomorphizer`

Transforms generic HIR into concrete HIR with all type parameters resolved:

1. **Discovery** — scan HIR for generic instantiations (calls with type args, record inits, method calls).
2. **Shell creation** — create `DefinitionId` entries with mangled names for each concrete instance.
3. **Body substitution** — clone generic bodies, replacing `Type::Param` with concrete types via `Substitution`.
4. **Fixpoint** — repeat until no new instantiations are discovered.

**Invariant:** post-mono, no `Type::Param` or `Type::Instance` should exist. Debug builds verify with `verify_no_generics()`.

### Ownership Analysis

**Entry:** `crates/ignis_analyzer/src/ownership_hir.rs` → `HirOwnershipChecker`

Runs on monomorphized HIR. Validates move semantics and reference rules. Produces `DropSchedules` that map HIR nodes to their exit points where cleanup code should be emitted (end of block, break, continue, return).

### HIR

**Location:** `crates/ignis_hir/src/`

Tree-based intermediate representation preserving program structure. Each `HIRNode` has a `HIRKind` (operation), `Span` (source location), and `TypeId` (inferred type). Uses `DefinitionId` references instead of names.

Key `HIRKind` categories:
- **Expressions:** `Literal`, `Variable`, `Binary`, `Unary`, `Call`, `Cast`, `Reference`, `Dereference`, `Index`, `FieldAccess`, `MethodCall`, `EnumVariant`, `RecordInit`.
- **Statements:** `Let`, `Assign`, `Block`, `If`, `Loop`, `Break`, `Continue`, `Return`.
- **Builtins:** `SizeOf`, `AlignOf`, `Panic`, `Trap`, `BuiltinUnreachable`, `BuiltinLoad`, `BuiltinStore`.

### LIR

**Location:** `crates/ignis_lir/src/`

Three-address code (TAC) with basic block structure. Each instruction has at most one operation with results stored in temporaries (`TempId`) or locals (`LocalId`).

- **Instructions:** `Load`, `Store`, `LoadPtr`, `StorePtr`, `Copy`, `BinOp`, `UnaryOp`, `Cast`, `BitCast`, `Call`, `RuntimeCall`, `GetElementPtr`, `InitVector`, `SizeOf`, `AlignOf`, `AddrOfLocal`.
- **Terminators:** `Jump`, `CondJump`, `Return`, `Unreachable`.
- **Program structure:** `LirProgram` contains `HashMap<DefinitionId, FunctionLir>`. Each `FunctionLir` has an entry block, a store of `Block`s, `LocalData`, and `TempData`.

**Lowering:** `LoweringContext` (in `lowering/mod.rs`) converts HIR to LIR, managing block creation, control flow, drop scheduling, and temporary allocation. `FunctionBuilder` (in `lowering/builder.rs`) emits instructions into blocks.

**Verification:** `verify.rs` checks LIR well-formedness after lowering.

### C Codegen

**Entry:** `crates/ignis_codegen_c/src/emit.rs` → `CEmitter`

Emission pipeline:
1. `emit_implicit_headers()` — stdio, math, string as needed.
2. `emit_headers()` — user-provided C headers from `LinkPlan`.
3. `emit_type_forward_declarations()` — forward struct/enum declarations.
4. `emit_type_definitions()` — full struct and tagged union definitions.
5. `emit_static_constants()` — global constants.
6. `emit_extern_declarations()` — external C function prototypes.
7. `emit_forward_declarations()` — function prototypes.
8. `emit_functions()` — function implementations.

Type representation in C:
- **Records** → named structs with mangled names.
- **Enums** → tagged unions (tag field + payload union). Each variant gets a `#define TAG_VARIANT value`.
- **Generic types** → only fully monomorphized instances emitted.

**Classification:** `classify.rs` determines whether a definition is User, Std, or Runtime code. Emission targets (`EmitTarget`) control which definitions are included: `User`, `StdModule(name)`, `UserModule(ModuleId)`, or `All`.

Attribute mapping to C:
- `@packed` → `__attribute__((packed))`
- `@aligned(N)` → `__attribute__((aligned(N)))`
- `@cold` → `__attribute__((cold))`
- `@inline(always)` → `__attribute__((always_inline)) inline`
- `@inline(never)` → `__attribute__((noinline))`
- `@externName("name")` → uses the specified C symbol name.

### Linking

**Entry:** `crates/ignis_driver/src/link.rs`

Three stages:
1. **Object compilation:** `gcc -c <input.c> -o <output.o> -I <include_dirs>`.
2. **Archive creation:** `ar rcs <archive.a> <obj1.o> <obj2.o> ...`.
3. **Executable linking:** `gcc <objects> <user.a> <std.a> <runtime_objects> -o binary -l<libs>`.

Link order is critical: user objects → user archive → std archive → runtime objects → external libraries.

`LinkPlan` carries headers, objects, archives, libs, and include dirs through the pipeline.

### LSP

**Entry:** `crates/ignis_lsp/src/`

Capabilities: diagnostics, hover (type info + docs), go-to-definition, find references, completions, semantic tokens, inlay hints (parameter names), workspace symbols, document symbols, file watching.

Key design decisions:
- **Token-based completion** (`completion.rs`): detects context from tokens (after `.`, `::`, in imports, in record init) without requiring a valid AST. Works even when the file has parse errors.
- **Panic safety:** analysis wrapped in `catch_unwind()` to prevent server crashes.
- **Caching:** analysis results cached per document version. Last good analysis preserved for incomplete code.
- **File overrides:** open file content collected from LSP state, overriding disk content for analysis.

### Standard Library

**Registry:** `std/manifest.toml` maps module names to `.ign` files and declares linking requirements (headers, archives, `-l` flags).

Key modules:

| Module | Provides |
| --- | --- |
| `io` | `println`, `print`, `eprintln`, `eprint` |
| `string` | `length`, `concat`, `substring`, `contains`, `toUpperCase`, `toLowerCase`, `toString` overloads |
| `memory` | `allocate<T>`, `free<T>`, `reallocate<T>`, `copy<T>`, `move<T>`, `Layout`, `Align` |
| `vector` | `Vector<T>` with `init`, `push`, `pop`, `at`, `free`, `clear`, `shrink` |
| `math` | `sin`, `cos`, `sqrt`, `pow`, `floor`, `ceil`, `round`, constants (`PI`, `E`, `TAU`) |
| `types` | `Option<T>` |
| `libc` | C standard library wrappers |
| `ptr` | Pointer utilities |

Std modules use `extern namespace` declarations backed by C runtime functions.

### C Runtime

**Location:** `std/runtime/`

Provides:
- **Memory:** `ignis_alloc`, `ignis_free`, `ignis_realloc`, `ignis_calloc`, `ignis_memcpy`, `ignis_memmove`.
- **Strings:** `IgnisString` heap type with `ignis_string_new`, `ignis_string_from_cstr`, `ignis_string_concat`, `ignis_string_substring`, etc.
- **I/O:** `ignis_print`, `ignis_eprint`.
- **Type conversions:** `ignis_i32_to_string`, `ignis_f64_to_string`, etc.

Built via `Makefile` → `libignis_rt.a`.

## Data Flow

```
CLI args → IgnisConfig
             ↓
     CompilationContext
       discover_modules()         → ModuleGraph + ParsedModules (AST per file)
             ↓
     Analyzer (per module, topological order)
       bind_phase()               → DefinitionStore
       resolve_phase()            → node_defs, ScopeTree
       typecheck_phase()          → node_types, TypeStore
       borrowcheck_phase()        → ownership diagnostics
       const_eval_phase()         → ConstantDefinition::value
       extra_checks_phase()       → control flow diagnostics
       lint_phase()               → lint diagnostics
       lower_to_hir()             → HIR
             ↓
     AnalyzerOutput (types, defs, hir, diagnostics, lookup maps)
             ↓
     Monomorphizer::run()         → concrete HIR (no Type::Param/Instance)
             ↓
     HirOwnershipChecker::check() → DropSchedules
             ↓
     lower_and_verify()           → LirProgram (basic blocks, TAC)
             ↓
     CEmitter::emit_*()           → C source (.c) + headers (.h)
             ↓
     gcc -c                       → object files (.o)
     ar rcs                       → archives (.a)
     gcc link                     → executable binary
```

### Build Layout

```
build/
  bin/          # Linked executables
  obj/          # Object files
  c/            # Generated C source

build/std/      # Precompiled standard library (ignis build-std)
  include/      # ignis_std.h + per-module .h files
  lib/          # libignis_std.a
  obj/          # Per-module .o files
  src/          # Per-module .c files

build/user/     # User modules (module-based compilation)
  include/      # Per-module .h files
  lib/          # libignis_user.a
  obj/          # Per-module .o files
  src/          # Per-module .c files
```

## Configuration

| File | Purpose |
| --- | --- |
| `Cargo.toml` | Workspace members, shared dependencies |
| `ignis.toml` | Project config: `std_path`, `source_dir`, `target`, `optimize`, `output_dir` |
| `std/manifest.toml` | Std module registry, linking config (headers, archives, `-l` flags) |
| `IGNIS_STD_PATH` env | Overrides standard library root path |
| `.rustfmt.toml` | 2-space indent, 120 width, no import reorder, vertical params |
| `.editorconfig` | LF line endings, 2-space indent |
