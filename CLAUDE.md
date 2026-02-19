Guidelines for AI agents working on the Ignis compiler.

## Project Overview

Ignis is a statically typed, general-purpose language that compiles to C and links native binaries via GCC. The compiler is written in Rust as a Cargo workspace.

**Workspace structure:**

```
crates/
  ignis/              # CLI entry point (clap)
  ignis_driver/       # Build pipeline orchestration, module discovery, linking
  ignis_parser/       # Lexer + recursive-descent parser with Pratt precedence
  ignis_analyzer/     # Binding, resolution, typechecking, borrowck, const eval, lints, HIR lowering
  ignis_hir/          # High-level IR (tree-based, typed)
  ignis_lir/          # Low-level IR (TAC / basic blocks), HIR->LIR lowering, verification
  ignis_codegen_c/    # LIR -> C emission (structs, tagged unions, functions)
  ignis_ast/          # AST node types (statements, expressions, types, attributes, patterns)
  ignis_type/         # Type system (Type, TypeStore, Definition, DefinitionStore, SymbolTable)
  ignis_token/        # Token types and lexer
  ignis_data_type/    # Legacy data type enum (used by ignis_token)
  ignis_config/       # Build configuration types (std manifest, linking info)
  ignis_diagnostics/  # Diagnostic messages, severity, rendering
  ignis_log/          # Build output macros (cmd_header!, phase_log!, cmd_ok!, cmd_fail!)
  ignis_lsp/          # Language Server (tower-lsp): diagnostics, hover, goto-def, completions
std/                  # Ignis standard library sources (.ign) + C runtime
  manifest.toml       # Module registry and linking configuration
  runtime/            # C runtime (ignis_rt.h, libignis_rt.a)
  fs/                 # Filesystem: readToString, writeString, Dir, File, Metadata
  ffi/                # FFI utilities: CString
  path/               # Path manipulation
  io/                 # Print functions + IoError types
  libc/               # C standard library wrappers (9 submodules)
test_cases/           # Ignis fixture files for analyzer tests
example/              # Example Ignis programs
```

## Build & Run Commands

```bash
# Compiler
cargo check                                    # Fast type check (all crates)
cargo build                                    # Debug build
cargo build --release                          # Release build
cargo install --path crates/ignis              # Install CLI

# Formatting & linting
cargo fmt --all                                # Format (2-space indent, 120 width)
cargo clippy -- -D warnings                    # Lint

# Testing
cargo test                                     # All tests (unit + integration + e2e)
cargo test -p ignis_driver                     # E2E tests (compile + run Ignis code)
cargo test -p ignis_analyzer                   # Analyzer tests (semantic analysis)
cargo test -p ignis_codegen_c                  # C codegen snapshot tests
cargo test -p ignis_driver e2e_arithmetic_add  # Single test by name

# Snapshot management (insta)
cargo insta review                             # Interactive review of changed snapshots
INSTA_UPDATE=always cargo test -p ignis_driver # Auto-accept all snapshot changes

# Compiling Ignis code
ignis build                                    # Compile project (reads ignis.toml)
ignis build path/to/file.ign                   # Compile single file
ignis check                                    # Type-check only (no codegen)
ignis build-std                                # Build standard library archive
```

## Rust Guidelines

### General Principles

- Prioritize correctness and clarity over speed.
- Do not write comments that summarize code; only explain non-obvious "why".
- Prefer implementing in existing files unless it's a new logical component.
- Use full words for variable names (no abbreviations like `q` for `queue`).

### Error Handling

- Avoid `unwrap()` and functions that panic; use `?` to propagate errors.
- Be careful with indexing operations that may panic on out-of-bounds.
- Driver/pipeline functions return `Result<(), ()>` and print errors via `eprintln!` + `colored`.
- Diagnostics are collected in `Vec<Diagnostic>` and rendered via `ignis_diagnostics::render()`.
- The LSP wraps analysis in `catch_unwind` to avoid crashing on panics.

### File Organization

- Never create `mod.rs` files; use `src/some_module.rs` instead.
- Rustfmt keeps import order as written (`.rustfmt.toml` sets `reorder_imports = false`).
- Imports are grouped: std, external, internal, separated by blank lines.

### Formatting

- 2-space indentation, `max_width = 120`.
- Vertical function parameter layout (`fn_params_layout = "Vertical"`).
- Match blocks with trailing commas.

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

```
CLI (crates/ignis/src/main.rs)
  → resolve input (project ignis.toml or single file)
  → build IgnisConfig with CLI overrides

Driver (crates/ignis_driver/src/pipeline.rs)
  → CompilationContext::discover_modules()     # Module graph discovery
  → parse each file (lexer → parser → AST)
  → ctx.compile() → AnalyzerOutput            # Full semantic analysis

Analyzer (crates/ignis_analyzer/src/lib.rs)
  1. bind_phase()        → DefinitionStore (two-pass: predeclare types, then complete)
  2. resolve_phase()     → node_defs, scopes (name → DefinitionId)
  3. typecheck_phase()   → node_types, TypeStore (bidirectional inference)
  4. const_eval_phase()  → compile-time constant values
  5. extra_checks_phase()→ control flow, never-typed expressions
  6. lint_phase()        → unused variables/imports/mut, deprecated calls
  7. lower_to_hir()      → HIR (includes capture analysis + escape analysis for closures)

Post-analysis (crates/ignis_driver/src/pipeline.rs)
  → Monomorphizer::run()              # Specialize generics to concrete types
  → HirBorrowChecker::check()         # HIR-level borrow checking
  → HirOwnershipChecker::check()      # Produce DropSchedules
  → ignis_lir::lowering::lower_and_verify()  # HIR → LIR (basic blocks, TAC)
  → ignis_codegen_c::emit_*()         # LIR → C source code
  → main wrapper generation            # __ignis_user_main + C main() wrapper

Linking (crates/ignis_driver/src/link.rs)
  → gcc -c → object files (.o)
  → ar rcs → archives (.a)
  → gcc link → executable binary
```

### Key Data Structures Between Phases

| Structure | Crate | Purpose |
| --- | --- | --- |
| `ASTNode` (NodeId) | ignis_ast | Parsed syntax tree nodes |
| `IgnisTypeSyntax` | ignis_ast | Syntactic type annotations (pre-resolution) |
| `Type` (TypeId) | ignis_type | Semantic types (post-typechecking) |
| `TypeStore` | ignis_type | Deduplicated type storage with creation methods |
| `Definition` (DefinitionId) | ignis_type | Declarations (functions, records, enums, variables, fields, variants) |
| `DefinitionStore` | ignis_type | All definitions indexed by DefinitionId |
| `SymbolId` / `SymbolTable` | ignis_type | Interned identifiers for cheap comparisons |
| `HIRNode` (HIRId) / `HIRKind` | ignis_hir | High-level IR (typed, resolved, tree-based) |
| `HIRPattern` / `HIRMatchArm` | ignis_hir | Pattern matching in HIR (match, if let, let else) |
| `HIRCapture` / `CaptureMode` | ignis_hir | Closure capture descriptors (by ref, by move, etc.) |
| `DropSchedules` | ignis_hir | When/where to emit drop calls for owned types |
| `Instr` / `Operand` / `Block` | ignis_lir | Low-level IR (TAC, basic blocks, terminators) |
| `Terminator` | ignis_lir | Block exit: `Goto`, `Branch`, `Return`, `Unreachable` |
| `FunctionLir` / `LirProgram` | ignis_lir | Per-function LIR with locals, temps, blocks |
| `Diagnostic` | ignis_diagnostics | Errors/warnings with spans, labels, notes |

## Extending the Compiler

### Adding a New AST Node

1. **Define the struct** in `crates/ignis_ast/src/statements/` or `expressions/`:
   - Fields use `NodeId` for child references, `Span` for source location.
   - Derive `Debug, Clone, PartialEq, Hash, Eq`.

2. **Add variant** to `ASTStatement` or `ASTExpression` enum in the corresponding `mod.rs`.

3. **Implement `span()`** in the match arm of the parent enum.

4. **Parse it** in `crates/ignis_parser/src/parser/` (`declarations.rs` for statements, `expression.rs` for expressions).

5. **Handle in analyzer phases** (only the ones that apply):
   - `binder.rs` — if the node creates a definition.
   - `resolver.rs` — if the node needs name resolution.
   - `typeck.rs` — if the node produces or consumes types.
   - `lowering.rs` — convert to HIR.
   - Post-HIR: `borrowck_hir.rs` — if the node affects ownership or borrowing.

6. **Handle in LIR lowering** (`crates/ignis_lir/src/lowering/mod.rs`) — convert HIR to LIR instructions.

7. **Handle in C codegen** (`crates/ignis_codegen_c/src/emit.rs`) — emit C code.

8. **Add tests** — E2E in `crates/ignis_driver/tests/`, analyzer in `crates/ignis_analyzer/tests/`.

### Adding a New Builtin

Builtins use `@name(args)` or `@name<Type>(args)` syntax in Ignis source.

1. **Parser** already handles `@` prefix — no changes needed.

2. **Type-check** in `crates/ignis_analyzer/src/typeck.rs`:
   - Add a branch in `typecheck_builtin_call()` matching the builtin name.
   - Validate type args and regular args count/types.
   - Return the appropriate `TypeId`.

3. **Lower to HIR** in `crates/ignis_analyzer/src/lowering.rs`:
   - Add a branch in `lower_builtin_call()`.
   - Return an appropriate `HIRKind` variant (or create a new one in `ignis_hir`).

4. **Lower to LIR** in `crates/ignis_lir/src/lowering/mod.rs`:
   - Handle the new `HIRKind` variant.
   - Emit LIR instructions.

5. **Emit C** in `crates/ignis_codegen_c/src/emit.rs`:
   - Handle the new LIR instruction or HIR-level construct.

Existing builtins: `sizeOf`, `alignOf`, `typeName`, `bitCast`, `pointerCast`, `integerFromPointer`, `pointerFromInteger`, `read`, `write`, `dropInPlace`, `dropGlue`, `maxOf`, `minOf`, `panic`, `trap`, `unreachable`, `configFlag`, `compileError`.

### Adding a New Pattern Form

Patterns are used by `match`, `if let`, `while let`, and `let else`.

1. **Add variant** to `ASTPattern` enum in `crates/ignis_ast/src/pattern.rs`.

2. **Parse it** in `crates/ignis_parser/src/parser/expression.rs` in the pattern parsing methods.

3. **Handle in typechecking** (`typeck.rs`) — validate the pattern against the scrutinee type.

4. **Add variant** to `HIRPattern` in `crates/ignis_hir/src/pattern.rs`.

5. **Lower AST pattern to HIR pattern** in `crates/ignis_analyzer/src/lowering.rs`.

6. **Handle in LIR lowering** (`crates/ignis_lir/src/lowering/mod.rs`) — generate condition checks and bindings.

AST patterns: `Wildcard`, `Literal`, `Path` (with optional destructure args), `Tuple`, `Or`.
HIR patterns: `Wildcard`, `Literal`, `Binding`, `Variant` (with destructure), `Tuple`, `Or`, `Constant`.

### Adding a New Lint

1. **Add variant** to `LintId` enum in `crates/ignis_type/src/lint.rs`.

2. **Implement checker** in `crates/ignis_analyzer/src/lint.rs`:
   - Create a method like `lint_my_check(&mut self, roots: &[NodeId])`.
   - Collect diagnostics via `self.add_diagnostic()`.

3. **Call from `lint_phase()`** in the same file.

4. **Add `DiagnosticMessage` variant** in `crates/ignis_diagnostics/src/message.rs` for the warning/error.

5. **Support `@allow`/`@warn`/`@deny`** — the directive system already maps attribute names to `LintId`.

Existing lints: `UnusedVariable`, `UnusedImport`, `UnusedMut`, `Deprecated`.

### Adding a New Type

1. **Add variant** to `Type` enum in `crates/ignis_type/src/types.rs`.

2. **Add cache + creation method** in `TypeStore` (e.g., `pub fn my_type(...) -> TypeId`).

3. **Update type utility methods** in `TypeStore`:
   - `is_copy()` — if the type has copy semantics.
   - `is_owned()` — if the type owns heap resources.
   - `contains_type_param()` — if it can contain type parameters.
   - `substitute()` — if it contains inner types that need substitution.
   - `format_type_name()` — for diagnostics and debugging.

4. **Handle in typechecking** (`typeck.rs`) — inference, unification, cast rules.

5. **Handle in codegen** (`emit.rs`) — C representation.

### Adding a New Attribute

Attributes use `@name` or `@name(args)` syntax on declarations.

1. **Add variant** to the appropriate enum in `crates/ignis_type/src/attribute.rs`:
   - `RecordAttr` for record-level attributes.
   - `FunctionAttr` for function-level attributes.
   - `FieldAttr` for field-level attributes.

2. **Convert in binder** (`crates/ignis_analyzer/src/binder.rs`):
   - Map from `ASTAttribute` to the typed attribute enum.
   - Validate argument types and constraints.

3. **Handle in codegen** (`crates/ignis_codegen_c/src/emit.rs`):
   - Emit the appropriate C `__attribute__` or equivalent.

Existing attributes: `@packed`, `@aligned(N)`, `@cold`, `@externName("name")`, `@deprecated`, `@deprecated("message")`, `@inline`, `@inline(always)`, `@inline(never)`, `@allow(lint)`, `@warn(lint)`, `@deny(lint)`, `@implements(Drop)`, `@implements(Clone)`, `@implements(Copy)`, `@implements(TraitName)`, `@extension(Type)`, `@extension(Type, mut)`, `@langHook("name")` (namespace attribute), `@lang(try)` (record attribute for Result/Option), `@takes` (parameter attribute), `@noescape` (parameter attribute for closures).

## Testing

### Test Types

| Type | Crate | What It Tests | Snapshot Dir |
| --- | --- | --- | --- |
| E2E (ok) | ignis_driver | Full compile + run, asserts exit code/stdout | `crates/ignis_driver/tests/snapshots/` |
| E2E (err) | ignis_driver | Runtime errors (panics, bounds checks) | same |
| Analyzer (golden) | ignis_analyzer | Diagnostics and HIR output | `crates/ignis_analyzer/tests/snapshots/` |
| Analyzer (fixtures) | ignis_analyzer | `.ign` files from `test_cases/` | same |
| Analyzer (diagnostics) | ignis_analyzer | Error codes at specific line numbers | N/A (assertions) |
| Analyzer (properties) | ignis_analyzer | Property-based (proptest) fuzz testing | N/A |
| Codegen (golden) | ignis_codegen_c | Generated C code snapshots | `crates/ignis_codegen_c/tests/snapshots/` |

### Adding an E2E Test

```rust
// In crates/ignis_driver/tests/e2e_ok.rs
#[test]
fn e2e_my_feature() {
  e2e_test(
    "my_feature",
    r#"
function main(): i32 {
    return 42;
}
"#,
  );
}
```

Run: `cargo test -p ignis_driver e2e_my_feature`
Accept snapshot: `INSTA_UPDATE=always cargo test -p ignis_driver e2e_my_feature`

### Adding an Analyzer Test

```rust
// In crates/ignis_analyzer/tests/golden_ok.rs
#[test]
fn my_semantic_check() {
  let result = common::analyze(r#"
    function main(): void {
        return;
    }
  "#);

  assert_snapshot!("my_semantic_check_diags", common::format_diagnostics(&result.output.diagnostics));
  assert_snapshot!("my_semantic_check_hir", common::format_hir(&result));
}
```

### Adding a Fixture Test

1. Create `test_cases/analyzer/<category>/my_test.ign`.
2. Add to `crates/ignis_analyzer/tests/fixtures.rs`:
   ```rust
   #[test]
   fn fixture_my_test() {
     test_fixture("test_cases/analyzer/<category>/my_test.ign");
   }
   ```

### Snapshot Workflow

- First run creates `.snap.new` pending files.
- `cargo insta review` opens interactive review.
- `INSTA_UPDATE=always cargo test` auto-accepts all changes.
- Snapshots are YAML with source reference, assertion line, and expression output.

## Common Pitfalls

1. **Forgetting to handle a new AST variant in all analyzer phases.** The Rust compiler will not warn you about non-exhaustive matches if a catch-all `_` arm exists. Grep for existing variant names to find all match sites. Similarly for new `HIRKind` variants in LIR lowering and codegen.

2. **Forgetting to update `offset_ids()` in HIR.** When adding a new `HIRKind` variant that contains `HIRId` fields, update the `offset_ids()` method or monomorphization will silently produce wrong references.

3. **Type invariants after monomorphization.** Post-mono, no `Type::Param` or `Type::Instance` should exist. Debug builds verify this with `mono_output.verify_no_generics()`.

4. **LIR verification failures.** After LIR lowering, `lower_and_verify()` checks well-formedness. If you add new instructions, ensure they satisfy the verifier's invariants.

5. **Snapshot tests require GCC.** E2E and codegen tests compile C code with `gcc`. Ensure it's installed and in `PATH`.

6. **Import order matters.** `.rustfmt.toml` disables import reordering. Maintain the existing grouping: std, external, internal.

7. **Two-pass binding.** Records/enums/type aliases are predeclared in pass 1, then fully bound in pass 2. This enables forward references. If you add a new declaration type that can be referenced before its definition, add it to the predeclaration pass.

8. **Bidirectional type inference.** The typechecker propagates expected types downward via `InferContext`. When adding new expression types, consider whether they should propagate or consume type expectations.

9. **Module classification in codegen.** `classify.rs` determines whether a definition belongs to User, Std, or Runtime code. If you add new definition kinds, ensure they classify correctly to avoid duplicate or missing emissions.

10. **Drop schedules.** Ownership analysis produces `DropSchedules` that tell LIR lowering where to emit cleanup code. If you add new control flow constructs, ensure drops are scheduled at all exit points (normal exit, break, continue, return).

## Key Files

| File | Purpose |
| --- | --- |
| `crates/ignis/src/main.rs` | CLI entry, config resolution, subcommands |
| `crates/ignis/src/cli.rs` | Clap CLI definition |
| `crates/ignis_driver/src/pipeline.rs` | Build pipeline: analysis → mono → LIR → codegen → link |
| `crates/ignis_driver/src/context.rs` | Module discovery, import resolution, per-module parsing |
| `crates/ignis_driver/src/link.rs` | GCC compilation, archive creation, executable linking |
| `crates/ignis_parser/src/parser/declarations.rs` | Top-level parsing (functions, records, enums, traits, imports) |
| `crates/ignis_parser/src/parser/expression.rs` | Expression parsing with Pratt precedence |
| `crates/ignis_parser/src/parser/statement.rs` | Statement parsing |
| `crates/ignis_parser/src/parser/type_syntax.rs` | Type annotation parsing |
| `crates/ignis_analyzer/src/lib.rs` | Analyzer struct, phase dispatch, `analyze_with_shared_stores()` |
| `crates/ignis_analyzer/src/binder.rs` | Binding phase (two-pass: predecl + complete) |
| `crates/ignis_analyzer/src/resolver.rs` | Name resolution phase |
| `crates/ignis_analyzer/src/typeck.rs` | Type checking phase (bidirectional inference) |
| `crates/ignis_analyzer/src/borrowck_hir.rs` | HIR-level borrow checking (replaces old AST-level borrowck) |
| `crates/ignis_analyzer/src/lowering.rs` | AST → HIR lowering |
| `crates/ignis_analyzer/src/mono.rs` | Monomorphization (generic specialization) |
| `crates/ignis_analyzer/src/lint.rs` | Lint infrastructure and checks |
| `crates/ignis_analyzer/src/scope.rs` | Scope tree management |
| `crates/ignis_analyzer/src/capture.rs` | Closure capture analysis (determine what closures capture) |
| `crates/ignis_analyzer/src/escape.rs` | Escape analysis (determine if closures outlive their defining scope) |
| `crates/ignis_hir/src/lib.rs` | HIR types: HIRNode, HIRKind, HIR store |
| `crates/ignis_hir/src/pattern.rs` | HIRPattern, HIRMatchArm for pattern matching |
| `crates/ignis_hir/src/drop_schedule.rs` | Drop scheduling for ownership |
| `crates/ignis_lir/src/instr.rs` | LIR instructions (Load, Store, BinOp, Call, etc.) |
| `crates/ignis_lir/src/program.rs` | FunctionLir, LirProgram |
| `crates/ignis_lir/src/lowering/mod.rs` | HIR → LIR lowering |
| `crates/ignis_lir/src/verify.rs` | LIR verification |
| `crates/ignis_codegen_c/src/emit.rs` | C code emission from LIR |
| `crates/ignis_codegen_c/src/classify.rs` | Definition classification (User/Std/Runtime) |
| `crates/ignis_ast/src/statements/mod.rs` | ASTStatement enum and all statement types |
| `crates/ignis_ast/src/expressions/mod.rs` | ASTExpression enum and all expression types |
| `crates/ignis_ast/src/type_.rs` | IgnisTypeSyntax (parsed type annotations) |
| `crates/ignis_ast/src/pattern.rs` | ASTPattern (Wildcard, Literal, Path, Tuple, Or) |
| `crates/ignis_ast/src/attribute.rs` | ASTAttribute (parsed `@` annotations) |
| `crates/ignis_type/src/types.rs` | Type enum, TypeStore, Substitution |
| `crates/ignis_type/src/definition.rs` | Definition, DefinitionKind, DefinitionStore |
| `crates/ignis_type/src/attribute.rs` | RecordAttr, FunctionAttr, FieldAttr |
| `crates/ignis_type/src/lint.rs` | LintId, LintLevel |
| `crates/ignis_diagnostics/src/message.rs` | DiagnosticMessage variants (450+) |
| `crates/ignis_diagnostics/src/diagnostic_report.rs` | Diagnostic, Severity, Label |
| `crates/ignis_lsp/src/server.rs` | LSP request handlers (hover, goto-def, completions) |
| `crates/ignis_lsp/src/completion.rs` | Token-based completion (works without valid AST) |
| `crates/ignis_lsp/src/at_items.rs` | Registry of `@`-prefixed builtins and directives |
| `crates/ignis_lsp/src/type_format.rs` | Type formatting for LSP hover/display |
| `std/manifest.toml` | Std module registry and linking config |
| `std/runtime/ignis_rt.h` | C runtime API (memory, strings, I/O) |
| `std/runtime/internal/rt_string.c` | IgnisString runtime implementation |

## UTF-8 String/Char Semantics (v0.4)

Ignis v0.4 defines string types as UTF-8 byte-backed:

| Type | Representation | C equivalent |
| --- | --- | --- |
| `char` | Single byte (`u8`) | `u8` |
| `str` | UTF-8 NUL-terminated byte slice | `const char*` |
| `String` | Heap-backed UTF-8 byte buffer (data + len + cap) | `IgnisString` |

Key rules:
- Char literals (`'a'`) accept only single-byte ASCII or byte-range escapes (`\u{00}`–`\u{FF}`).
- Multi-byte char literals (`'ñ'`, `'\u{100}'`+) produce a compile error (`MultiByteCharacterLiteral`).
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

1. **Capture analysis** (`capture.rs`) — determines which outer variables a closure captures and the capture mode (by ref, by move, by ref-mut).
2. **Escape analysis** (`escape.rs`) — determines if a closure outlives its defining scope. `@noescape` on parameters prevents escape propagation.
3. **HIR lowering** — `HIRKind::Closure` carries captures, thunk/drop definition IDs, and an `escapes` flag.
4. **LIR lowering** — emits `MakeClosure` (captures → env struct), `CallClosure` (indirect call through thunk), `DropClosure` (cleanup).
5. **C codegen** — non-escaping closures use stack-allocated env; escaping closures use heap-allocated env. Closure values are structs with `call` (thunk fn ptr), `drop` (optional drop fn ptr), and `env` (opaque `*u8`).
