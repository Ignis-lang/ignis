## Naming Conventions

### Rust

| Element | Convention | Examples | References |
| --- | --- | --- | --- |
| Types (struct/enum/trait) | PascalCase | `ASTFunction`, `TypeStore`, `HIRKind` | `crates/ignis_ast/src/statements/function.rs` |
| Functions/methods | snake_case | `typecheck_phase`, `lower_to_hir` | `crates/ignis_analyzer/src/lib.rs` |
| Fields/locals | snake_case | `node_defs`, `type_id`, `owner_module` | `crates/ignis_type/src/definition.rs` |
| Constants | UPPER_SNAKE_CASE | `MAX_RECURSION_DEPTH` | `crates/ignis_parser/src/parser/expression.rs` |
| Type aliases (Id wrappers) | PascalCase | `NodeId`, `TypeId`, `DefinitionId`, `HIRId` | `crates/ignis_type/src/types.rs` |
| Tests | `test_` + snake_case or descriptive snake_case | `e2e_arithmetic_add`, `fixture_mut_ref` | `crates/ignis_driver/tests/e2e_ok.rs` |

### Ignis

| Element | Convention | Examples | References |
| --- | --- | --- | --- |
| Types (record/enum) | PascalCase | `Vector`, `Option`, `Layout` | `std/vector/mod.ign` |
| Functions/methods/fields/params | camelCase | `getLength`, `pushBack`, `myVariable` | `std/string/mod.ign` |
| Constants, enum members | UPPER_SNAKE_CASE | `MAX_SIZE`, `PI`, `TAU` | `std/math/mod.ign` |
| Modules/namespaces | PascalCase | `Math`, `Memory`, `Io` | `std/manifest.toml` |
| Files | lower_snake_case | `my_module.ign`, `mut_self_test.ign` | `test_cases/analyzer/` |

## File Organization

- Workspace crates live in `crates/<name>/src` with a `lib.rs` or `main.rs` root.
- Never create `mod.rs` files; use `src/some_module.rs` instead.
- Each compiler phase is a dedicated file in its crate (e.g., `binder.rs`, `resolver.rs`, `typeck.rs`).
- Tests live under `crates/<crate>/tests/` with shared helpers in `tests/common/mod.rs`.
- Ignis fixture tests live in `test_cases/analyzer/<category>/` organized by semantic feature.
- Snapshots are stored in `crates/<crate>/tests/snapshots/`.

## Import Style

Imports are grouped in three blocks separated by blank lines:

```rust
// 1. Standard library
use std::collections::HashMap;
use std::sync::Arc;

// 2. External crates
use insta::assert_snapshot;
use colored::Colorize;

// 3. Internal crates and modules
use ignis_type::types::{TypeId, TypeStore};
use crate::scope::ScopeTree;
```

Rustfmt keeps import order as written (`.rustfmt.toml` sets `reorder_imports = false`). Do not rely on automatic reordering.

## Code Patterns

### Pipeline Functions

Driver and CLI functions return `Result<(), ()>` and emit user-facing errors directly:

```rust
pub fn compile_project(config: Arc<IgnisConfig>, entry_path: &str) -> Result<(), ()> {
  // ... work ...
  if has_errors {
    cmd_fail!("build", start);
    return Err(());
  }
  cmd_ok!("build", start);
  Ok(())
}
```

### Id-Indexed Stores

Core data structures use phantom-typed `Id<T>` indices into `Store<T>` arenas:

```rust
// Type alias for clarity
pub type NodeId = Id<ASTNode>;
pub type TypeId = Id<Type>;
pub type DefinitionId = Id<Definition>;
pub type HIRId = Id<HIRNode>;
```

Stores provide `alloc()` → `Id`, `get(id)` → `&T`, and iteration. Ids are cheap to copy and compare.

### TypeStore Deduplication

`TypeStore` deduplicates types via per-category `HashMap`s:

```rust
// Creating types (idempotent — returns existing TypeId if identical)
let ptr_ty = types.pointer(inner_ty, /* mutable */ true);
let vec_ty = types.vector(elem_ty, Some(10));
let fn_ty = types.function(param_tys, ret_ty, /* variadic */ false);
let rec_ty = types.record(def_id);
```

### Definition Lookup

All declarations (functions, records, enums, variables, fields, variants, type params) become `Definition` entries in `DefinitionStore`:

```rust
let def = defs.get(def_id);
match &def.kind {
  DefinitionKind::Function(func_def) => { /* params, return_type, type_params */ }
  DefinitionKind::Record(rec_def) => { /* fields, methods, type_params */ }
  DefinitionKind::Variable(var_def) => { /* type_id, mutable */ }
  // ...
}
```

### Diagnostic Creation

Diagnostics are created from `DiagnosticMessage` variants and collected in the analyzer:

```rust
self.add_diagnostic(
  DiagnosticMessage::UndeclaredVariable {
    name: name.to_string(),
    span,
  }.report()
);
```

For lint-level diagnostics, use `report_with_severity()` to override the default severity.

### Scope Management

The analyzer uses a `ScopeTree` with push/pop semantics:

```rust
self.scopes.push(ScopeKind::Function);
self.scopes.define(symbol_id, def_id);
// ... process body ...
let my_def = self.scopes.lookup_def(symbol_id);
self.scopes.pop();
```

Scope kinds: `Global`, `Function`, `Block`, `Generic`, `Loop`, `Namespace`.

### Bidirectional Type Inference

The typechecker propagates expected types downward:

```rust
// Caller provides expected type
self.typecheck_node_with_infer(node_id, InferContext::expecting(expected_ty));

// Callee checks the expectation
fn typecheck_expression(&mut self, expr: &ASTExpression, infer: InferContext) {
  if let Some(expected) = infer.expected {
    // Use expected type to guide inference
  }
}
```

### AST Metadata and Attributes

`ASTMetadata` is a bitflag set for declaration properties:

```rust
// Syntactic properties
metadata.contains(ASTMetadata::STATIC)
metadata.contains(ASTMetadata::MUTABLE)
metadata.contains(ASTMetadata::EXPORT)
```

`ASTAttribute` carries declaration annotations parsed from `@name(args)`:

```rust
// Consumed by the analyzer binder to produce typed attributes
// e.g., ASTAttribute { name: "packed", args: [] } → RecordAttr::Packed
```

### HIR Construction

HIR nodes are allocated in the HIR store and referenced by `HIRId`:

```rust
let hir_id = self.hir.alloc(HIRNode {
  kind: HIRKind::Binary {
    operation: BinaryOperation::Add,
    left: left_id,
    right: right_id,
  },
  span,
  type_id: result_type,
});
```

### LIR Instruction Emission

LIR is built via `FunctionBuilder`:

```rust
let dest = builder.new_temp(result_type);
builder.emit_instr(Instr::BinOp {
  dest,
  op: BinOp::Add,
  left: Operand::Temporary(left_temp),
  right: Operand::Temporary(right_temp),
});
```

## Error Handling

- Driver/CLI functions return `Result<(), ()>` and print errors via `eprintln!` + `colored`.
- The analyzer collects `Vec<Diagnostic>` without early termination (continues after errors).
- Diagnostics are rendered by `ignis_diagnostics::render(diag, source_map)`.
- The LSP wraps analysis in `catch_unwind` to avoid crashing on panics.
- Avoid `unwrap()` and functions that panic; use `?` to propagate errors.

## Logging

- CLI/build output: `ignis_log` macros (`cmd_header!`, `phase_log!`, `cmd_ok!`, `cmd_fail!`).
- LSP: logs to file via `completion::log`.
- Do not use `println!` for build output; use the `ignis_log` macros.

## Testing

- **Snapshots:** `insta::assert_snapshot!` for deterministic output comparison.
- **E2E:** full compile → run in `crates/ignis_driver/tests/` (requires GCC).
- **Analyzer:** semantic analysis snapshots in `crates/ignis_analyzer/tests/`.
- **Codegen:** C output snapshots in `crates/ignis_codegen_c/tests/`.
- **Fixtures:** `.ign` test cases in `test_cases/analyzer/<category>/`.
- **Properties:** `proptest` for fuzz testing in `crates/ignis_analyzer/tests/properties.rs`.
- Update snapshots: `cargo insta review` or `INSTA_UPDATE=always cargo test`.

## Formatting

- 2-space indentation (`tab_spaces = 2`).
- Max line width: 120 characters.
- Vertical function parameter layout (`fn_params_layout = "Vertical"`).
- Match blocks with trailing commas.
- No automatic import reordering.

## Do's and Don'ts

- Do keep Ignis naming: PascalCase types, camelCase members, UPPER_SNAKE_CASE constants.
- Do keep Rust imports in intentional grouping order (std / external / internal).
- Do use `ignis_log` macros for user-facing build output.
- Do use `Id<T>` / `Store<T>` for new arena-allocated data.
- Do use `TypeStore` creation methods (they deduplicate automatically).
- Do collect diagnostics via `add_diagnostic()` instead of returning early on errors.
- Do add snapshot tests for new features (E2E for behavior, golden for diagnostics/HIR).
- Don't create `mod.rs` files; use flat module files.
- Don't rely on implicit std path; prefer `IGNIS_STD_PATH` or `ignis.toml`.
- Don't use `unwrap()` in compiler code; prefer `?` or diagnostic reporting.
- Don't use `println!` for build output; use `ignis_log` macros.
- Don't assume Ignis behaves like Rust or TypeScript; check the codebase.
