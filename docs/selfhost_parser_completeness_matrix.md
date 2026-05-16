# Selfhost Parser Completeness Matrix

Direct selfhost AST/parser coverage tracker for `parser-complete-selfhost-ast`.

## Status legend
- `Supported`: current selfhost AST + parser path exists and is exercised by real corpus/tests.
- `Partial`: parsed today, but important structure is still collapsed, skipped, or not audited deeply enough.
- `Unsupported`: syntax is not accepted by the current language/parser reality, so selfhost should reject it explicitly instead of inventing AST semantics.
- `Next`: explicit follow-up target for the next AST/parser batches.

## Corpus and gate baseline

Canonical corpus markers used by the lightweight matrix gate:
- `std/**/*.ign`
- `test_cases/analyzer/**/*.ign`
- `example/**/*.ign`

| Gate | Current source | Status | Notes |
|---|---|---:|---|
| Clean std corpus | `ignis/parser/corpus_tests.ign` -> `collectIgnFiles("std")` | Supported | Parses direct selfhost AST, no parity output required. |
| Clean analyzer corpus | `ignis/parser/corpus_tests.ign` -> `collectIgnFiles("test_cases/analyzer")` | Supported | Broad language usage surface. |
| Clean example corpus | `ignis/parser/corpus_tests.ign` -> `collectIgnFiles("example")` | Supported | Keeps user-facing examples in the gate. |
| Formatter mirror corpus | `ignis/parser/corpus_tests.ign` -> `formatterMirrorCorpusPaths()` | Supported | Confirms parser still accepts formatter round-trip inputs. |
| Malformed diagnostics fixtures | `test_cases/selfhost/parser/malformed/**` + `expected.diagnostics` | Supported | Real diagnostics/recovery harness remains the malformed gate. |
| Completeness matrix presence | `docs/selfhost_parser_completeness_matrix.md` | Supported | Lightweight review gate, checked by `parserCompletenessMatrixExistsAndTracksCurrentCorpusGates`. |

## AST/parser coverage inventory

| Syntax family / critical field | Rust source of truth | Selfhost AST / parser path | Current fixtures / gates | Status | Notes |
|---|---|---|---|---:|---|
| Top-level items: function / record / enum / trait / namespace / type alias / const / extern | `crates/ignis_ast/src/statements/*` | `ignis/ast/items.ign`, `ignis/parser/declarations.ign` | `test_cases/selfhost/parser/ast_core`, full corpora | Supported | Core item families parse today. |
| Generics and bounds | `crates/ignis_ast/src/generics.rs` | `ignis/parser/declarations.ign`, `ignis/ast/items.ign`, `ignis/ast/serialize.ign` | `ignis/ast/tests.ign`, `ignis/parser/tests.ign`, analyzer generics corpus | Supported | Generic params and bound paths now persist on direct item AST nodes and the structural serializer exposes them for focused selfhost tests. |
| Import / export structure | `crates/ignis_ast/src/statements/import_statement.rs`, `export_statement.rs` | `ignis/ast/items.ign`, `ignis/parser/declarations.ign`, `ignis/ast/serialize.ign` | `ignis/ast/tests.ign`, `ignis/parser/tests.ign`, std + analyzer corpus | Supported | Direct selfhost AST now preserves import item names/kinds, `from` path + span, export declaration-vs-name-vs-re-export shape, and focused serializer/parser assertions cover the real structure. |
| Metadata / attributes / modifiers | `crates/ignis_ast/src/metadata.rs`, `attribute.rs` | `ignis/ast/metadata.ign`, `ignis/parser/declarations.ign`, `ignis/ast/serialize.ign` | `ignis/ast/tests.ign`, `ignis/parser/tests.ign`, std corpus | Supported | Direct selfhost metadata now preserves attribute names, argument text, attribute spans, and modifier lists across representative item, method, field, and parameter binding surfaces; focused serializer/parser assertions cover the analyzer-handoff-critical structure. |
| Statements and declaration handoff | `crates/ignis_ast/src/statements/*` | `ignis/ast/statements.ign`, `ignis/parser/statements.ign` | `ignis/parser/tests.ign`, full corpora | Supported | Current parser coverage is broad and should stay stable. |
| Expressions and operators | `crates/ignis_ast/src/expressions/*` | `ignis/ast/expressions.ign`, `ignis/parser/expressions.ign` | `ignis/parser/tests.ign`, full corpora | Supported | Real parse support exists; remaining work is completeness auditing, not parity dumps. |
| Patterns | `crates/ignis_ast/src/pattern.rs` | `ignis/ast/patterns.ign`, `ignis/parser/patterns.ign`, `ignis/ast/serialize.ign` | `ignis/ast/tests.ign`, `ignis/parser/tests.ign`, analyzer corpus | Supported | Focused selfhost AST/parser tests now assert binding metadata+annotation, destructuring path arguments, tuple/or nesting, wildcard, and literal patterns, and the structural serializer renders path/literal details directly instead of collapsing them to placeholders. |
| Types: path / tuple / function / pointer / reference / slice / fixed array / generic | `crates/ignis_ast/src/type_.rs` | `ignis/ast/types.ign`, `ignis/parser/types.ign` | std + analyzer corpus | Supported | Current corpus uses these forms successfully. |
| Types: union / intersection | `crates/ignis_ast/src/type_.rs`, `crates/ignis_parser/src/parser/type_syntax.rs` | `ignis/parser/types.ign` | `ignis/parser/tests.ign` | Unsupported | Rust still declares `IgnisTypeSyntax::Union` / `Intersection`, but the current Rust parser has no infix `|` / `&` type parsing path. Selfhost now mirrors that language reality with focused unsupported-type diagnostics and recovery tests instead of inventing direct AST nodes prematurely. |
| Diagnostics and recovery observables | Rust parser diagnostics behavior | `ignis/parser/recovery.ign`, diagnostics fixtures | `test_cases/selfhost/parser/diagnostic_recovery`, malformed corpus | Supported | Use `expected.diagnostics`, not parity baselines. |

## Superseded direction
- The former parser parity layer is no longer the product contract.
- `expected.parity`, `ignis/ast/parity.ign`, Rust parity dump tests, and `--dump parser-parity` were transitional artifacts and are intentionally retired from the main path.
- Replacement readiness now means: direct AST coverage, parser corpus success, malformed diagnostics/recovery stability, and later analyzer-handoff audits.

## Next implementation targets
1. Expand AST field-level assertions for any remaining `Partial` rows.
2. If/when the language admits union/intersection type syntax in Rust first, add direct selfhost AST nodes/parser support in the same batch before moving that row out of `Unsupported`.
3. Keep promotion-only Rust-vs-Ignis comparison ephemeral and out of the checked-in product surface.
