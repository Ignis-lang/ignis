# Ignis Formatting Policy (v0.5)

This document defines the first canonical formatter policy shipped by `ignis fmt`.

## Scope

- The formatter rewrites trivia and layout only.
- It does not intentionally change program semantics.
- It preserves source order; it does not reorder imports, declarations, members, attributes, match arms, or statements.
- There are no user-configurable style profiles in v0.5.
- The bounded configuration surface is `indent_width`, `line_width`, and `use_tabs` only.

## Canonical layout rules

- Use the effective `indent_width` for all indentation, including printer-owned comments and directive branches.
- When `use_tabs = true`, each indentation level is emitted as a tab while `indent_width` still defines the logical width used by layout decisions.
- Normalize spacing around `:`, `=`, `+`, `-`, and commas.
- Keep generic angle brackets tight: `identity<T>`, not `identity < T >`.
- Preserve import order exactly as written.
- Preserve a single intentional blank line between supported statements or items when the original gap is whitespace-only.
- Insert a blank line between a contiguous import block and the following declaration block without detaching owned doc comments from the declaration.
- Emit function bodies as multi-line blocks when a block contains statements.
- Preserve structural blank lines between detached comment blocks, import groups, declarations, and block-local statement groups so sections do not collapse together.
- Strip trailing whitespace from formatter-owned output lines and always end the file with a single trailing newline.
- Trailing commas are currently enforced only on formatter-owned multiline record literals and enum variant lists; wrapped function or method parameter lists are not a trusted surface and do not have a shipped trailing-comma contract.
- There is no shipped general long-line or long-function wrapping contract yet. When a tighter `line_width` would require a risky rewrite on a real function slice, the formatter currently rejects the rewrite via safety validation instead of pretending the wrap is supported.

Example:

```ignis
function main(): void {
  let value: i32 = 1 + 2;
  return;
}
```

## Comment and directive preservation

- Regular comments and documentation comments are preserved.
- Comments may have surrounding whitespace normalized, but they are not dropped or moved across semantic boundaries.
- Compile-time directives stay in source order and inactive branches are retained.
- Directive headers are preserved verbatim while branch bodies are reformatted canonically.

Example:

```ignis
// detached

@if(featureFlag) {
    /// docs
    function enabled<T>(value: Result<T, Error>): Result<T, Error> {
        return value;
    }
}
@else {
    // keep branch
    function disabled(): void {
        return;
    }
}
```

## Safety guarantees

- `ignis fmt` writes a file only when the formatted output differs.
- The formatter reparses and revalidates formatted output before accepting it.
- If lexing, modeling, or safety validation fails, the command exits with an error and does not rewrite the file.
- Unsupported syntax domains fail fast instead of being reformatted heuristically.

## Configuration contract

- Supported keys: `indent_width`, `line_width`, `use_tabs`.
- Unsupported or unknown formatter keys are hard errors.
- Defaults: `indent_width = 2`, `line_width = 100`, `use_tabs = false`.
- Effective settings resolve in this order: built-in defaults, then `[formatter]` in `ignis.toml`, then `ignisfmt.toml` (or an explicit `--config` path), then CLI flags.
- The resolved indentation settings are applied to both layout-owned output and printer-owned rendering paths.

## Coverage notes

- Canonical examples are pinned by snapshot tests in `crates/ignis_formatter/tests/canonical_layout.rs`.
- CLI rewrite, `--check`, project traversal, invalid-input failure, and help text are covered in `crates/ignis/tests/test_command.rs`.
- Real-file approval coverage is reviewed in `crates/ignis_formatter/tests/real_files.rs` and `crates/ignis_formatter/tests/readability_gate.rs` against `example/hello-world.ign`, exact `example/allocator/src/heap_allocator.ign` windows, and a coherent `std/io/mod.ign` slice.
- Real-file approvals also pin the formatter's current trailing-whitespace policy, trailing-comma policy for multiline record literals and enum variant lists, two-space default indentation, tab override behavior, preserved blank-line behavior, and the explicit unsupported status of tight `line_width` rewrites for real function slices.
- Unsupported today: `--stdin`, `--emit diff`, a trusted general-purpose long-expression / long-statement / long-function wrapping policy, exported namespaces, pipe expressions, and other slices still marked fail-fast in the reviewed corpus.
