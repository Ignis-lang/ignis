# Ignis Formatting Policy (v0.5)

This document defines the first canonical formatter policy shipped by `ignis fmt`.

## Scope

- The formatter rewrites trivia and layout only.
- It does not intentionally change program semantics.
- It preserves declaration order, member order, attribute order, match-arm order, and statement order.
- It may canonicalize import and re-export groups by sorting or merging them according to the rules below.
- There are no user-configurable style profiles in v0.5.
- The bounded configuration surface is `indent_width`, `line_width`, `use_tabs`, and `sort_imports`.

## Canonical layout rules

- Use the effective `indent_width` for all indentation, including printer-owned comments and directive branches.
- When `use_tabs = true`, each indentation level is emitted as a tab while `indent_width` still defines the logical width used by layout decisions.
- Normalize spacing around `:`, `=`, `+`, `-`, and commas.
- Keep generic angle brackets tight: `identity<T>`, not `identity < T >`.
- Preserve import/re-export order exactly as written unless grouping or `sort_imports` applies.
- Consecutive `import ... from` statements with the same path are merged into one import list.
- Consecutive `export ... from` statements with the same path are merged into one re-export list.
- Same-path imports or re-exports separated by an intentional blank line remain separate.
- Discard imports such as `import _ from "...";` are not merged with named imports.
- When `sort_imports = true`, sort imports/re-exports within each existing import group only. Preserve comment-separated and blank-line-separated groups.
- Preserve a single intentional blank line between supported statements or items when the original gap is whitespace-only.
- Insert a blank line between a contiguous import block and the following declaration block without detaching owned doc comments from the declaration.
- Emit function bodies as multi-line blocks when a block contains statements.
- Preserve structural blank lines between detached comment blocks, import groups, declarations, and block-local statement groups so sections do not collapse together.
- Strip trailing whitespace from formatter-owned output lines and always end the file with a single trailing newline.
- Empty high-level blocks (`namespace`, `record`, `enum`, `trait`, `extern`) canonicalize to inline `{}`.
- Callable parameter lists use layout-driven trailing commas: single-line canonical output drops the final comma, multiline canonical output adds the final comma.
- Record initializers use the same rule: single-line canonical output drops the final comma, multiline canonical output adds the final comma.
- Import and re-export item lists wrap when the flat form exceeds `line_width`; multiline canonical output adds the final comma before `from`.
- Single pipe expressions may stay inline when they fit `line_width`; pipe chains with 2+ `|>` always format multiline.
- There is still no shipped general-purpose wrapping contract for every long expression shape. When the formatter cannot prove a rewrite is safe, it fails rather than guessing.

Import grouping example:

```ignis
import TomlArray from "./value";
import TomlValue from "./value";
```

formats as:

```ignis
import TomlArray, TomlValue from "./value";
```

Long import example with `line_width = 80`:

```ignis
import
  TomlArray,
  TomlArrayOfTables,
  TomlDateTime,
from "./value";
```

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
- `namespace` and `extern` bodies that contain raw compile-time directives fall back to source-preserving formatting for that body instead of reparsing and pruning branches.

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
- Safety validation compares token shape, comment ownership, and directive structure, but treats optional trailing commas before `)`, `}`, and `from` as layout-normalized equivalents.
- Safety validation also treats consecutive same-path import/re-export statements and their grouped canonical form as equivalent.
- Formatter failures are formatter bugs or invalid input, not lint diagnostics. Valid source is expected to format successfully.

## Configuration contract

- Supported keys: `indent_width`, `line_width`, `use_tabs`, `sort_imports`.
- Unsupported or unknown formatter keys are hard errors.
- Defaults: `indent_width = 2`, `line_width = 100`, `use_tabs = false`, `sort_imports = false`.
- Effective settings resolve in this order: built-in defaults, then `[formatter]` in `ignis.toml`, then `ignisfmt.toml` (or an explicit `--config` path), then CLI flags.
- The resolved indentation settings are applied to both layout-owned output and printer-owned rendering paths.

### `indent_width`

- Type: integer.
- Accepted range: `1..=8`.
- Default: `2`.
- Controls the logical indentation width used for spaces and for layout-width decisions.

### `line_width`

- Type: integer.
- Accepted range: `40..=160`.
- Default: `100`.
- Controls layout-aware wrapping for supported constructs, including callable signatures, record initializers, import/re-export item lists, and single pipe expressions.

### `use_tabs`

- Type: boolean.
- Default: `false`.
- When `true`, indentation is emitted as one tab per indentation level.
- `indent_width` still controls logical width for layout decisions.
- CLI override: `--use-tabs`; use `--spaces` to force spaces when config enables tabs.

### `sort_imports`

- Type: boolean.
- Default: `false`.
- When `true`, imports/re-exports are sorted within each existing import group.
- Blank-line-separated and comment-separated import groups remain separate.
- This does not sort declarations, members, attributes, match arms, or statements.

## Coverage notes

- Canonical examples are pinned by snapshot tests in `crates/ignis_formatter/tests/canonical_layout.rs`.
- CLI rewrite, `--check`, project traversal, invalid-input failure, and help text are covered in `crates/ignis/tests/test_command.rs`.
- Real-file approval coverage is reviewed in `crates/ignis_formatter/tests/real_files.rs` and `crates/ignis_formatter/tests/readability_gate.rs` against `example/hello-world.ign`, `example/record.ign`, exact `example/allocator/src/heap_allocator.ign` windows, and many `std/` files.
- Real-file approvals also pin the formatter's current trailing-whitespace policy, two-space default indentation, tab override behavior, preserved blank-line behavior, canonical empty-block output, and layout-driven trailing-comma policy.
- Shipped CLI features today include project mode, single-file mode, multiple explicit file paths, `--check`, `--stdin-json`, `--emit diff`, and `--sort-imports`.
