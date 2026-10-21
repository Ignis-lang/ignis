# Ignis Formatting Policy (v0.5)

This document defines the first canonical formatter policy shipped by `ignis fmt`.

The **v0.5** here is the revision number of this formatter policy, not a compiler release:
the compiler on `main` is 0.4.x. The label is kept because the policy text and its tests
refer to it.

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
- Compound assignments are preserved exactly: the formatter prints back every assignment operator the parser accepts (`+=`, `-=`, `*=`, `/=`, `%=`, `&=`, `|=`, `^=`, `<<=`, `>>=`, `~=`) and plain `=` verbatim.
- Keep generic angle brackets tight: `identity<T>`, not `identity < T >`.
- Print string and template literals verbatim. Whitespace inside a template's `${ }` slots is left as written.
- Print an `asm` header on one line in canonical layout, `asm pure (value in rax, other inout rbx) -> (result: u8 in al) clobber(memory, flags) { ... }`, and its body verbatim from `{` to `}`: spacing, line breaks and comments inside the body are not reformatted or reindented. The header is not wrapped at `line_width`.
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
- A trait default method is printed once, inside its trait. Types that inherit it through `@implements` are printed as written, without the inherited method.
- Callable parameter lists use layout-driven trailing commas: single-line canonical output drops the final comma, multiline canonical output adds the final comma.
- Record initializers use the same rule: single-line canonical output drops the final comma, multiline canonical output adds the final comma.
- Array literals that initialize a `let`, `const`, `return` or static field use the same rule. One written on a single line stays inline when it fits `line_width` and otherwise wraps one element per line. One written over several lines keeps its rows: elements that started on the same source line stay on one line, reindented, and every row ends with a comma, the last one included. An array literal anywhere else that the source wrote over several lines is kept as written, like a multiline call or record initializer.
- Import and re-export item lists wrap when the flat form exceeds `line_width`; multiline canonical output adds the final comma before `from`.
- Range expressions are written without spaces around `..` and `..=` (`0..n`, `a + 1..b * 2`); their bounds keep the spacing of the operators inside them.
- Single pipe expressions may stay inline when they fit `line_width`; pipe chains with 2+ `|>` always format multiline.
- A call whose last argument is a closure with a `{ ... }` body (a trailing closure), or a call whose last argument is itself such a call, is laid out from the syntax tree whatever lines the source used, when the call is a whole expression statement or the whole value of a `let`, `const`, `static` or `return`. Anywhere else, such as inside an `if let` condition, it keeps the general call layout:
  - The closure hugs the call when the line that opens its body, from the start of the call to the closing `-> {`, fits `line_width`. The body is indented one level deeper than the statement and the closing `})` aligns with the start of the statement.
  - Otherwise every argument goes on its own line one level deeper than the statement, each followed by a comma, and the closing `)` aligns with the start of the statement. The closure's body is then one level deeper than its own line and its `}` closes on that line's indentation.
  - When the closure's header still does not fit on its own line, its parameters break one per line, one level deeper again, with a trailing comma, and `): ReturnType -> {` closes the list.
  - A nested trailing-closure call as last argument follows the same rule: `workers.push(Thread::spawn<T>((): T -> {` hugs both calls when it fits, and the outer call breaks first when it does not.
  - A comment between the call's start and the closure body, or after the body inside the call, or another argument the source wrote across several lines, keeps the whole call as written, like any other multiline call.
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

Multiline array example, rows kept as written:

```ignis
const GRID: u8[6] = [
  1, 2, 3,
  4, 5, 6,
];
```

Trailing closure examples. The head fits, so the closure hugs the call:

```ignis
let handle: JoinHandle<i32> = Thread::spawn<i32>((): i32 -> {
  return 1;
});
```

The head does not fit, so the arguments break, and so do the closure's parameters:

```ignis
router.post(
  "/tasks",
  (
    request: &mut Web::Request,
    response: &mut Web::Response,
    next: Web::Next,
  ): Result<void, Web::Error> -> {
    return createTask(request, response);
  },
);
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
- Safety validation compares token shape, comment ownership, and directive structure, but treats optional trailing commas before `)`, `]`, `}`, and `from` as layout-normalized equivalents.
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
- Controls layout-aware wrapping for supported constructs, including callable signatures, record initializers, array literal initializers, import/re-export item lists, single pipe expressions, and calls with a trailing closure.

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

- Canonical layouts are pinned by the `@test` functions in `ignis/format/*_tests.ign` (`api_tests.ign`, `layout_items_tests.ign`, `layout_expressions_tests.ign`, `layout_statements_tests.ign`, `comments_tests.ign`, `config_tests.ign`, `safety_tests.ign` and others).
- `ignis/format/corpus_tests.ign` formats the real files under `ignis/` and `std/` and requires each to come back byte for byte. Files under `test_cases/` and `example/` must format cleanly and stably, but not necessarily byte for byte.
- CLI parsing for `fmt` (explicit files, `--project`, `--check`, style overrides) is covered in `ignis/cli.ign`, and config-error messages and the `--stdin-json` line protocol in `ignis/main_tests.ign`.
- CI runs `ignis fmt --check` over `std/`, `ignis/` and `example/`.
- Shipped CLI features today include project mode, single-file mode, multiple explicit file paths, `--check`, `--stdin-json`, `--emit diff`, and `--sort-imports`.
