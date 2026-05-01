# Ignis Project Configuration

Ignis projects are configured with `ignis.toml` at project root.

## File Structure

```toml
[package]
name = "myapp"
version = "0.1.0"
authors = ["Your Name <you@example.com>"]
description = "My Ignis project"
keywords = ["ignis"]
license = "MIT"
repository = ""

[ignis]
std = true
std_path = "../std"
runtime_path = "../std/runtime"

[build]
bin = true
source_dir = "src"
entry = "main.ign"
out_dir = "build"
opt_level = 0
debug = false
target = "c"
cc = "cc"
cflags = []
emit = []

[formatter]
indent_width = 2
line_width = 100
use_tabs = false
sort_imports = false
```

## Sections

### `[package]`

- `name` - Project name.
- `version` - Semantic version string.
- `authors` - Author list.
- `description` - Free-text description.
- `keywords` - Search keywords.
- `license` - License identifier.
- `repository` - Source repository URL.

### `[ignis]`

- `std` - Enable standard library support.
- `std_path` - Optional path to std root.
- `runtime_path` - Optional path to runtime root.

If `runtime_path` is omitted, the resolver uses `std_path/runtime`.

### `[build]`

- `bin` - `true` for executable projects, `false` for library projects.
- `source_dir` - Source directory relative to project root.
- `entry` - Entry file relative to `source_dir`.
- `out_dir` - Build output directory.
- `opt_level` - Optimization level (`0`..`3`).
- `debug` - Include debug information.
- `target` - Target backend (currently only `"c"` is accepted by project resolver).
- `cc` - C compiler executable.
- `cflags` - Extra C compiler/linker flags.
- `emit` - Extra artifacts (`"c"`, `"obj"`).

### `[formatter]`

Formatter configuration used by `ignis fmt`.

- `indent_width` - Logical indentation width. Must be an integer in `1..=8`. Default: `2`.
- `line_width` - Preferred maximum line width for constructs with layout-aware wrapping. Must be an integer in `40..=160`. Default: `100`.
- `use_tabs` - Emit one tab per indentation level instead of spaces. `indent_width` still controls logical layout width. Default: `false`.
- `sort_imports` - Sort imports and re-exports inside each existing import group. Blank-line-separated groups stay separate. Default: `false`.

`ignis fmt` also reads a dedicated `ignisfmt.toml` file when present. Settings resolve in this order:

1. Built-in defaults.
2. `[formatter]` in `ignis.toml`.
3. `ignisfmt.toml`, or the file passed with `ignis fmt --config <path>`.
4. CLI overrides such as `--indent-width`, `--line-width`, `--use-tabs`, `--spaces`, and `--sort-imports`.

Unknown formatter keys are hard errors.

Formatter layout rules include:

- Normalized indentation, operator spacing, comma spacing, and tight generic angle brackets.
- Canonical final newline and no trailing whitespace.
- Preservation of comments, compile-time directives, intentional blank lines, declaration order, member order, match-arm order, and statement order.
- Import/re-export groups are separated from the following declaration block with a blank line.
- Consecutive `import ... from` statements with the same path are merged into one import list. Consecutive `export ... from` statements with the same path are merged the same way.
- Same-path import or re-export statements separated by an intentional blank line remain separate.
- `sort_imports = true` sorts imports/re-exports within each existing import group only; it does not sort declarations or statements.
- Callable parameter lists and record initializers drop the final trailing comma when printed on one line and add it when printed multiline.
- Import and re-export item lists wrap when the flat form exceeds `line_width`; multiline import/re-export lists include a trailing comma before `from`.
- Single pipe expressions may stay inline when they fit `line_width`; pipe chains with two or more `|>` operators format multiline.
- Empty high-level blocks (`namespace`, `record`, `enum`, `trait`, `extern`) format as inline `{}`.
- The formatter does not provide a general-purpose wrapping guarantee for every long expression shape; unsupported or unsafe rewrites fail instead of guessing.

### `[aliases]`

Import path aliases. Each key is a first-segment prefix, each value is a directory path (relative to project root or absolute).

```toml
[aliases]
mylib = "libs/mylib"
ext = "../external/packages"
```

With this configuration, `import Foo from "mylib::utils"` resolves to `libs/mylib/utils.ign` (or `libs/mylib/utils/mod.ign` if that directory exists).

- The `"std"` key is reserved and cannot be used in `[aliases]`.
- Alias paths must point to existing directories.
- Only the first segment of the import path is matched (e.g., `"mylib"` in `"mylib::sub::mod"`).
- Single-file builds do not support user aliases (only the implicit `std` alias is available).

## Binary vs Library Projects

Binary project:

```toml
[build]
bin = true
entry = "main.ign"
```

Library project:

```toml
[build]
bin = false
entry = "lib.ign"
```

## `ignis init` Generation Rules

When running `ignis init`, std paths are generated in this order:

1. If `IGNIS_STD_PATH` is set and exists, it is used as `std_path`.
2. Otherwise, if `../std` exists (relative to project root), `std_path = "../std"` is used.
3. If neither is available, `std_path` and `runtime_path` are omitted.

`runtime_path` is written only when `<std_path>/runtime` exists.

## Testing Behavior

`ignis test` uses the same source discovery rules as the normal build pipeline.

- **Project mode** (`ignis test`) discovers `@test` functions from the analyzed module graph rooted at `[build].entry`.
- **Single-file mode** (`ignis test path/to/file.ign`) does not require `ignis.toml`, but it still needs the standard library available through `IGNIS_STD_PATH` or another explicit std-path input.
- The test harness binary is written under the configured build output directory.
- Language-level snapshots are stored in `__snapshots__/` next to the Ignis source module under test, not inside `build/`.
