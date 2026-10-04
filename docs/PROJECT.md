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

- `std` - Enable standard library support. `false` builds in freestanding mode
  (see below).
- `std_path` - Optional path to std root. With `std = false` it names a user
  standard library instead of the official one. `--std-path` overrides it in
  both modes. Without either, a hosted build (`std = true`) falls back to
  `IGNIS_STD_PATH`; a freestanding build never reads that variable.

#### Freestanding mode (`std = false`)

A project with `std = false` neither requires nor loads the official standard
library. `IGNIS_STD_PATH` is ignored. `--std-path` still wins over `std_path`,
as in a hosted build, and names the user standard library to use.

- Without `std_path` or `--std-path` there is no standard library: no prelude is
  auto-loaded and a `std::` import is an unresolved module.
- With either, the directory is a user standard library with its own
  `manifest.toml` in the same format as `std/manifest.toml`. Its `[modules]`
  table resolves `std::` imports and its `[auto_load]` list is the prelude.

The emitted C carries only the type prelude (`stdbool.h`, `stddef.h`,
`stdint.h`): no `ignis_rt.h`, no hosted libc or POSIX headers and no C `main`
wrapper. Every unit is compiled with `-ffreestanding` ahead of `[build] cflags`,
the runtime include directory is not passed, and nothing links `libm`. With
`bin = false` the build compiles the unit into `<out_dir>/user/obj/<name>.o` with
`cc` and `cflags` instead of only writing the C source.

The services a hosted build takes from libc come from the program's own
runtime handlers instead. Each is a function marked with an attribute, at most
one of each kind per program, and each attribute is an error in a hosted build:

| Attribute | Signature | Replaces |
| --- | --- | --- |
| `@panicHandler` | `(message: str, file: str, line: u32): void` (or `never`) | `fprintf` and `exit` for `@panic`, match exhaustion and drop state guards |
| `@allocHandler` | `(size: u64, align: u64): *mut u8` | `malloc` for the heap environment of an escaping capturing closure |
| `@freeHandler` | `(pointer: *mut u8): void` | `free` for that environment |

```ignis
@panicHandler
function onPanic(message: str, file: str, line: u32): void {
  while (true) {}
}
```

- A panic calls the handler with its message and the file and line of the
  panic site, then reaches `__builtin_unreachable()`, so the handler must not
  return. The file never names the build machine's directories: a source
  under the project root is named relative to it (`src/kernel/console.ign`),
  a source under the std root is `std/` followed by its path relative to that
  root, and any other source is its file name alone. A drop state guard
  reports the site of the value it reads, or file `""` and line `0` when no
  site is known.
- A build that can panic without a `@panicHandler` is an error (`A0211`).
  Every `match` lowers with an exhaustion panic and every use of an
  `@implements(Drop)` value is guarded, so most programs need one. The check
  runs when the build emits C, so `ignis check` does not report it.
- `@allocHandler` and `@freeHandler` come as a pair (`A0210`). A capturing
  closure that escapes its scope without them is an error (`A0206`). A closure
  with no captures never allocates and needs neither.
- A handler with the wrong signature, or on an `extern` declaration, is
  `A0209`. A second handler of one kind is `A0208`. A handler in a hosted
  build is `A0207`.

The handlers keep external linkage in the emitted unit.

C compilers may still emit calls to `memcpy` and `memset` on their own, for
example for array and record copies, bit casts, and the zeroing of enum and
droppable locals. A freestanding program provides both symbols at link time.

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

A hosted library project writes the C translation unit only. A freestanding
one (`[ignis] std = false`) also compiles it into an object file.

## `ignis init` Generation Rules

When running `ignis init`, std paths are generated in this order:

1. If `IGNIS_STD_PATH` is set and exists, it is used as `std_path`.
2. Otherwise, if `../std` exists (relative to project root), `std_path = "../std"` is used.
3. If neither is available, `std_path` is omitted.

## Testing Behavior

`ignis test` uses the same source discovery rules as the normal build pipeline.

- **Project mode** (`ignis test`) discovers `@test` functions from the analyzed module graph rooted at `[build].entry`.
- **Single-file mode** (`ignis test path/to/file.ign`) does not require `ignis.toml`, but it still needs the standard library available through `IGNIS_STD_PATH` or another explicit std-path input.
- The test harness binary is written under the configured build output directory.
- Language-level snapshots are stored in `__snapshots__/` next to the Ignis source module under test, not inside `build/`.
