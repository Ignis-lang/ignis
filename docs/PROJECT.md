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
