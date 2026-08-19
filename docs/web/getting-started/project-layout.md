---
title: Project layout
description: What ignis init writes, and every section of the ignis.toml manifest.
section: getting-started
order: 3
status: stable
---

A project is a directory with an `ignis.toml` at its root. Every compiling command finds it by
walking up from the working directory, which is why `ignis build` works from anywhere inside the
tree.

```bash
ignis init hello-app        # binary project
ignis init mylib --lib      # library project
ignis init .                # initialize the current directory
ignis init scratch --no-git # skip the git init that otherwise runs
```

`init` writes the manifest and an entry file — `src/main.ign` for a binary, `src/lib.ign` for a
library — and never overwrites an existing one.

## Where init points std

The generated `std_path` is resolved in order:

1. `IGNIS_STD_PATH`, if it is set and the directory exists.
2. `../std`, relative to the project root, if it exists.
3. Neither, in which case `std_path` and `runtime_path` are left out entirely.

`runtime_path` is written only when `<std_path>/runtime` exists. If a fresh project cannot find the
standard library, this is the setting to check first.

## The manifest

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

### [package]

Metadata: `name`, `version`, `authors`, `description`, `keywords`, `license`, `repository`. The
name is what the produced binary is called.

### [ignis]

`std` turns standard library support on. `std_path` and `runtime_path` point at their roots; if
`runtime_path` is omitted the resolver falls back to `<std_path>/runtime`.

### [build]

| Key | Meaning |
| --- | --- |
| `bin` | `true` for an executable, `false` for a library |
| `source_dir` | Source directory, relative to the project root |
| `entry` | Entry file, relative to `source_dir` |
| `out_dir` | Where build output lands |
| `opt_level` | Optimization level, `0` to `3` |
| `debug` | Include debug information |
| `target` | Backend — the project resolver accepts only `"c"` today |
| `cc` | The C compiler to invoke |
| `cflags` | Extra flags passed to the compiler and linker |
| `emit` | Extra artifacts to keep: `"c"`, `"obj"` |

A binary project sets `bin = true` with `entry = "main.ign"`; a library sets `bin = false` with
`entry = "lib.ign"`.

### [formatter]

The four settings `ignis fmt` reads, documented in [formatter](/docs/tooling/formatter). They can
also live in a dedicated `ignisfmt.toml`, which takes precedence over this section.

### [aliases]

Import path aliases. Each key is matched against the first segment of an import path, each value is
a directory relative to the project root or absolute.

```toml
[aliases]
mylib = "libs/mylib"
ext = "../external/packages"
```

With that, `import Foo from "mylib::utils"` resolves to `libs/mylib/utils.ign`, or
`libs/mylib/utils/mod.ign` when that directory exists.

Four rules that bite:

- `"std"` is reserved and cannot appear here.
- An alias path must point at a directory that exists.
- Only the first segment is matched — `mylib` in `mylib::sub::mod`.
- Single-file builds ignore user aliases. Only the implicit `std` alias works there.
