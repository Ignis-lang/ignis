# Ignis CLI

Reference for the `ignis` command line interface.

For Ignis programs that need to parse their own command-line arguments, use the
standard-library `std::cli` module. It provides a bounded parser for declared
flags, valued options, positional arguments, and `--` terminators.

## Overview

```bash
ignis <command> [options]
```

Commands:

- `build` - Compile a file or project.
- `check` - Run analysis checks without codegen or linking.
- `fmt` - Rewrite Ignis source to canonical formatting.
- `test` - Run native Ignis tests.
- `test-std` - Run the standard library's native tests.
- `doc` - Extract API documentation from doc comments.
- `init` - Create or initialize an Ignis project.
- `build-std` - Build standard library artifacts.
- `check-std` - Check standard library codegen output.
- `lsp` - Start the language server.

`ignis --help` lists every command and flag. Without a command, `ignis <path>`
builds the path.

## Progress output

`build`, `check`, `build-std`, `check-std`, `test` and `test-std` can show a
live line at the bottom of stderr while they run: the current phase
(`discover`, `parse`, `analyze`, `lower`, `mono`, `ownership`, `lir`,
`codegen`, `cc`, `link`, `test`), a bar when the phase counts steps (modules,
C units, tests), what it is on and the elapsed time:

```
⠼ lower      ███████████░░░░░░░░░ 112/198  ignis/parser/statements.ign  6.3s
```

The line is redrawn in place, at most every 80 ms, and never reaches the last
column of the terminal, whose width is read before each redraw. Diagnostics
and other output are printed above it, and it is erased before the closing
`✓`/`✗` line, so what stays on screen is what a run without it prints.

`--progress <MODE>` (or `--progress=<MODE>`) chooses when it is drawn:

- `auto` (default): when stderr is a terminal and `TERM` is not `dumb`.
- `live`: always, even when stderr is a file or a pipe.
- `plain`: never.

With the line off, stdout and stderr carry exactly the bytes they would
without it, which is what CI logs, editors and the fixture baselines see.

## Errors reported after analysis

The phases from `lower` on run only when `analyze` reported no errors, because
they assume every name resolved and every expression typed (`-v` prints
`lower: skipped (analyze errors)`). The checks that read the lowered program
are therefore reported only once the earlier errors are fixed: the generic
bounds of the types and calls inside function bodies (A0247), closure
captures, and ownership and borrowing (the `O` codes). A file with a type
mismatch and a `Channel<T>` of a type that is not `Send` reports the mismatch
first and the bound on the next run.

## `ignis init`

Initializes a project directory with:

- `ignis.toml`
- `src/main.ign` (binary project)
- or `src/lib.ign` (library project)

Usage:

```bash
ignis init <name> [options]
```

Examples:

```bash
# Create a new binary project in ./hello-app
ignis init hello-app

# Initialize current directory as an Ignis project
ignis init .

# Create a library project
ignis init mylib --lib

# Disable git initialization
ignis init scratch --no-git
```

Behavior:

- `init` works for both new and already-existing directories.
- By default, `git init` is executed (disable with `--no-git`).
- Existing `ignis.toml` or entry file is never overwritten.

## `ignis build`

Compile source code in one of two modes:

- Single file mode: `ignis build path/to/file.ign`
- Project mode: `ignis build` (auto-detects `ignis.toml` upward from current directory)
  or `ignis build path/to/dir` (auto-detects `ignis.toml` upward from that directory)

A `.ign` argument always selects single file mode, whether the path is
absolute, relative or goes through `..`, even when an `ignis.toml` governs the
file. That `ignis.toml` is not read: the file gets the standard library from
`--std-path` or `IGNIS_STD_PATH` (one of them is required), no
`[aliases]`, no `[features]` and none of the project's `[build]` settings, and
its artifacts go to the working directory. To build a project, pass its
directory or no path. The same rule applies to `ignis check` and `ignis test`.

Examples:

```bash
# Build a single file
ignis build src/main.ign

# Build current project
ignis build

# Build the project in a specific directory
ignis build ./my-app
```

`--linker <L>` overrides `[build] linker` and chooses the linker the link step
uses. Any non-empty name is accepted and passed to the C driver as
`-fuse-ld=<L>` (`mold`, `lld`, `gold`, `bfd`, ...); `default` and `ld` keep the
system linker. An empty name is rejected like a missing one. Without it, and
without `[build] linker`, a Linux host links with mold when the C driver can use
it, which is checked once per linking command by running
`<cc> -fuse-ld=mold -Wl,--version`; otherwise it uses the system linker (see
`docs/PROJECT.md`).

```bash
# Link with GNU ld (bfd) instead of the default selection
ignis build --linker bfd
```

`--std-archive` turns on `[build] std_archive` for one build (it cannot turn off
a project's own `std_archive = true`) and links the standard library
from `libignis_std.a` instead of compiling it into the program's unit. The build
first makes sure the archive under its output directory (`<out_dir>/std/lib`) is
current, rebuilding it with the C backend when its stamp says it is stale, then
compiles the program's unit with only the project's own definitions and the
generic instances it uses, and links the archive. It works with `--target c` and
`--target qbe`; the archive itself is always built with C. Without the flag the
whole program is one unit, as before. The flag is accepted only by `build`:
`check`, `test`, `build-std` and `fmt` reject it with
`error: --std-archive is a build option: only build links the standard library`,
and the commands that take no build flags (`check-std`, `test-std`, `doc`)
report it as an unknown argument. A `std = false` build rejects the mode, since
it has no standard library to link. The mode only affects a build that links an
executable: `check`, `test` and `build -c` ignore `std_archive`, and an object
written by `-c` keeps every std definition it uses.

```bash
# Link std from the archive and compile the program with QBE
ignis build --std-archive --target qbe
```

A project with `[ignis] std = false` builds in freestanding mode (see
`docs/PROJECT.md`): the emitted C has no runtime header, no hosted libc or
POSIX headers and no C `main` wrapper, every unit is compiled with
`-ffreestanding`, and nothing links `libm`. With `[build] bin = false` the build
writes an object file (`<name>.o`) instead of only the C source. `--std-path`
still overrides `[ignis] std_path` and names the user standard library a
freestanding build loads, while `IGNIS_STD_PATH` is ignored.

## `ignis check`

Same input modes as `build`, stopping after analysis: no code generation and no linking.

Examples:

```bash
# Check current project
ignis check

# Check single file
ignis check src/main.ign

```

## `ignis test`

Run language-level tests declared with `@test`.

Supported modes:

- Project mode: `ignis test`
- Single-file mode: `ignis test path/to/file.ign`

Examples:

```bash
# Run all tests in the current project
ignis test

# Run only tests whose fully-qualified name contains "string"
ignis test --filter string

# Run tests from a single file
ignis test src/example.ign

# Create or replace language-level snapshots
ignis test --update-snapshots
```

Behavior:

- Tests are top-level functions annotated with `@test`.
- The runner executes tests in deterministic order and continues after failures.
- `--update-snapshots` enables creation and replacement of `__snapshots__/` baselines.
- Project mode stores snapshots next to the module under test.
- Single-file mode stores snapshots next to the single Ignis source file.

## Standard-Library CLI Helpers

`std::cli` is separate from the compiler's `ignis` command. Use it inside Ignis
programs to build small, deterministic command-line parsers.

```ignis
import Cli from "std::cli";
import Io from "std::io";

function main(): i32 {
  let mut command: Cli::Command = Cli::Command::new("tool");
  command.aboutText("Build project artifacts.");
  command.flag("verbose", "v", "Enable verbose output.");
  command.option("output", "o", "Write output file.");

  match (command.parseProcess()) {
    Result::OK(matches) -> {
      if (matches.has("verbose")) {
        Io::println("verbose enabled");
      }
      return 0;
    },
    Result::ERROR(error) -> {
      Io::println(error.message.toStr());
      return 1;
    },
  };
}
```

Supported parser surface:

| Form | Status |
|---|---|
| `--flag`, `-f` | Supported for declared boolean flags. |
| `--output file`, `-o file` | Supported for declared valued options. |
| Positional arguments | Preserved in order. |
| `--` terminator | Supported; later tokens become positionals. |
| `--opt=value`, grouped short flags, subcommands | Intentionally out of scope for the bounded parser. |

For terminal-aware CLI output, pair `std::cli` with `std::terminal`. The terminal
module exposes semantic APIs such as `Terminal::Style::new()`,
`Terminal::colorText(...)`, `Terminal::Screen::clear()`,
`Terminal::Cursor::moveTo(...)`, and terminal capability checks.

## `ignis fmt`

Rewrite Ignis source files in place to the canonical layout defined in [`docs/FORMATTING.md`](./FORMATTING.md). That document's "v0.5" is the formatter policy's own revision number; it is independent of the compiler release (the compiler on `main` is 0.4.x).

Supported modes:

- Project mode: `ignis fmt`
- Single-file mode: `ignis fmt path/to/file.ign`
- Explicit multi-file mode: `ignis fmt a.ign b.ign c.ign`
- Explicit project mode: `ignis fmt --project ./my-app`
- NDJSON batch stdin: `ignis fmt --stdin-json`

Examples:

```bash
# Format the current project in place
ignis fmt

# Format one file in place
ignis fmt src/main.ign

# Format multiple files explicitly
ignis fmt std/fs/mod.ign std/io/mod.ign std/path/mod.ign

# Check whether a file is already canonical without rewriting it
ignis fmt --check src/main.ign

# Override indentation width for one run
ignis fmt --indent-width 4 src/main.ign

# Emit tab-indented output for one run
ignis fmt --use-tabs src/main.ign

# Sort imports for one run
ignis fmt --sort-imports src/main.ign

# Emit a diff instead of rewriting
ignis fmt --emit diff src/main.ign

# Batch format multiple virtual files over NDJSON
ignis fmt --stdin-json
```

Behavior:

- Files are rewritten only when the canonical output differs from the current bytes.
- `--check` performs the same formatting and safety validation, but it does not rewrite files.
- Multiple explicit file paths are formatted in the order provided. All explicit paths are validated before formatting starts.
- Project mode walks the configured source directory recursively and formats every `.ign` file.
- Output is accepted only if the formatted text reparses and passes formatter safety validation.
- Invalid or unsafe input fails the command and leaves the original file unchanged.
- Supported style overrides are `--indent-width`, `--line-width`, `--use-tabs`, `--spaces`, and `--sort-imports`.
- Formatter settings resolve in this order: built-in defaults, then `[formatter]` in `ignis.toml`, then `ignisfmt.toml` (or `--config <path>`), then CLI flags.
- The shipped defaults are `indent_width = 2`, `line_width = 100`, `use_tabs = false`, and `sort_imports = false`.
- Formatter config keys are `indent_width`, `line_width`, `use_tabs`, and `sort_imports`. Unknown formatter keys are rejected.
- `indent_width` must be in `1..=8`; `line_width` must be in `40..=160`.
- `--spaces` overrides configured tab indentation back to spaces. `--use-tabs` and `--spaces` conflict.
- `fmt` preserves declaration order, member order, attribute order, match-arm order, and statement order.
- Consecutive `import ... from` statements with the same path are merged into one import list. Consecutive `export ... from` statements with the same path are merged the same way.
- Same-path imports or re-exports separated by an intentional blank line remain separate.
- `--sort-imports` sorts imports/re-exports within each existing import group only; it preserves comment-separated and blank-line-separated groups.
- Long import and re-export item lists wrap when the flat form exceeds `line_width`; multiline lists include a trailing comma before `from`.
- Callable parameter lists and record initializers drop the final trailing comma when they fit on one line and add it when they print multiline.
- Single pipe expressions may stay inline when they fit `line_width`; pipe chains with two or more `|>` operators format multiline.
- Empty high-level blocks (`namespace`, `record`, `enum`, `trait`, `extern`) format as inline `{}`.
- There is no general-purpose wrapping guarantee for every long expression shape; unsafe or unsupported rewrites fail instead of guessing.
- `--stdin-json` reads one JSON object per line with `path` and `text` fields and emits one JSON result per line with `path`, `changed`, and either `formatted`, `diff`, or `error`.
- `--emit diff` works in file, multi-file, project, and `--stdin-json` modes.
- Formatter failures are safety/modeling failures, not lint diagnostics; valid source is expected to format successfully.

## `ignis build-std`

Build the standard library artifacts used by project compilation.

```bash
ignis build-std
```

## `ignis check-std`

Run standard library checks up to C emission without archiving.

```bash
ignis check-std
ignis check-std -o out
```

`-o` / `--output-dir` names the directory the emitted C is written to (default `build`).

- It emits one translation unit for the whole library, `<dir>/ignis_std.c`, and writes no `ignis_std.h`.
- It resolves the std root the way `build-std` does: a governing `ignis.toml` `std_path` wins over `IGNIS_STD_PATH`.

## `ignis lsp`

Start the Language Server Protocol process.

```bash
ignis lsp
```

Known limitation: the language server does not yet report `A0217` for a capturing closure passed to a function-typed extern parameter, nor `A0264` for a capturing closure taken as a function pointer (`fn(A) -> R`). The CLI reaches both in the capture pass it runs over the HIR, a stage the editor analysis does not perform, so run `ignis check` to validate function pointers and FFI boundaries even when the editor is silent.
