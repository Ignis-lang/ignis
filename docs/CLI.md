# Ignis CLI

Reference for the `ignis` command line interface.

## Overview

```bash
ignis <command> [options]
```

Commands:

- `build` - Compile a file or project.
- `check` - Run analysis/codegen checks without linking.
- `test` - Run native Ignis tests.
- `init` - Create or initialize an Ignis project.
- `build-std` - Build standard library artifacts.
- `check-std` - Check standard library codegen output.
- `check-runtime` - Syntax-check C runtime sources.
- `lsp` - Start the language server.

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

Examples:

```bash
# Build a single file
ignis build src/main.ign

# Build current project
ignis build

# Build project from a specific directory
ignis build --project ./my-app
```

## `ignis check`

Same input modes as `build`, but without final linking.

Examples:

```bash
# Check current project
ignis check

# Check single file
ignis check src/main.ign

# Analyzer-only pass (skip lowering/codegen)
ignis check --analyze-only
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
ignis test string

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

## `ignis build-std`

Build the standard library artifacts used by project compilation.

```bash
ignis build-std
```

## `ignis check-std`

Run standard library checks up to C emission without archiving.

```bash
ignis check-std
```

## `ignis check-runtime`

Syntax-check C runtime sources.

```bash
ignis check-runtime
```

## `ignis lsp`

Start the Language Server Protocol process.

```bash
ignis lsp
```
