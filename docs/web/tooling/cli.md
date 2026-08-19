---
title: CLI reference
description: The ignis command — building, checking, testing, formatting and creating projects.
section: tooling
order: 1
status: stable
---

```bash
ignis <command> [options]
```

| Command | What it does |
| --- | --- |
| `build` | Compile a file or a project and link the executable. |
| `check` | Run the analysis and codegen checks without linking. |
| `fmt` | Rewrite source to the canonical formatting. |
| `doc` | Extract API documentation from doc comments. |
| `test` | Run language-level tests. |
| `init` | Create or initialize a project. |
| `build-std` | Build the standard library artifacts. |
| `check-std` | Check the standard library's codegen output. |
| `check-runtime` | Syntax-check the C runtime sources. |
| `lsp` | Start the language server. |

Every compiling command works in two modes: pass a path to act on one file, or pass nothing and
the project is detected by walking up from the current directory looking for `ignis.toml`.

## init

```bash
ignis init hello-app        # new binary project in ./hello-app
ignis init .                # initialize the current directory
ignis init mylib --lib      # library project, with src/lib.ign
ignis init scratch --no-git # skip the git init that otherwise runs
```

It writes `ignis.toml` and an entry file, and works on both new and existing directories. An
existing `ignis.toml` or entry file is never overwritten.

## build

```bash
ignis build src/main.ign      # single file
ignis build                   # the project around the working directory
ignis build --project ./app   # a project somewhere else
```

## check

Same inputs as `build`, stopping before the link step. This is the fast loop while writing.

```bash
ignis check
ignis check src/main.ign
ignis check --analyze-only    # analysis only, no lowering or codegen
```

## test

Runs the top-level functions annotated with `@test`.

```bash
ignis test                    # every test in the project
ignis test string             # only tests whose qualified name contains "string"
ignis test src/example.ign    # tests from one file
ignis test --update-snapshots # create or replace snapshot baselines
```

Tests run in a deterministic order and the runner continues past failures. Snapshots live in a
`__snapshots__/` directory: next to the module under test in project mode, next to the source file
in single-file mode. The two roots are separate — do not expect a snapshot written in one mode to
be found by the other.

## doc

Extracts the API documentation from `///` comments and prints it as JSON.

```bash
ignis doc                      # the project around the working directory
ignis doc std/io/mod.ign       # one module
ignis doc --output api.json    # write to a file instead of stdout
```

Extraction runs the semantic phases and stops: no lowering, no code generation, no linking. Each
entry carries the module, the fully qualified path, the kind, the signature as it is written in
source, the doc comment, and the members of a record, enum or trait.

Visibility is reported rather than filtered. A namespace member is private unless exported, yet
those members are the callable surface of a module — `Io::println` among them — so the decision
about what counts as published API belongs to whatever renders the output.

## fmt

Rewrites source in place to the canonical layout: two-space indentation, a 100-column line, spaces
rather than tabs, and imports left in the order you wrote them.

```bash
ignis fmt src/main.ign
ignis fmt a.ign b.ign         # several explicit files
ignis fmt --check src/main.ign
ignis fmt --emit diff src/main.ign
```

`--check` validates without rewriting, which is what a CI job wants. `--emit diff` prints a unified
diff instead of touching the file.

## Not the same thing: std::cli

`std::cli` is a standard-library module for Ignis programs that parse their own arguments. It has
nothing to do with the `ignis` command above. It is a deliberately bounded parser: declared boolean
flags, declared valued options, positional arguments in order, and a `--` terminator. `--opt=value`,
grouped short flags and subcommands are out of scope by design.
