---
title: Your first program
description: Create a project, write a program that compiles, and run the binary.
section: getting-started
order: 2
status: stable
---

## Create a project

`ignis init` writes the project manifest and a source entry point. It works on a new directory or
an existing one.

```bash
ignis init hello-app
cd hello-app
```

You get an `ignis.toml` and a `src/main.ign`. Passing `--lib` produces `src/lib.ign` instead, and
`--no-git` skips the `git init` that otherwise runs.

## Write something

An Ignis file is a sequence of declarations. Execution starts at `main`.

```ignis
import Io from "std::io";

const VERSION: i32 = 1;

function main(): i32 {
    Io::println("Hello");
    return VERSION;
}
```

The return value of `main` is the process exit code. `main` may also return `void`, in which case
the process exits with 0, or `Result<i32, E>`, in which case an error is reported and the process
exits with 101.

## Build and run

```bash
ignis build          # compile the project described by ignis.toml
./build/hello-app    # run the linked binary
```

`ignis build` accepts a path if you want to compile a single file without a project:

```bash
ignis build src/main.ign
```

To type-check without producing a binary — the fast loop while you are writing — use:

```bash
ignis check
```

## What just happened

The compiler parsed the file, ran its analysis phases, lowered the result through two intermediate
representations, emitted C, and handed that C to GCC. Nothing is interpreted and there is no
runtime beyond a small C support library.
