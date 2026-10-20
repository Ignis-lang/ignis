---
title: Modules and imports
description: Namespaces, import and export forms, extern blocks, and how an import path is resolved.
section: language
order: 12
status: stable
---

A namespace groups declarations under a name reached with `::`.

```ignis
namespace Math {
    function add(a: i32, b: i32): i32 {
        return a + b;
    }
}

function main(): i32 {
    return Math::add(1, 2);
}
```

Inside the namespace, and inside namespaces nested in it, members are reached by their bare names.
Outside, a member is reached only by its path; nothing brings a namespace's members into scope by
their bare names. That holds for records, enums, type aliases, traits, constants, functions and
nested namespaces, wherever the name is written: expressions, patterns, type annotations, bounds
and `@implements(...)`. A path has to start at a name reachable from where it is written, so a
nested `Outer::Inner` is written `Inner::f` only inside `Outer`.

```ignis
namespace Geometry {
    record Point {
        public x: i32;
    }

    function origin(): Point {
        return Point { x: 0 };
    }
}

function main(): i32 {
    let point: Geometry::Point = Geometry::origin();
    return point.x;
}
```

Writing `Point` in `main` is an error (A0263): `'Point' is declared in namespace 'Geometry'; write
'Geometry::Point'`. Importing a namespace from another module brings its members the same way,
still reached by their path.

## Imports and exports

```ignis
import Io from "std::io";
import println, print from "std::io";

export function run(): void {
    Io::println("run");
    return;
}
```

Two forms are worth knowing beyond the obvious ones.

`import _ from "..."` loads a module for its side effects — namespace contributions, mostly —
without binding anything into scope. The `_` has to be alone; combining it with named items is an
error.

```ignis
import _ from "std::libc::memory";
```

`export X from "..."` imports and re-exports in one statement. The symbol lands in the current
module's scope and is visible to anything importing from it. There is no `export _ from`.

```ignis
export CType from "./primitives";
```

## Path resolution

The string after `from` is resolved in three steps, in this order:

1. **Alias.** The first segment is matched against the `[aliases]` table in `ignis.toml`. An exact
   segment match is tried first, then a prefix match for shorter keys. An alias pointing at a
   directory tries `<dir>/mod.ign` and falls back to `<dir>.ign`.
2. **Relative path.** Anything starting with `./` or `../` resolves from the importing file.
3. **Bare path.** Everything else resolves from the project's source root.

```toml
# ignis.toml
[aliases]
mylib = "libs/mylib"
"@" = "./src"
```

```ignis
import Utils from "mylib::utils";
import Token from "@token::token";
import Lexer from "@lexer";
```

The `std` alias always exists and cannot be overridden.

## Extern blocks

An extern block declares symbols that come from C. It holds function signatures and constants
without initializers, and the block name may be a qualified path.

```ignis
extern libc {
    function puts(s: str): i32;
    const BUFSIZ: i32;
}

extern __errno {
    const ENOENT: i32;
    const EACCES: i32;
}
```

Outside the block, its members are reached by their path, like a namespace's: `libc::puts`,
`__errno::ENOENT`. A bare `puts` is an error (A0263).

By default an extern function borrows what you pass it. Mark a parameter `@takes` when the C side
takes ownership — see [ownership](/docs/language/ownership).

## Extension methods

`@extension(Type)` adds a method to an existing type, including a primitive. The `mut` form takes
the receiver mutably.

```ignis
@extension(i32)
function doubled(value: i32): i32 {
    return value * 2;
}

@extension(i32, mut)
function bump(value: i32): i32 {
    return value + 1;
}

function main(): i32 {
    let mut x: i32 = 20;
    let y: i32 = x.doubled();

    return y + x.bump();
}
```
