---
title: Modules and imports
description: Namespaces, import and export forms, extern blocks, and how an import path is resolved.
section: language
order: 11
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
