---
title: Template literals
description: Backtick strings with ${} interpolation, what a slot accepts, and why interpolating a variable does not move it.
section: language
order: 11
status: stable
---

A template literal is delimited by backticks and evaluates to an owned `String`. `${ }` drops an
expression into the surrounding text.

```ignis
import String from "std::string";

function banner(name: str, version: i32): String {
    return `hello ${name} v${version}`;
}
```

Without a slot it is still a `String`, not a `str`.

```ignis
let plain: String = `no interpolation`;
```

A literal may span several lines, and the newlines are part of the value.

```ignis
let block: String = `first line
second line`;
```

## Slots

A slot takes any expression, another template included.

```ignis
function describe(version: i32): String {
    let computed: String = `sum=${version + 1}`;
    let nested: String = `outer ${`inner ${version}`}`;

    return computed.concat(nested);
}
```

Braces, strings and blocks written inside a slot are scanned as ordinary code, so a `}` closes the
slot only when it matches the `${` that opened it.

## Escapes

The string escapes all apply, plus `` \` `` for a literal backtick and `\${` for text that must not
open a slot.

```ignis
let raw: String = `a \` b \${notASlot}`;   // a ` b ${notASlot}
```

## What a slot accepts

Interpolation resolves through `String::concat`, so a slot holds a `String`, a `str`, a `char`, a
`boolean`, or any integer or float.

```ignis
function render(label: str, count: i32, ready: boolean, initial: char): String {
    return `${label}: ${count} ready=${ready} initial=${initial}`;
}
```

A record needs an explicit conversion. The compiler does not infer a method's type parameters from
its arguments yet, so a generic conversion cannot be selected from the slot itself.

```ignis
import String from "std::string";

record Point {
    x: i32;
    y: i32;

    public toString(&self): String {
        return `(${self.x}, ${self.y})`;
    }
}

function show(point: &Point): String {
    return `point=${point.toString()}`;
}
```

A slot whose type has no `concat` overload reports `A0100` and lists the signatures that do exist.

## Ownership

A slot that names a place — a variable, a field, an element — is borrowed, not moved, so the value
stays usable afterwards.

```ignis
import String from "std::string";

function twice(): String {
    let owned: String = String::create("kept");

    let first: String = `a=${owned}`;
    let second: String = `b=${owned}`;

    return first.concat(second);
}
```

A temporary is passed by value, which is what you want: there is nothing left to keep alive.

```ignis
let text: String = `now=${String::create("temporary")}`;
```

A slot that already holds a reference is not borrowed a second time, so a `&String` parameter
interpolates directly.

```ignis
function label(name: &String): String {
    return `[${name}]`;
}
```

## How it compiles

The literal is desugared into a `String::create` call followed by one `concat` per part.

```ignis
let direct: String = `a=${1} b`;
// builds the same value as
// String::create("a=").concat(1).concat(" b")
```

Empty chunks are dropped, so `` `${a}${b}` `` performs two concatenations and not four. Diagnostics
point at the slot you wrote, not at the generated call.

The formatter prints a template verbatim: whitespace inside `${ }` is left exactly as written.
