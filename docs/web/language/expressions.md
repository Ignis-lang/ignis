---
title: Expressions and operators
description: Literals, the operator set, casts, the try operator and the pipe.
section: language
order: 10
status: stable
---

## Literals

```ignis
function examples(): i32 {
    let a: i32 = 42;
    let b: f64 = 3.14;
    let c: boolean = true;
    let d: char = 'a';
    let e: str = "hello";
    let f: i32 = 0xFF;
    let g: i32 = 0b1010;
    let h: atom = :ok;
    let i: i32[3] = [1, 2, 3];

    return a;
}
```

A numeric literal adapts to the type the context asks for, as long as the value fits: `let a: u8 =
255;` is fine and `let x: u8 = 256;` is a compile-time error. Float literals move between `f32` and
`f64` the same way.

Same-sign integers widen implicitly — `u8` through `u64`, `i8` through `i64` — so assigning a `u8`
to a `u32` needs no cast. Crossing signs does: `u8` to `i32` requires an explicit `as`.

## Template literals

A backtick literal builds an owned `String`, and `${ }` drops an expression into the surrounding
text. It may span several lines, and the newlines are part of the value.

```ignis
function greet(name: str, version: i32): String {
    let banner: String = `hello ${name} v${version}`;
    let block: String = `first line
second line`;

    return banner.concat(block);
}
```

A slot takes any expression, another template included, and the usual string escapes apply plus
`` \` `` for a backtick and `\${` for text that must not open a slot.

```ignis
let nested: String = `outer ${`inner ${version}`}`;
let raw: String = `a \` b \${notASlot}`;   // a ` b ${notASlot}
```

Interpolation goes through `String::concat`, so a slot holds a `String`, `str`, `char`, `boolean`,
or any integer or float. A record needs an explicit conversion — `${point.toString()}` — because
the compiler does not infer a method's type parameters from its arguments yet.

Interpolating a variable, field, or element borrows it instead of moving it, so an owned `String`
stays live across as many templates as you like. A slot that already holds a reference is not
borrowed twice, so a `&String` parameter interpolates directly.

```ignis
function render(label: &String, owned: &String): String {
    let first: String = `[${label}]`;

    return `${first}${owned}`;
}
```


## Operators

| Group | Operators |
| --- | --- |
| Arithmetic | `+` `-` `*` `/` `%` |
| Comparison | `==` `!=` `<` `>` `<=` `>=` |
| Logical | `&&` `\|\|` `!` |
| Bitwise | `&` `\|` `^` `~` `<<` `>>` |
| Assignment | `=` `+=` `-=` `*=` `/=` `%=` `&=` `\|=` `^=` `<<=` `>>=` |
| Increment | `x++` `x--` `++x` `--x` |
| Cast | `expr as Type` |
| Pipe | `\|>` |
| Try | `expr!` |
| Ternary | `cond ? a : b` |

## Casts

```ignis
function main(): i32 {
    let value: i32 = 42;
    let a: i64 = 42 as i64;
    let p: *i32 = &value as *i32;

    return a as i32;
}
```

## The try operator

Postfix `!` unwraps the success variant of a `@lang(try)` enum, or returns the failure variant from
the enclosing function.

```ignis
@lang(try)
enum Result<T, E> {
    OK(T),
    ERROR(E),
}

function divide(a: i32, b: i32): Result<i32, str> {
    if (b == 0) {
        return Result::ERROR("division by zero");
    }

    return Result::OK(a / b);
}

function compute(): Result<i32, str> {
    let x = divide(10, 2)!;
    let y = divide(20, 4)!;

    return Result::OK(x + y);
}
```

`expr!` is shorthand for a match: the success arm yields the payload, the failure arm returns the
failure variant unchanged. Three conditions have to hold — the expression's type is a `@lang(try)`
enum, the function's return type is compatible with it, and the error type matches exactly. That
last one is the usual reason a `!` refuses to compile.

## The pipe operator

`lhs |> rhs` passes the left value as the first argument to the call on the right, and it is
left-associative.

```ignis
import Io from "std::io";
import String from "std::string";

function add(x: i32, y: i32): i32 {
    return x + y;
}

function main(): i32 {
    add(2, 3)
        |> String::create
        |> Io::println;

    return 0;
}
```

## Ternary

```ignis
function pick(x: i32, isReady: boolean): i32 {
    let a: i32 = isReady ? 1 : 0;
    let b: i32 = x > 10 ? x : 10;

    return a + b;
}
```
