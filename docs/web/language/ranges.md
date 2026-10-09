---
title: Ranges
description: The range expressions a..b and a..=b, the values they produce, and the loops they count.
section: language
order: 15
status: stable
---

`a..b` is the range from `a` up to, and not including, `b`. `a..=b` includes `b`. Both
bounds are required and both are integers of one type; open-ended ranges like `a..` and
`..b` are rejected.

```ignis
let exclusive = 0..4;                     // Range<i32>: 0, 1, 2, 3
let inclusive: RangeInclusive<u8> = 0..=255;
```

## Range values

A range is a value of the builtin type `Range<T>` (`a..b`) or `RangeInclusive<T>` (`a..=b`).
It can be bound, passed, returned and stored in a record field.

```ignis
function width(span: Range<i32>): i32 {
    return span.end - span.start;
}

function make(first: i32, last: i32): Range<i32> {
    return first..last;
}
```

`r.start` and `r.end` read the bounds; a range has no methods and no other members. A range
is `Copy` and is never dropped. Its C representation is a plain struct of two `T` values, so
it needs no standard library.

## Loops

`for` over a range is a counted loop. The bounds are evaluated once. Inclusive loops use a
flag when needed to avoid overflow; literal bounds ending below the type's maximum use `<=`.
A range ending on the maximum of its type still terminates:

```ignis
let mut bytes: u32 = 0;

for (let value: u8 of 0..=255) {
    bytes += 1;
}
```

The optional annotation on the loop variable selects the element type; without one, the
bounds decide.

## Element types

The expected type decides first: `for (let i: u8 of 0..=255)` and
`let r: Range<u16> = 0..8` type their literals as `u8` and `u16`. Without an expected type,
the bounds decide: a literal bound adopts the type of the other bound, and two literal
bounds are `i32`. Bounds of two different integer types are an error, and so is a bound
that is not an integer (`A0231`).

## Precedence

A range binds looser than `||` and tighter than `|>`, and it is not associative: `a..b..c`
is an error (`I0002`). `a + 1..b * 2` is `(a + 1)..(b * 2)`.

## Array ranges

A vector literal whose only element is a range of integer literals is the array of its
values:

```ignis
let small: u8[4] = [0..4];                // [0, 1, 2, 3]
let bytes: u8[256] = [0..=255];
let signed: i8[256] = [-128..=127];
```

The bounds must be integer literals (`A0233`), so the array has a fixed size and its values
are checked at compile time. The range must not be empty (`A0236`), may hold at most 65536
elements (`A0235`), and must not be combined with other elements (`A0234`). A
parenthesized range, `[(0..3)]`, is an ordinary array holding one `Range`.

A user-declared record, enum or alias named `Range` or `RangeInclusive` takes precedence
over the builtin type of that name.
