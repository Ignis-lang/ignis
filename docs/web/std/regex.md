---
title: Regular expressions
description: Linear-time regex matching with std::regex — syntax, captures, replacement, limits and what is not supported.
section: std
order: 2
status: stable
---

`std::regex` matches regular expressions in the style of RE2. A pattern is parsed, compiled to a
small program and run by a Pike VM, so a search takes time proportional to the text times the
program. It never backtracks, which means a pattern such as `(a*)*b` cannot hang your program.

```ignis
import Match, Regex from "std::regex";

function main(): i32 {
    let digits: Regex = Regex::new("\\d+").unwrap();
    let found: Match = digits.find("abc 123").unwrap(); // bytes 4..7
    let number: String = found.textIn("abc 123");       // "123"

    return number.length() as i32;
}
```

A pattern is a `str`, so each regex backslash is written `\\` in an Ignis string literal: `"\\d+"`
is the pattern `\d+`.

## Choosing a method

| You need | Use | Returns |
| --- | --- | --- |
| Only a yes or no | `isMatch(text)` | `boolean` (cheapest, stops at the first match) |
| The first match | `find(text)` | `Option<Match>` |
| Every match | `findAll(text)` | `Vector<Match>` |
| The first match with its groups | `captures(text)` | `Option<Captures>` |
| Every match with its groups | `capturesAll(text)` | `Vector<Captures>` |
| Replace the first match | `replace(text, replacement)` | `String` |
| Replace every match | `replaceAll(text, replacement)` | `String` |
| Cut the text at the matches | `split(text)` | `Vector<String>` |

`Regex::new(pattern)` returns `Result<Regex, RegexError>`. Compile a pattern once and reuse the
`Regex`; it holds the compiled program and its group names, and every call allocates its own
working state.

## Matches and offsets

A `Match` stores byte offsets, never a view of the text: `start()`, `end()`, `length()` and
`isEmpty()`. Offsets are into the UTF-8 bytes and always fall on scalar boundaries. To get the
matched text back, pass the same text to `textIn(haystack)`, which returns a copy.

The match rules worth knowing:

- **Leftmost-first.** Of the matches starting at the leftmost position, the one the pattern prefers
  wins. Alternatives are tried in order, so `a|ab` on `"ab"` matches `"a"`. Greedy repetition
  prefers more, lazy repetition prefers fewer.
- **Unanchored by default.** The search can start anywhere unless the pattern begins with `^`.
- **Non-overlapping.** `findAll` returns matches left to right. After an empty match it resumes one
  scalar further on, so `a*` on `"baa"` gives `0..0`, `1..3` and `3..3`.
- **Invalid UTF-8.** An invalid byte is read as U+FFFD and spans one byte.

## Captures

`captures` returns a `Captures` with one entry per group. Group 0 is the whole match and group `n`
is the `n`-th `(` in the pattern.

```ignis
import Captures, Match, Regex from "std::regex";

function main(): i32 {
    let date: Regex = Regex::new("(?<year>\\d{4})-(\\d\\d)").unwrap();
    let captures: Captures = date.captures("on 2024-05").unwrap();
    let year: Match = captures.name("year").unwrap(); // bytes 3..7

    return year.length() as i32;
}
```

| Method | Description |
| --- | --- |
| `group(index)` | `Option<Match>` for group `index`; 0 is the whole match |
| `name(name)` | `Option<Match>` for the named group |
| `whole()` | The whole match, group 0 |
| `groupCount()` | Capturing groups, the whole match not included |
| `length()` | Groups including the whole match: `groupCount() + 1` |

A group that did not take part in the match is `Option::NONE`. For `(a)|(b)` on `"b"`, group 1 is
`Option::NONE` and group 2 is `0..1`. A group inside a repetition holds its last iteration: `(a|b)+` on `"ab"`
leaves group 1 at `1..2`.

`Regex` also answers questions about its own groups: `groupCount()`, `groupName(index)` and
`groupIndex(name)`.

## Replacement

In the replacement text of `replace` and `replaceAll`:

| Write | Inserts |
| --- | --- |
| `$n` or `${n}` | Group `n`. Digits are read greedily, so `$12` is group 12; use `${1}0` for group 1 followed by `0` |
| `${name}` | The named group. Braces hold `[A-Za-z0-9_]+`: all digits is a number, anything else is a name |
| `$$` | A literal `$` |

A group that did not take part, or does not exist, inserts nothing. Any other `$` (`$x`, `${}`,
`${a b}`, a trailing `$`) is a literal `$`. A name needs braces: `$name` is the text `$name`, not
the group.

```ignis
let date: Regex = Regex::new("(?<year>\\d{4})-(\\d\\d)").unwrap();
let swapped: String = date.replaceAll("on 2024-05", "$2/${year}"); // "on 05/2024"
```

`replaceAll` and `split` use the matches `findAll` finds, so an empty match counts: replacing `x*`
with `-` in `"abc"` gives `"-a-b-c-"`. `split` leaves an empty first or last piece when a match
touches the start or end, and returns the whole text when there is no match.

```ignis
let comma: Regex = Regex::new("\\s*,\\s*").unwrap();
let parts: Vector<String> = comma.split("a , b,c"); // "a", "b", "c"
```

## Syntax

| Syntax | Meaning |
| --- | --- |
| `x` | The scalar `x`, any UTF-8 scalar |
| `.` | Any scalar except `\n` |
| `\n` `\t` `\r` `\f` `\v` | Control scalars |
| `\.` `\*` `\\` … | Any escaped ASCII punctuation is itself |
| `[abc]` `[a-z]` `[^…]` | Classes, ranges and negation; escapes work inside |
| `\d` `\w` `\s`, `\D` `\W` `\S` | `[0-9]`, `[0-9A-Za-z_]`, `[ \t\n\r\f\v]` and their negations (ASCII) |
| `^` `$` | Start and end of the whole text, not of a line |
| `\b` `\B` | ASCII word boundary and its negation |
| `(x)` `(?:x)` `(?<name>x)` | Capturing, non-capturing and named groups |
| `x\|y` | Alternation, leftmost-first |
| `x*` `x+` `x?` | Zero or more, one or more, zero or one |
| `x{n}` `x{n,}` `x{n,m}` | Counted repetition |
| `x*?` `x+?` `x??` `x{n,m}?` | Lazy forms, preferring fewer repetitions |
| `(?i)` `(?i:x)` | ASCII case-insensitive for the rest of the enclosing group, or inside `x` |

Group names match `[A-Za-z_][A-Za-z0-9_]*` and must be unique. Capturing groups are numbered from 1
in the order of their `(`.

The parser is strict where a pattern would otherwise be ambiguous:

- `{` always starts a counted repetition, so a literal brace is `\{`. Behind an atom, a `{` that
  is not `{n}`, `{n,}` or `{n,m}` is `invalid repetition`; a `{` with no atom before it is
  `nothing to repeat`, like any other quantifier. A lone `}` or `]` is a literal.
- A quantifier after a quantifier (`a**`, `a+*`, `a{2}{3}`), or after `^`, `$`, `\b` or `\B`, is
  `nothing to repeat`. The `?` of a lazy form is part of its quantifier.
- `[]` and `[^]` are `empty class`. A `]` inside a class is written `\]`. A `-` first or last in a
  class is a literal.

## Errors

A bad pattern gives a `RegexError` with `message()`, `toString()` and `column()`. The column is
1-based and counted in Unicode scalars, pointing at the part that is wrong.

The messages are: `unclosed group` (at its `(`), `unmatched )`, `nothing to repeat`,
`invalid range z-a` (at its `-`; when an operand is a class the message is `invalid range` with no
characters, as in `[\d-z]`), `unclosed class` (at its `[`), `empty class`,
`trailing backslash`, `unknown escape \q`, `invalid repetition`, `invalid repetition range`,
`repetition count above 1000`, `invalid group name`, `duplicate group name x`,
`unknown group flag`, `unsupported: backreference` and `unsupported: lookaround`.

## Limits

- A count in `{n}`, `{n,}` or `{n,m}` is at most `REGEX_MAX_REPETITION` (1000).
- Counted repetition copies its body, so a compiled program holds at most `REGEX_MAX_PROGRAM`
  (100000) instructions. A pattern such as `(a{1000}){1000}` is `pattern too large` at column 1.

## Performance

Matching is linear in the text times the program, whatever the pattern. When every match starts
with the same literal scalars (`foo(bar|baz)` starts with `foo`), the search jumps to the next
occurrence of that prefix. A pattern that starts with `^`, a class, an alternation, a repetition
that may be empty, or a case-insensitive letter has no such prefix. Prefer `isMatch` when you only
need a yes or no.

## Not supported

- **Backreferences** (`\1`) and **lookaround** (`(?=`, `(?!`, `(?<=`, `(?<!`) are rejected, because
  they cannot be matched in linear time.
- **`\d`, `\w`, `\s`, `\b` and `(?i)` are ASCII-only.** `\d` does not match other Unicode digits,
  and `(?i)` folds only `A-Z` and `a-z`, so `(?i)straße` matches `"STRAßE"` but not `"STRASSE"`.
  Unicode classes such as `\p{...}` do not exist.
- **Other flags** (`(?m)`, `(?s)`, `(?x)`) are `unknown group flag`. `^` and `$` always mean the
  whole text.
