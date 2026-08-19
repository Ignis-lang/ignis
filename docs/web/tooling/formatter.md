---
title: Formatter
description: Canonical layout, the four settings that control it, and the cases where the formatter refuses to guess.
section: tooling
order: 2
status: stable
---

`ignis fmt` rewrites source to a canonical layout. It parses the file, formats it, and reparses the
result to confirm the tree did not change — a formatter that alters meaning is worse than no
formatter, so it fails rather than write something it cannot verify.

```bash
ignis fmt src/main.ign
ignis fmt --check src/main.ign
ignis fmt --emit diff src/main.ign
```

## Settings

| Key | Type | Range | Default |
| --- | --- | --- | --- |
| `indent_width` | integer | 1–8 | `2` |
| `line_width` | integer | 40–160 | `100` |
| `use_tabs` | boolean | — | `false` |
| `sort_imports` | boolean | — | `false` |

They resolve in this order: built-in defaults, then `[formatter]` in `ignis.toml`, then
`ignisfmt.toml` or an explicit `--config`, then CLI flags. An unknown key is a hard error, not a
warning — a typo in a config file should not silently do nothing.

With `use_tabs`, indentation is emitted as one tab per level while `indent_width` still decides the
logical width used for wrapping decisions.

## What it normalizes

- Spacing around `:`, `=`, `+`, `-` and commas.
- Generic brackets stay tight: `identity<T>`, never `identity < T >`.
- Empty `namespace`, `record`, `enum`, `trait` and `extern` blocks collapse to `{}` on one line.
- Trailing whitespace goes, and the file ends with exactly one newline.
- Trailing commas follow the layout: a single-line parameter list or record initializer drops the
  final comma, a multiline one adds it.
- A single pipe expression may stay inline when it fits; a chain of two or more `|>` always breaks
  across lines.

## What it preserves

Import order stays as written unless `sort_imports` is on, and even then sorting happens *within*
each existing group — blank lines and comments between imports mark deliberate groups and the
formatter does not merge across them.

Consecutive imports from the same path are merged into one list, unless a blank line separates
them. A discard import (`import _ from "..."`) is never merged into a named list.

A single blank line between declarations survives, because it is how you group things.

## Where it stops

There is no general wrapping contract for every long expression yet. The formatter wraps what it
has rules for — callable signatures, record initializers, import lists, single pipes — and when it
cannot prove a rewrite is safe it fails instead of guessing. Expect that on unusual long
expressions, and treat it as a missing rule rather than a bug in your code.
