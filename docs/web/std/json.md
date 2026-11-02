---
title: JSON
description: Parsing JSON into a JsonValue tree, building values in code, and writing compact or pretty text with std::json.
section: std
order: 3
status: stable
---

`std::json` has two halves. One reads and writes whole documents as a `JsonValue` tree. The other
writes typed values straight to JSON through the shared `std::serializer` contracts.

| You need | Use |
| --- | --- |
| Read JSON text whose shape you do not know | `Json::parse` |
| Build a document in code | The `JsonValue` constructors |
| Turn a tree into text | `Json::toJsonString` or `Json::toJsonStringPretty` |
| Write a scalar or a string as a JSON document | `Json::writeRoot` |
| Write a `Serialize` record as JSON text | `Json::stringify` |

## Parsing

`Json::parse(source: &String)` returns `Result<JsonValue, String>`. The error is a message with a
line and column, for example `trailing characters at line 1 column 5`. There is no line tracking:
the line is always `1` and the column is a byte offset into that one line. Whitespace around the
document is allowed and anything after the value is an error.

A document may nest at most 127 containers. Opening the 128th gives a `recursion limit exceeded`
error instead of exhausting the stack; that same limit is `serde_json`'s default.

## The JsonValue tree

A `JsonValue` has a `kind` and an `endOffset`. The `kind` is a `JsonValueKind`:

| Variant | Holds |
| --- | --- |
| `NULL` | Nothing |
| `BOOL(boolean)` | The value |
| `NUMBER(String)` | The number as source text, unparsed |
| `STRING(String)` | The decoded string |
| `ARRAY(Vector<JsonValue>)` | The elements, in order |
| `OBJECT(Vector<JsonObjectEntry>)` | `key` and `value` entries, in source order, duplicates included |

Numbers keep their source text, so a parsed float is written back exactly as it was read. For the
same reason there is no float constructor: `f64.toString()` uses C `%g` formatting (six significant
digits, `42.0` becomes `42`), which neither round-trips nor matches what `serde_json` writes.

Objects keep every member in order and do not merge duplicate keys. If you want "last one wins",
walk the entries in order yourself.

`endOffset` is the 0-based byte offset just after the value's own token in the source. Values built
in code have no source text, so their `endOffset` is `0`.

## Building values

| Constructor | Builds |
| --- | --- |
| `JsonValue::nullValue()` | `null` |
| `JsonValue::boolValue(value)` | `true` or `false` |
| `JsonValue::stringValue(value: str)` | A string, copying `value` |
| `JsonValue::fromString(value: String)` | A string that takes ownership of `value`; keeps interior NUL bytes, which a `str` cannot hold |
| `JsonValue::integerValue(value: i64)` | A number written in decimal |
| `JsonValue::unsignedValue(value: u64)` | A number written in decimal |
| `JsonValue::array(items)` | An array from a `Vector<JsonValue>` |
| `JsonValue::object(entries)` | An object from a `Vector<JsonObjectEntry>`; build entries with `JsonObjectEntry::create(key, value)` |

## Writing a tree

| Function | Result |
| --- | --- |
| `Json::toJsonString(&value)` | A fresh `String` of compact JSON, no whitespace between tokens |
| `Json::toJsonStringPretty(&value)` | A fresh `String` of pretty JSON |
| `Json::writeValue(&mut output, &value)` | Appends compact JSON to `output` |
| `Json::writeValuePretty(&mut output, &value)` | Appends pretty JSON to `output` |

Pretty output uses two-space indentation, one element or member per line, `": "` between a key and
its value, `[]` and `{}` for empty containers, and no trailing newline.

These writers match `serde_json::to_string` and `serde_json::to_string_pretty` byte for byte, string
escaping included: `\"`, `\\`, `\b`, `\f`, `\n`, `\r`, `\t`, other bytes below `0x20` as lowercase
`\u00xx`, and everything else (`/`, `0x7f`, non-ASCII UTF-8) unchanged.

## Writing typed values

`Json::writeRoot(&mut output, value)` appends a document to `output` and returns
`Result<boolean, Serializer::Error>`. It has one overload per scalar type: `boolean`, `char`, the
integer types, `f32`, `f64`, `str` and `&String`. A record that implements `Serialize` has no
overload here: `Json::stringify<T: Serialize>(value: &T)` returns the document as a fresh
`Result<String, Serializer::Error>` instead.

```ignis
import Json from "std::json";

function main(): i32 {
    let mut output: String = String::create("");
    Json::writeRoot(&mut output, true);

    return 0;
}
```

JSON allows scalar roots, always quotes field names and separates record entries with commas.
`writeRoot` and `stringify` use the shared serializer's escaping, which differs slightly from the
tree writers: it writes other control bytes as uppercase `\u00XX` and has no `\b` or `\f`. Use the
tree writers when you need `serde_json`-compatible text.
