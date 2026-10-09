# Ignis Standard Library (`std`)

This directory contains the built-in modules shipped with the Ignis toolchain.

The design goal is to stay small, explicit and predictable.

## Module Map

| Module | Import | Description |
|--------|--------|-------------|
| `std::compile` | `Compile` | Compile-time-only directive API (`error`, `warning`, `note`); never links into a binary |
| `std::libc` | `LibC`, `CType` | Raw C/POSIX bindings and type aliases |
| `std::io` | `Io` | Print to stdout/stderr; structured I/O error types |
| `std::math` | `Math` | `f64` math wrappers (libm) |
| `std::string` | `String` | Owned heap-backed string with byte-level operations |
| `std::char` | `Char` | `char` helpers: scalar value, ASCII and script classification |
| `std::number` | `Number`, `Float` | Overloaded `abs` and rounding helpers |
| `std::types` | `Type` | Runtime type ID constants |
| `std::option` | `Option` | `Option<S>` — `SOME(S)` / `NONE` |
| `std::result` | `Result` | `Result<T, E>` — `OK(T)` / `ERROR(E)` |
| `std::memory` | `Memory` | Allocation, reallocation, copy/move primitives |
| `std::hash` | `Hash`, `Eq`, `Hasher` | Canonical hashing and equality traits |
| `std::collections` | `HashMap`, `HashSet`, `BitSet` | Hash map, hash set and dense `u32` bit set |
| `std::vector` | `Vector` | Growable contiguous array |
| `std::ptr` | `Pointer` | Raw pointer wrapper with method-style API |
| `std::terminal` | `Terminal` | Terminal styling, screen and cursor control |
| `std::cli` | `Cli` | Bounded command-line argument parser for Ignis programs |
| `std::path` | `Path` | `PathBuf` — owned POSIX path manipulation |
| `std::sort` | `Sort` | Comparator-based sorting entry points |
| `std::rc` | `Rc`, `Weak` | Reference-counted shared ownership |
| `std::ffi` | `FFI` | `CString` — owned NUL-terminated C string for FFI |
| `std::fs` | `Fs` | Filesystem operations (RAII `File`, `ReadDir`, convenience functions) |
| `std::env` | `Env` | Owned access to process environment variables |
| `std::process` | `Process` | Process arguments, exit status and wait status helpers |
| `std::process_runtime` | — | Startup runtime the generated entrypoint calls; internal, never imported by user code |
| `std::test` | `Test` | Assertions and snapshot helpers for `@test` functions |
| `std::time` | `Time` | Clock reads and duration helpers over second/nanosecond parts |
| `std::serializer` | `Serialize`, `Serializer` | Serialization traits and writer |
| `std::json` | `Json` | JSON value model, parsing and writing |
| `std::toml` | `Toml` | TOML document model, parsing and writing |
| `std::format` | `Display` | `Display` trait for user-defined textual representations |
| `std::text` | `Text` | UTF-8 byte-offset ↔ line/column position conversion (`Text::LineIndex`) |

Auto-loaded (imported implicitly): `string`, `number`, `vector`, `types`, `option`,
`result`, `format`, plus the internal `process_runtime`, which the generated C entrypoint
requires in every program.

## Layering

```text
User code
   │
   ├── std::fs / std::path / std::ffi
   │
   ├── std::vector / std::string / std::number / std::io / std::rc
   │
   ├── std::memory / std::ptr / std::types / std::option / std::result
   │
   ├── std::libc
   │
Ignis runtime (implemented in Ignis)
   │
System libc / OS
```

`std::memory` and `std::libc` are the foundation. Most other modules are thin
wrappers built on top. `std::fs`, `std::path`, and `std::ffi` sit at the top
layer and depend on multiple lower modules.

## Quick Usage

```ignis
import Io from "std::io";
import String from "std::string";
import Vector from "std::vector";

function main(): i32 {
  let mut values: Vector<i32> = Vector::new<i32>();
  values.push(10);
  values.push(20);

  Io::println(String::create("len=").concat(values.length()));

  // Vector implements Drop; freed automatically at end of scope
  return 0;
}
```

## Module Notes

### `std::io`

- `print`, `println`, `eprint`, `eprintln` — write to stdout/stderr.
- `println` and `eprintln` are overloaded for `String` (consumed) and `str` (no allocation).
- `Io::ErrorKind` and `Io::IoError` provide structured I/O errors with errno mapping.

### `std::string`

- `String` is an owned, heap-backed byte string with `Drop` and `Clone`.
- `String::create` is overloaded for `str` and all numeric types.
- Byte-level higher-order methods: `forEachByte`, `findByte`, `findLastByte`, `trimWhere`, `split`.
- `concat` is overloaded for `&String`, `str`, and numeric types.
- `toBytes` / `toChars` convert to `Vector<u8>` / `Vector<char>`.

### `std::char`

- `Char` helpers around the `char` primitive: `scalarOf`, ASCII classification and digit
  values, and per-script alphabetic checks.

### `std::hash`

- `Hash` and `Eq` traits that `@hash<T>` / `@eq<T>` and `std::collections` dispatch through.
- `Hasher` — the stateful hasher passed to `Hash`.

### `std::collections`

- `HashMap<K, V>` — deterministic hash table with open addressing, linear probing, and tombstones.
- `HashSet<T>` — thin unique-value set built on top of `HashMap`.
- `BitSet` — dense bit set over `u32` indexes.
- Re-exports `Hash` and `Eq` from `std::hash`.

### `std::terminal`

- `Terminal` — semantic styling (`Terminal::Style`, `colorText`), screen clearing and cursor
  positioning (`Terminal::Screen`, `Terminal::Cursor`), with terminal capability checks.

### `std::cli`

- `Cli::Command` — bounded parser for declared flags, valued options, positionals and the
  `--` terminator. See `docs/CLI.md` for the supported surface and its deliberate limits.

### `std::math`

- Wraps double-precision C math functions (`f64`).
- Constants: `PI`, `E`, `TAU`.
- Functions: `sin`, `cos`, `tan`, `asin`, `acos`, `atan`, `atan2`, `exp`, `log`, `log10`, `pow`, `sqrt`, `floor`, `ceil`, `round`, `trunc`, `fabs`.

### `std::number`

- `Number::abs` — overloaded for all signed integer and float types.
- `Float::toFixed`, `Float::round`, `Float::floor`, `Float::ceil` — overloaded for `f32` and `f64`.

### `std::types`

- `Type::TypeId` — compile-time constants mapping primitive types to numeric IDs.

### `std::option`

- `Option<S>` with `SOME(S)` / `NONE`. Marked `@lang(try)`.
- Methods: `isSome`, `isNone`, `unwrap`, `unwrapOr`, `unwrapOrElse`.
- Higher-order: `filter`, `map`, `andThen`, `orElse`, `inspect`.

### `std::result`

- `Result<T, E>` with `OK(T)` / `ERROR(E)`. Marked `@lang(try)`.
- Methods: `isOk`, `isError`, `unwrap`, `unwrapErr`, `unwrapOr`, `unwrapOrElse`.
- Higher-order: `map`, `mapErr`, `andThen`, `orElse`, `inspect`, `inspectErr`.

### `std::memory`

- Allocation: `allocate<T>`, `allocateVector<T>`, `allocateZeroed<T>`, `allocateZeroedVector<T>`.
- Reallocation: `reallocate<T>`, `reallocateVector<T>`.
- Deallocation: `free<T>`.
- Data transfer: `copy<T>`, `move<T>`, `copyBytes`, `moveBytes`.
- Submodules: `Layout` (size/alignment descriptors), `Align` (alignment utilities), `ArenaAllocator` (grouped-lifetime arena allocation).

### `std::vector`

- `Vector<T>` — growable contiguous storage. Implements `Drop` and `Clone`.
- Growth: geometric (`capacity *= 2`, minimum 1).
- Higher-order: `forEach`, `forEachMut`, `filter`, `any`, `all`, `findIndex`, `count`, `reduce`, `map`, `flatMap`.

### `std::sort`

- `Sort` — comparator-based sorting over vectors; `stableStrings` for owned `String` values.

### `std::env`

- `Env` — owned access to process environment variables; returns owned `String` values
  instead of borrowed C pointers.

### `std::process`

- `Process` — process arguments, exit status and wait status helpers; backed by the
  `std::process_runtime` startup module the generated entrypoint initializes.

### `std::test`

- `Test` — assertions and snapshot helpers for functions marked `@test`, run by `ignis test`.

### `std::time`

- `Time` — clock reads and duration helpers over second/nanosecond parts.

### `std::serializer`

- `Serialize` trait and `Serializer` writer used by the `std::json` and `std::toml` front ends.

### `std::json`

- `Json` namespace plus `JsonValue`, `JsonValueKind` and `JsonObjectEntry` — parse, inspect
  and write JSON.

### `std::toml`

- `Toml` namespace plus `TomlDocument`, `TomlTable`, `TomlArray`, `TomlValue` and
  `TomlValueKind` — parse, inspect and write TOML.

### `std::format`

- `Display` trait for user-defined textual representations.

### `std::text`

- `Text::LineIndex` — converts between UTF-8 byte offsets and LSP-style line/column
  positions in both directions.

### `std::collections`

- `HashMap<K, V>` — deterministic hash table with open addressing, linear probing, and tombstones.
- `HashSet<T>` — thin unique-value set built on top of `HashMap`.
- Traits: `Hash`, `Eq`.

### `std::ptr`

- `Pointer<T>` — thin wrapper around `*mut T`.
- Null checks, address conversion, pointer arithmetic, read/write helpers.
- No ownership, no bounds checks, no lifetime tracking.

### `std::rc`

- `Rc<T>` — reference-counted shared ownership of heap-allocated `T`.
- `Weak<T>` — non-owning observer with `upgrade()` to check liveness.
- Both implement `Drop` and `Clone`. Move-by-default; use `.clone()` to share.

### `std::path`

- `PathBuf` — owned, mutable filesystem path backed by `String`.
- Constructors: `new()`, `create(str)`, `fromString(&String)`.
- Query: `isEmpty`, `isAbsolute`, `asStr`, `toString`, `fileName`, `extension`, `parent`.
- Mutation: `push`, `pop`, `join`.
- POSIX-oriented (separator is `/`).

### `std::ffi`

- `FFI::CString` — owned, NUL-terminated C string for FFI interop.
- `FFI::NulError` — error for inputs with interior NUL bytes.
- Construction validates no interior NULs. Buffer freed on drop.

### `std::fs`

- `Fs::File` — RAII file descriptor wrapper with `open`, `create`, `read`, `write`, `metadata`.
- `Fs::ReadDir` — owned directory iterator with automatic `closedir` on drop.
- Convenience functions: `Fs::read`, `Fs::readToString`, `Fs::write`, `Fs::writeString`, `Fs::exists`, `Fs::metadata`, `Fs::createDir`, `Fs::createDirAll`, `Fs::removeFile`, `Fs::removeDir`.
- All fallible operations return `Result<T, Io::IoError>`.
- Platform: Linux/glibc (POSIX). Low-level wrappers in `Fs::Sys::Unix`.

### `std::libc`

- Raw C/POSIX bindings grouped by domain: `LibC::Allocator`, `LibC::MemoryOperations`, `LibC::Memory`, `LibC::String`, `LibC::File`, `LibC::Stdio`, `LibC::Errno`, `LibC::Process`, `LibC::Signals`, `LibC::Wait`, `LibC::Conversion`, `LibC::Math`, `LibC::Character`, `LibC::Time`.
- `CType` namespace provides LP64 type aliases (`CSize`, `CSsize`, `CConstStr`, etc.).
- Constants from C headers (open flags, errno values, signal numbers, etc.).

## Memory Ownership and Safety

Ignis std gives direct access to raw memory and pointers. That power is useful,
but misuse can easily produce undefined behavior.

Key rules:

- Free only memory that was allocated by matching allocators.
- Do not use pointers after `free`.
- Do not read uninitialized memory.
- Use `move` instead of `copy` for overlapping regions.
- Keep pointer arithmetic aligned with element boundaries.

For higher-level code, prefer `Vector<T>`, `String`, `Rc<T>`, and `Fs::File`,
and only drop to `Memory` / `Pointer` / `LibC` when necessary.

## Choosing the Right Module

| Need | Module |
|------|--------|
| Print to stdout/stderr | `std::io` + `std::string` |
| Dynamic arrays | `std::vector` |
| Lookup by key / membership | `std::collections` |
| Shared ownership | `std::rc` |
| Filesystem I/O | `std::fs` |
| Path manipulation | `std::path` |
| Environment variables | `std::env` |
| Process control | `std::process` |
| Time and durations | `std::time` |
| JSON / TOML data | `std::json` / `std::toml` |
| Command-line parsing | `std::cli` |
| Terminal output styling | `std::terminal` |
| C string interop | `std::ffi` |
| Raw allocation / byte ops | `std::memory` |
| C/POSIX syscalls | `std::libc` |
| Raw pointer helpers | `std::ptr` |
| Native test assertions | `std::test` |

## Stability Expectations

Ignis is evolving. Standard library APIs are intended to remain small and clear,
but signatures and behavior can still change between compiler versions.
