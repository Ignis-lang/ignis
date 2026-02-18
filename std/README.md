# Ignis Standard Library (`std`)

This directory contains the built-in modules shipped with the Ignis toolchain.

The design goal is to stay small, explicit and predictable.

## Module Map

| Module | Import | Description |
|--------|--------|-------------|
| `std::io` | `Io` | Print to stdout/stderr; structured I/O error types |
| `std::string` | `String` | Owned heap-backed string with byte-level operations |
| `std::math` | `Math` | `f64` math wrappers (libm) |
| `std::number` | `Number`, `Float` | Overloaded `abs` and rounding helpers |
| `std::types` | `Type` | Runtime type ID constants |
| `std::option` | `Option` | `Option<S>` — `SOME(S)` / `NONE` |
| `std::result` | `Result` | `Result<T, E>` — `OK(T)` / `ERROR(E)` |
| `std::memory` | `Memory` | Allocation, reallocation, copy/move primitives |
| `std::vector` | `Vector` | Growable contiguous array |
| `std::ptr` | `Pointer` | Raw pointer wrapper with method-style API |
| `std::rc` | `Rc`, `Weak` | Reference-counted shared ownership |
| `std::path` | `Path` | `PathBuf` — owned POSIX path manipulation |
| `std::ffi` | `FFI` | `CString` — owned NUL-terminated C string for FFI |
| `std::fs` | `Fs` | Filesystem operations (RAII `File`, `ReadDir`, convenience functions) |
| `std::libc` | `LibC`, `CType` | Raw C/POSIX bindings and type aliases |

Auto-loaded (imported implicitly): `string`, `number`, `vector`, `types`, `option`, `result`.

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
Ignis runtime (C)
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
  let mut values: Vector<i32> = Vector::init<i32>();
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
- Submodules: `Layout` (size/alignment descriptors), `Align` (alignment utilities).

### `std::vector`

- `Vector<T>` — growable contiguous storage. Implements `Drop` and `Clone`.
- Growth: geometric (`capacity *= 2`, minimum 1).
- Higher-order: `forEach`, `forEachMut`, `filter`, `any`, `all`, `findIndex`, `count`, `reduce`, `map`, `flatMap`.

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
| Shared ownership | `std::rc` |
| Filesystem I/O | `std::fs` |
| Path manipulation | `std::path` |
| C string interop | `std::ffi` |
| Raw allocation / byte ops | `std::memory` |
| C/POSIX syscalls | `std::libc` |
| Raw pointer helpers | `std::ptr` |

## Stability Expectations

Ignis is evolving. Standard library APIs are intended to remain small and clear,
but signatures and behavior can still change between compiler versions.
