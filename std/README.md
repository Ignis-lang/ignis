# Ignis Standard Library (`std`)

This directory contains the built-in modules shipped with the Ignis toolchain.
They provide the low-level runtime surface (`memory`, `libc`, `ptr`) and a small
set of high-level building blocks (`string`, `io`, `vector`, `number`, `math`,
`types`, `option`, `result`).

The design goal is to stay small, explicit and predictable.

## Module Map

- `std::io` - runtime-backed stdout/stderr helpers.
- `std::string` - string primitives and conversion helpers.
- `std::math` - `f64` math wrappers (libm-backed).
- `std::number` - numeric utility helpers (`abs`, rounding wrappers).
- `std::types` - runtime type IDs.
- `std::option` - `Option<S>` (`SOME` / `NONE`).
- `std::result` - `Result<T, E>` (`OK` / `ERROR`).
- `std::memory` - allocation, reallocation, copy/move primitives.
- `std::vector` - growable contiguous array.
- `std::ptr` - raw pointer wrapper utilities.
- `std::libc` - C/POSIX bindings and constants.

## Layering

At a high level, the std stack looks like this:

```text
User code
   |
   +--> std::vector / std::string / std::number / std::io
   |
   +--> std::memory / std::ptr / std::types / std::option / std::result
   |
   +--> std::libc
   |
Ignis runtime (C)
   |
System libc / OS
```

`std::memory` and `std::libc` are the foundation. Most other modules are thin
wrappers built on top.

## Quick Usage

```ignis
import Io from "std::io";
import String from "std::string";
import Vector from "std::vector";

function main(): i32 {
  let mut values: Vector<i32> = Vector::init<i32>();
  values.push(10);
  values.push(20);

  Io::println(String::concat("len=", String::toString(values.length())));

  values.free();
  return 0;
}
```

## Module Notes

### `std::io`

- Provides `print`, `println`, `eprint`, `eprintln`.
- APIs are string-only and intentionally minimal.
- Newlines are explicit (`println` appends `"\n"`, `print` does not).

### `std::string`

- Provides length, compare, concat, substring, search, case conversion.
- Exposes overloaded `toString(...)` conversions for primitives.
- Behavior follows the runtime implementation for edge cases.

### `std::math`

- Wraps double-precision C math functions (`f64`).
- Includes constants `PI`, `E`, and `TAU`.
- Semantics follow platform libm behavior.

### `std::number`

- Adds convenience helpers on top of `std::math`.
- Contains overloaded `abs` and floating-point rounding helpers.

### `std::types`

- Includes runtime type ID constants under `Type::TypeId`.

### `std::option`

- Defines `Option<S>` with variants `SOME(S)` and `NONE`.
- Provides helpers like `isSome`, `isNone`, `unwrap`, and `unwrapOr`.

### `std::result`

- Defines `Result<T, E>` with variants `OK(T)` and `ERROR(E)`.
- Provides helpers like `isOk`, `isError`, `unwrap`, `unwrapErr`, and `unwrapOr`.

### `std::memory`

- Low-level allocation and byte movement API:
  - `allocate*`, `reallocate*`, `free`
  - `copy*` and `move*`
- Generic APIs compute sizes using `sizeOf<T>()`.
- Includes helper submodules:
  - `Layout`: size/alignment descriptors
  - `Align`: alignment utilities

### `std::vector`

- Growable contiguous storage of `T`.
- Uses geometric growth (`capacity *= 2`, minimum 1).
- Requires explicit `free()` to release backing memory.

### `std::ptr`

- Method-style convenience wrapper for raw pointers.
- Includes null checks, arithmetic helpers, and read/write helpers.
- Does not provide ownership, bounds checks, or lifetime tracking.

### `std::libc`

- C/POSIX bindings grouped under `LibC` and `CType`.
- Useful for low-level system integration and runtime-adjacent code.
- Assumes LP64 type sizes in `CType` aliases.

## Memory Ownership and Safety

Ignis std gives direct access to raw memory and pointers. That power is useful,
but misuse can easily produce undefined behavior.

Key rules:

- Free only memory that was allocated by matching allocators.
- Do not use pointers after `free`.
- Do not read uninitialized memory.
- Use `move` instead of `copy` for overlapping regions.
- Keep pointer arithmetic aligned with element boundaries.

For higher-level code, prefer `Vector<T>` and only drop to `Memory` / `Pointer`
when necessary.

## Choosing the Right Module

- Need formatted output? Start with `std::io` + `std::string`.
- Need dynamic arrays? Use `std::vector`.
- Need raw allocation or byte-level control? Use `std::memory`.
- Need C/POSIX interop? Use `std::libc`.
- Need pointer method helpers in low-level code? Use `std::ptr`.

## Stability Expectations

Ignis is evolving. Standard library APIs are intended to remain small and clear,
but signatures and behavior can still change between compiler versions.
