# Ignis ABI contract

This is the normative contract every backend implements: the C backend, the QBE backend and any later one. Where it disagrees with a backend, the backend is wrong. `docs/ABI_CURRENT.md` describes how the C backend spells the contract in C.

The contract has two layers:

1. **The platform C ABI.** Every symbol that crosses an object boundary follows the platform's C calling convention and data layout (System V on the hosted 64-bit targets). Ignis does not define its own.
2. **The Ignis layer.** Each Ignis type has one canonical C representation, defined below. Its layout and its calling behavior are whatever the platform C ABI gives that representation.

So two backends agree on a value exactly when they agree on its canonical representation.

## Status

| Part | State |
| --- | --- |
| Data layout | Specified here, computed by `ignis/abi/`, enforced on every C build |
| Platform constants (`O_RDONLY`, `ENOENT`, `TIOCGWINSZ`, ...) | Specified for `x86_64` Linux in `std/libc/platform.ign`, checked against the C headers in CI; other targets still read the headers |
| Symbol names | Not yet: Ignis symbols carry per-build definition ids |
| Calling convention for Ignis-internal calls | Not yet: fixed arrays, closures and drop glue differ between backends |
| Runtime ownership across objects | Not yet: every object carries its own std and runtime |

## Data layout

Sizes and alignments are in bytes. A pointer is 8 bytes, aligned to 8.

| Ignis type | Canonical C representation | Size, alignment |
| --- | --- | --- |
| `i8`, `u8`, `boolean` | `int8_t`, `uint8_t`, `uint8_t` | 1, 1 |
| `i16`, `u16` | `int16_t`, `uint16_t` | 2, 2 |
| `i32`, `u32`, `f32`, `char`, atoms | `int32_t`, `uint32_t`, `float`, `uint32_t`, `uint32_t` | 4, 4 |
| `i64`, `u64`, `f64` | `int64_t`, `uint64_t`, `double` | 8, 8 |
| `str`, pointers, references, raw functions | a pointer | 8, 8 |
| `T[N]` | `T[N]`, stored inline | `N` × size of `T`, alignment of `T` |
| record | `struct` of its fields in declared order | C struct rules |
| enum | `struct { u32 tag; union { struct { payload... } variant_<tag>; ... } payload; }` | C struct rules |
| slice `T[]` | `struct { T* data; u64 len; }` | 16, 8 |
| `Range<T>` | `struct { T field_0; T field_1; }` | 2 × size of `T`, alignment of `T` |
| closure `(A) -> R` | `struct { call; drop_fn; env; }`, three pointers | 24, 8 |

The rules that complete the table:

- **Drop state.** A record or enum that implements `Drop` ends with one `uint8_t` drop-state byte, placed after its last member and before the trailing padding.
- **Enum payloads.** The tag is always a `u32` at offset 0. A variant without payload, and a `void` payload field, take no storage. An enum with no payload at all is only its tag.
- **`@packed`** removes every padding byte between and after fields; the record's alignment becomes 1 unless `@aligned(N)` raises it.
- **`@aligned(N)`** on a record or a field raises its alignment to `N` and never lowers it. The record's size rounds up to its final alignment.
- **Empty records** have size 0 and alignment 1, the GNU C empty struct.
- **C-layout records** (`@cLayout`) store a function-typed field as a raw function pointer.
- **Extern records** (declared in an `extern` block) are laid out by the C side that owns them, and are outside this contract.

### How the layout is enforced

`ignis/abi/` computes every layout once. The QBE backend reads its sizes and offsets from it. The C backend writes the canonical representation and, after the type definitions of every unit, a `_Static_assert` for the size, the alignment and every member offset that `ignis/abi/` computed. If the shared layout and the C compiler ever disagree, the C build fails at that line, naming the struct and the member:

```c
_Static_assert(__builtin_offsetof(struct Shape_12, payload.variant_1.field_1) == 16, "ignis abi layout: Shape_12.payload.variant_1.field_1");
```

Because the compiler itself and the standard library are built by the C backend, every build of them checks the layout of every type they use.

## Platform constants

The C constants the standard library uses (open flags, file modes, error numbers, signals, memory-mapping flags, clocks, terminal requests) live in one module, `std/libc/platform.ign`, as the `Platform` namespace. Every other part of std reads them from there.

| Target | Where the values come from |
| --- | --- |
| `x86_64` Linux | Written in Ignis. Any backend can use them; no C header is involved. |
| every other target | Read from the C header macro of the same name, which only the C backend can do. |

`scripts/check_platform_constants.py` keeps the written values honest. It compiles one C program against the headers `std/manifest.toml` lists, prints every constant, and fails on any value that differs. It also fails when the written table, the header-reading table and the extern block that declares the macros do not list the same constants in the same order, so a constant cannot be added to one target only. CI runs it on every pull request.

Values that are Ignis's own, not the platform's, are plain Ignis constants: the runtime type ids in `std::types` and `PI`, `E` and `TAU` in `std::math`.

`errno` itself is read through the platform's accessor function (`__errno_location` on Linux, `__error` on macOS), a symbol every backend can call. The layout of `struct stat` is described in Ignis by `RawStat` in `std/fs/sys/unix.ign`, for `x86_64` Linux only.
