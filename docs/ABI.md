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
| Symbol names | Specified below, computed by `ignis/abi/symbols.ign`; checked across programs in CI |
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

## Symbol names

A definition's symbol is derived from what it is, so the same function has the same symbol in every program that compiles it. That is what lets one object call into another. Every backend takes its symbols from `ignis/abi/symbols.ign`.

A symbol is `_I` followed by a nested name, `N` parts `E`. Each part is one segment, its byte length followed by the identifier:

| Definition | Symbol |
| --- | --- |
| `close` in `std/libc/io.ign` | `_INL3std4libc2ioE5closeE` |
| `File::open` in the same module | `_INL3std4libc2ioE4File4openE` |
| method `raw` of record `Handle` there | `_INL3std4libc2ioE6Handle3rawE` |
| static field `ARGC` of `ProcessSlots` in `std/process/runtime/mod.ign` | `_INL3std7process7runtime3modE12ProcessSlots4ARGCE` |
| `Vector<u8>::push` | `_INL3std6vector3modE6VectorI2u8E4pushE` |
| the first closure in `PathBuf::extension` | `_INNL3std4path3modE4Path7PathBuf9extensionECt0_E` |

The parts, in order:

- **Module.** `L` segments `E`: for a std module `std` followed by its path below the std root, for a project module the package name followed by its path below the directory of the entry module. Only the `.ign` extension is dropped, so `a.ign` and `a/mod.ign` stay apart, and the closing `E` keeps the module apart from the written path: `util.ign` declaring `text::width` and `util/text.ign` declaring `width` get different symbols.
- **Written path.** The declaration's path inside its module, `Outer::Inner::Name`. A method or a static field is its owner's name followed by its own name.
- **Generic arguments.** `I` types `E` after the segment they instantiate.
- **Overload signature.** `F` parameter types `V` return type `E`, only for a function that shares its name with another one in the same scope.
- **Closure functions.** A closure's thunk and drop function are named after the function, variable or constant whose body holds the closure, then `Ct` or `Cd` and the closure's position in that body. Inside a generic function, each instantiation's copy adds that instantiation's type arguments.

Types inside a symbol are encoded as follows: a primitive is its name as a segment (`3i32`); `P`/`Q` a pointer or mutable pointer, `R`/`M` a reference or mutable reference, `S` a slice, `A<count>_` a fixed array, `T...E` a tuple, `F...V...E` a closure type, `W...V...E` a raw function, `Z`/`Y` a range or inclusive range, a record or enum its nested name, and `K` a generic parameter. Every production starts with a digit or with a letter that no other production in the same position uses, so a symbol reads back one way only and two definitions never share one. A segment holding a byte that cannot appear in a C identifier, or starting with a digit, is written `X`, the length of its escaped text, `_`, then that text: each invalid byte becomes `_` and two hex digits and a literal `_` is doubled (`a-b` is `X5_a_2db`).

**Length.** QBE accepts identifiers of at most 79 characters, and every backend uses the same symbols, so a longer symbol keeps its first 62 characters followed by `_` and the 64-bit FNV-1a hash of the whole symbol in sixteen lowercase hex digits (offset basis `14695981039346656037`, prime `1099511628211`). A full symbol always ends in `E` and a shortened one in a hex digit, so the two forms never collide; two shortened symbols are told apart by their hashes, and a collision would surface as a duplicate definition when the object is compiled.

Some names stay as they are: `extern` declarations and `@externName` keep the C name the other side defines, and the generated C `main` stays `main`. A definition with no module or written path to derive a name from keeps a per-program name, `name_<id>`; none of the compiler's own emitted symbols does.

`scripts/check_symbol_stability.py` is the check. It compiles two programs that share functions, with every definition id shifted in the second, and fails when a symbol of the first program is missing from the second.

Symbols are still local to their object in a program build: every function is `static`. `ignis build-std` already exports every function of the std archive under these names. Exporting a program's own symbols, so another object can link against them, is the next step.
