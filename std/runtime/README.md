# Ignis Runtime

There is no C runtime any more. `std/runtime` holds one header and nothing
else; there is no `Makefile`, no `.c` file, and no `libignis_rt.a`. Every
runtime service the standard library used to reach through C is Ignis, in
`std/`, and links out of `libignis_std.a`.

## What is left

### `ignis_rt.h`

The base header both compilers include, always at `<std_path>/runtime/ignis_rt.h`
and not configurable anywhere. It carries the runtime type definitions — the integer aliases, `boolean`,
`ignis_atom_t`, `ignis_char_t`, `Pointer`, `IgnisString`, `null` and the
`IGNIS_TYPE_*_ID` constants — under the `IGNIS_RT_TYPES_H` guard, plus the
declaration of `ignis_runtime_init`.

Both compilers emit that same guarded block at the top of every translation
unit they produce, so a unit that also includes this header ends up with
exactly one definition of each name whichever it reaches first. The two copies
have to stay byte-identical:

- `RUNTIME_TYPE_PRELUDE` in `crates/ignis_codegen_c/src/emit.rs`
- `CodegenC::emitTypePrelude` in `ignis/codegen/mod.ign`

## Where the runtime went

| Service | Now lives in |
| --- | --- |
| Process allocator, arena | `std/memory/allocator.ign`, `std/memory/arena.ign` |
| Startup argument capture (`ignis_runtime_init`) | `std/process/runtime/mod.ign` |
| Reference counting | `std/rc/mod.ign` |
| `IgnisString` buffer, UTF-8, search, case, conversions | `std/string/mod.ign` |
| Number formatting | `std/string/mod.ign` over `std/libc` |
| Filesystem syscalls, `stat`, `dirent`, recursive removal | `std/fs/sys/unix.ign` over `std/libc` |
| Hashing | `std/hash/mod.ign` |
| Output helpers | `std/io/mod.ign` |
