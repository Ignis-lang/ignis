# QBE backend

The QBE target lowers the self-hosted compiler's LIR directly to QBE IL. The C target remains the default. This first slice is hosted-only; it still uses the configured `cc` to assemble and the existing linker to link.

## Build and inspect

```sh
nix develop
ignis build --target qbe program.ign
```

Project configuration selects the same backend with `[build] target = "qbe"`. The pipeline writes `.ssa` IL, runs `qbe` to produce `.s` assembly, assembles it into an object, then links it. An unsupported construct is a compilation error with the prefix `not supported by the qbe target yet: `; failed emission must not produce a runnable artifact.

## Representation and ABI

| Concern | Decision |
| --- | --- |
| Addressable locals | Stack slots preserve address-taking and mutation without a frontend dominance/phi pass. |
| LIR temporaries | QBE temporaries; QBE accepts non-SSA input and performs SSA conversion. |
| Narrow integers | Load/store and extension operations preserve width and signedness. |
| Pointers | QBE `l` values on supported 64-bit targets. |
| Calls | QBE ABI types describe scalar and aggregate arguments and results. Aggregate IL values are addresses, not language-level pointer parameters. |
| Records | Field classes and padding preserve layout; unaligned packed members use QBE opaque types for the SysV memory class. Zero-sized and over-aligned (>16 byte) aggregate values fail explicitly. |
| Extern calls | This integer-only slice uses QBE's variadic convention for typed C wrappers; it also matches fixed integer prototypes and initializes the SysV vector-register count. |
| Hosted roots | Keep the entry point, user exports, test harness roots and runtime initialization, then walk dependencies. Unused C-only std entrypoints do not constrain integer programs. |
| Divergence | Panic and `never` calls end the block with `hlt`; dead tail instructions must not enter QBE's SSA conversion. |

QBE's [IL reference](https://c9x.me/compile/doc/il.html) documents non-SSA input and aggregate calls. Its [AMD64 ABI reference](https://c9x.me/compile/doc/abi.html) describes register and memory classification. Do not add a second hidden return-pointer convention on top of QBE's aggregate ABI.

## Differential fixture lane

Build the lane runner using the installed self-hosted compiler, then pass the newly built compiler as its first argument:

```sh
(cd scripts/qbe && ignis build -O 0 -o ../../build/qbe-lane/runner)
build/qbe-lane/runner build/selfhost/ignis
```

`test_cases/qbe_supported.txt` lists the supported fixtures. `test_cases/qbe_skip.tsv` lists every other fixture with its first missing feature or unsupported fixture mode. The runner requires every current `test_cases/e2e/ok` fixture to occur in exactly one inventory, rejects duplicates and escaping paths, and compares exit code, stdout, and stderr against a fresh C-target build. It reports both lane pass counts and the skip count, and fails if the supported lane is empty.

To deliberately regenerate the inventories after adding support:

```sh
build/qbe-lane/runner build/selfhost/ignis --inventory
```

Inventory generation accepts only explicit backend unsupported diagnostics as feature skips. A QBE parser error, assembly/link failure, compiler crash, or runtime mismatch fails the lane instead of becoming a skip. Diagnostic-only and existing expected-failure fixtures are recorded separately because this runner compares executable behavior. Review inventory changes before committing them; normal lane runs never update snapshots or inventories.

Construct tests execute emitted IL through QBE, assembly and linking; the aggregate tests link a real C helper and check both call directions, including packed and byte records. Run them with QBE and `cc` on `PATH`:

```sh
ignis test --filter '::codegen::qbe::tests::qbe'
```

Runner regression tests:

```sh
(cd scripts/qbe && ignis test --filter '::qbeLane')
```

## Verification of this slice

Measured on hosted AMD64 with QBE 1.2 and the installed Ignis 0.4.0 compiler:

| Check | Result |
| --- | --- |
| Executable QBE construct tests | 28/28 passed, including bidirectional native C aggregate ABI. |
| Differential lane, including inventory regeneration | QBE 225/225; C reference 225/225; 715 explicit skips; 940 fixtures covered. |
| Full `ignis test` in an isolated source copy | 3366 passed, 0 failed, 1 skipped. |
| CLI / driver / tool suites | 78/78, 9/9, 4/4 passed. |
| Runner regression tests | 5/5 passed; imported fixture helpers also passed in the isolated wrapper (39/39 combined). |
| `ignis fmt --check` over explicit files in `ignis/`, `std/`, `example/` | Passed. |

The formatter accepts files, not directory operands. The recursive check used:

```sh
find ignis std example -type f -name '*.ign' -print0 | xargs -0 ignis fmt --check
```

Three cold `-O 2` builds of [`example/qbe_benchmark.ign`](../example/qbe_benchmark.ign), an integer-loop, pointer and aggregate-call benchmark, took 1.426/1.467/1.485 seconds with C and 1.378/1.246/1.209 seconds with QBE (medians: 1.467 and 1.246 seconds). Both binaries returned 206 with empty stdout/stderr. This measures the whole small-program build, not compiler self-hosting speed: the compiler itself still needs unsupported features.

Independent bounded public-CLI probes passed for default/config/override selection, packed and large/nested record ABI, user exports, strings/globals and rejection before toolchain. They used the new compiler; installed compiler provenance was not verified against the baseline commit, so no historical executable-equivalence claim is made.

## Follow-ups

- Closures and their allocation/runtime conventions.
- Drop glue and ownership cleanup.
- Payload enums and their aggregate layout.
- Inline assembly.
- Freestanding runtime handlers and assembler/linker-only toolchains.
- Generic edge cases and expansion of the supported fixture inventory.
- Floating-point instructions, raw closure/function-pointer adapters, aggregate globals and runtime bridge calls.
- Zero-sized and over-aligned aggregate ABI/storage.
- Volatile memory operations; ordinary QBE loads/stores must not silently replace them.

## Review order

1. QBE type/layout and symbol mapping.
2. Instruction, control-flow and call emission with construct tests.
3. CLI/configuration, artifacts, cache keys and toolchain failures.
4. Fixture inventories and C/QBE differential results.
5. Nix tooling, full-suite results, formatting and timing evidence.
