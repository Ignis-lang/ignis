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
| `std::sync` | `Ordering`, `AtomicBool`, `AtomicU32`, `AtomicU64`, `AtomicI64`, `AtomicPtr`, `Mutex`, `MutexGuard`, `Condvar`, `Arc`, `Channel`, `Sender`, `Receiver`, `ThreadPool` | Atomic values, locks, condition variables, shared ownership, channels and a thread pool |
| `std::thread` | `Thread`, `JoinHandle<T>` | Spawn, join and detach POSIX threads |
| `std::net` | `IpAddress`, `Ipv4Address`, `Ipv6Address`, `SocketAddress`, `TcpListener`, `TcpStream`, `Shutdown` | Numeric IP addresses and blocking or non-blocking TCP (x86_64 Linux only) |
| `std::http` | `Http` | HTTP/1.1 server side: sans-IO request parser and response serializer, blocking `Http::Server` driver |

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

### `std::sync`

- `Ordering` — `RELAXED`, `ACQUIRE`, `RELEASE`, `ACQ_REL`, `SEQ_CST`.
- `AtomicBool`, `AtomicU32`, `AtomicU64`, `AtomicI64`, `AtomicPtr<T>` — `new`, `load`,
  `store`, `swap`, `compareExchange`; the integer types add `fetchAdd`, `fetchSub`,
  `fetchAnd`, `fetchOr`, `fetchXor`. Every method takes `&self`. Atomics are not Copy:
  share one between threads through `Arc`, or lend it to a `@noescape` task.
- `compareExchange` is strong and returns `Result::OK(previous)` when it wrote,
  `Result::ERROR(observed)` when it did not.
- An ordering the operation cannot use (a `load` with `RELEASE`, a `store` with
  `ACQUIRE`, a `compareExchange` failure ordering of `RELEASE` or `ACQ_REL`) panics.
- The allocator counters behind `Memory::stats` are atomic, so allocation is
  thread-safe.
- `Mutex<T: Send>` — `new`, `lock(&self): MutexGuard<T>` (blocks), `tryLock(&self):
  Option<MutexGuard<T>>`. The guard gives `read(&self): &T` and `get(&mut self): &mut T`
  and unlocks when it is dropped. No poisoning; locking twice from one thread deadlocks.
- `Condvar` — `wait(&self, guard)`, `notifyOne`, `notifyAll`, used with a `MutexGuard`.
- Every pthread return code is checked. A failed unlock, notification or destruction
  panics with a `std::sync:` message; it signals misuse, such as dropping a `Mutex`
  while a guard holds it or a `Condvar` while a thread waits on it.
- `Arc<T: Send & Sync>` — `new`, `clone`, `get(&self): &T`, `strongCount`; the value is
  dropped when the last clone is.
- `Mutex` and `Condvar` hold a `pthread_mutex_t` (40 bytes) and `pthread_cond_t`
  (48 bytes) inline, the sizes on Linux x86-64 with glibc. A `Mutex` or `Condvar` must
  not move while a guard or a waiter uses it; keep it in place, typically inside an
  `Arc`.
- `Channel::new<T: Send>(): Channel<T>` — an unbounded many-producer, many-consumer
  channel; move its `sender` and `receiver` fields out. `Sender<T>` and `Receiver<T>`
  are cloneable and Send; `T` need not be Sync.
- `Sender<T>::send(&self, value): Result<void, T>` — never blocks; gives the value back
  as `ERROR` once every receiver is gone.
- `Receiver<T>::recv(&self): Option<T>` — blocks; `NONE` once every sender is gone and
  the queue is empty. `tryRecv(&self): Option<T>` does not block. Values come out in
  send order, each to exactly one receiver. Values left when the last end is dropped
  are dropped with it.
- `ThreadPool::new(size: u64)` — starts `size` worker threads; panics when `size` is 0.
  `execute(&self, @sendable task: () -> void)` queues a job for the first idle worker.
  Dropping the pool lets the workers finish every job queued before the drop, then
  joins them.

### `std::net`

- `IpAddress` — enum `V4(Ipv4Address)` / `V6(Ipv6Address)`; `parse(text: str)`,
  `v4(a, b, c, d)`, `toString`, `equals`, `isLoopback`, `isUnspecified`. IPv6 text is
  formatted per RFC 5952. Numeric addresses only; there is no name resolution.
- `SocketAddress` — `create(ip, port)`, `parse("a.b.c.d:port" | "[ipv6]:port")`, `ip`,
  `port`, `toString`.
- `TcpListener` — `bind(&address)`, `accept(&self): Result<TcpStream, _>`,
  `localAddress`, `setNonBlocking`. Closes on drop.
- `TcpStream` — `connect`, `connectTimeout(&address, Time::Duration)`,
  `read(buf: *mut void, count)`, `readInto(&mut Vector<u8>, limit)` (appends),
  `write(buf: *void, count)`, `writeAll`, `writeBytes(&Vector<u8>)` / `writeAllBytes`
  (`u8[]` overloads wait for slice length/pointer access in the compiler), `shutdown`,
  `peerAddress`, `localAddress`, `setNoDelay`, `setReadTimeout` / `setWriteTimeout`
  (`Option<Time::Duration>`), `setNonBlocking`. Closes on drop.
- Every fallible call returns `Result<_, Io::IoError>`. Sends use `MSG_NOSIGNAL`, so a
  closed peer is `BROKEN_PIPE` or `CONNECTION_RESET`, never `SIGPIPE`. Sockets are
  `SOCK_CLOEXEC`; `EINTR` is retried. An expired read or write timeout is `WOULD_BLOCK`.
- x86_64 Linux only: the socket calls go through `syscall(2)` because the emitted C
  does not declare the socket functions. See the `std::net` module docs.

### `std::http`

- `Http::RequestParser::new(Http::Limits)` — `feed(&Vector<u8>)` or `feedStr(str)` copy
  bytes in; `next()` returns a `Http::ParseEvent`: `NEED_MORE`, `HEAD(RequestHead)` once
  per request, zero or more `BODY_CHUNK(Vector<u8>)`, then `DONE`. Bytes past a request
  belong to the next one (keep-alive, pipelining). `MALFORMED(ParseError)` is terminal and
  carries a suggested status (400, 413, 414, 431, 501, 505). It never reads a socket.
- `Http::RequestHead` — `method` (`Http::Method`, `OTHER(String)` for extension methods),
  `target`, `version` (`HTTP_1_0` / `HTTP_1_1`), `headers`; `keepAlive()`.
- `Http::Headers` — ordered fields, original casing kept, case-insensitive `get`,
  `getAll`, `contains`, `hasToken`, `set`, `remove`; `add`, `length`, `nameAt`, `valueAt`.
- Bodies: `Content-Length` or `Transfer-Encoding: chunked` (extensions ignored, trailers
  checked and discarded). Both together, conflicting lengths and other codings are rejected.
- `Http::Limits::new()` — request line 8 KiB, header line 8 KiB, 100 headers, 1 MiB body;
  each is a public field.
- `Http::writeHead`, `writeResponse` (adds `Content-Length`), `writeChunk`,
  `writeLastChunk` append to a `&mut Vector<u8>`. CR or LF in a header or reason is a
  `Http::SerializeError` and nothing is written.
- `Http::Server::bind(&SocketAddress, Http::ServerConfig)`, `localAddress()`,
  `run<H: Http::Handler & Send & Sync>(self, handler)` — `config.threads` threads (default
  8) each accept and serve connections; `run` returns only when every thread's `accept`
  failed, so a running server blocks for good. Handlers implement `trait Http::Handler`
  (`handle(&self, &Http::Request, &mut Http::Response)`), declared as `@implements(Http::Handler)`.
- `Http::serveConnection<H: Http::Handler>(&mut TcpStream, &H, &Http::ServerConfig)` —
  serves one connection: keep-alive, idle read timeout (default 30 s, closes quietly),
  optional `maxRequestsPerConnection`. Malformed requests get the parser's status with
  `Connection: close`; an unserializable response becomes an empty 500; `HEAD` gets no
  body. Bodies are buffered (up to `Limits.maxBody`) and responses carry `Content-Length`.
- `Http::Request` — `head`, `body`; `target()`, `header(name)`, `bodyText()` (`NONE` when
  not UTF-8). `Http::Response::new()` — 200, no fields, empty body; `setStatus`,
  `setHeader`, `addHeader`, `write(&Vector<u8>)`, `writeStr(str)`.

### `std::thread`

- `Thread::spawn<T: Send>(@sendable task: () -> T): JoinHandle<T>` — runs `task` on a new
  POSIX thread. The compiler checks what the closure captures (`@sendable`).
- `JoinHandle<T>::join(self): T` — waits for the thread and returns the task's
  result. It consumes the handle, so joining twice is a use-after-move compile error.
  Dropping an unjoined handle detaches the thread.
- A task with nothing to report returns `void`: `Thread::spawn<void>` gives a
  `JoinHandle<void>` whose `join` only waits.
- `Thread::currentId(): u64` — the calling thread's `pthread_t`.
- A panic in any thread ends the process with exit code 101.
- `std::io` output is not audited for concurrent use yet.

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
| Atomic counters and flags | `std::sync` |
| Threads | `std::thread` |
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
