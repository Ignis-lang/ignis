# Networking and HTTP: design

Status: proposal. Nothing in this document is implemented yet.

This document plans networking for the standard library, from raw TCP up to an Express-style web
framework, and the one language feature that has to land before it: thread-local storage. It
records the decisions already taken, the order of work, and the questions still open. The code in
it is illustrative: it shows the shape of an API, not a final signature.

## Decisions

| Topic | Decision |
| --- | --- |
| Panics | A panic aborts the process, always. Recoverable failures are `Result` and `Option`; a server that panics on bad input has a bug. |
| Handler errors | A framework handler that returns an error `Result` produces a `500` response. |
| Layering | Three layers, each built only on the one below: TCP, HTTP, framework. |
| HTTP core | Sans-IO: the protocol is a state machine fed bytes, with no socket access. A thin driver does the I/O. |
| First concurrency model | One thread per connection, from a thread pool. |
| Event-driven I/O | Deferred. The choice between stackful coroutines and stackless futures stays open; this plan keeps both viable. |
| Thread-local storage | Lands first, in its own pull request, because of the two-step rule (see `BOOTSTRAP.md`). |

## Roadmap

Each step is one or more pull requests. A step depends only on the steps above it.

| Step | Delivers | Kind | Depends on |
| --- | --- | --- | --- |
| 0 | Thread-local storage | Language | - |
| 1 | `std::net`: addresses, `TcpListener`, `TcpStream` | std | - |
| 2 | Channel and thread pool in `std::sync` | std | - |
| 3 | `std::http` core (sans-IO) and its blocking driver | std | 1 |
| 4 | Framework: router, middleware, `next` | std or separate package | 2, 3 |
| 5 | Readiness reactor (`epoll`) | std | 0, 1 |
| 6 | Event-driven model: decision and runtime | Language and/or std | 0, 5 |

Step 0 has no dependency of its own, but it gates step 5 and 6 behind a compiler promotion, so it
starts first. Steps 1 and 2 can run in parallel with it.

## Step 0: thread-local storage

### Why it comes first

Any scheduler, executor or reactor needs to find "the current one" without threading it through
every call. Thread-local storage is that mechanism. It is a compiler feature, so `std` can use it
only after the official stage0 understands it. Landing it early means the promotion has happened by
the time step 5 needs it.

### Proposed surface

An attribute on a `static mut` field, the only mutable global storage Ignis has today:

```ignis
record Scheduler {
  @threadLocal
  static mut CURRENT: *mut Scheduler;
}
```

Each thread sees its own `CURRENT`. Reads and writes look like any other `static mut` access.

### Rules

| Rule | Reason |
| --- | --- |
| Only on `static mut` fields | An immutable `static` has no storage; it folds into its uses. |
| Initializer is a constant expression, or absent (zero) | Same rule as `static mut` today (A0230, A0239). It maps directly to C `_Thread_local`, which requires a constant initializer. |
| The type must not implement `Drop` | Nothing runs at thread exit, so a destructor would never run. |
| Taking `&` or `&mut` of a thread-local is an error | A reference would dangle once the owning thread exits, and `&T` has no way to tell the `Send` check where it came from. Reads and writes by value still work. A later version can lift the rule without breaking code. |

### Code generation

| Backend | Emission |
| --- | --- |
| C | `static _Thread_local T name = <init>;` |
| QBE | `thread` data linkage. To verify: the QBE version the toolchain pins supports it. |

### Tests

- Codegen unit test: the global carries `_Thread_local`.
- E2E fixture: two threads write different values to the same thread-local and each reads back its own.
- Error fixtures: `@threadLocal` on a non-`mut` static, on an instance field, on a type with `Drop`,
  with a non-constant initializer, and `&` / `&mut` of a thread-local.
- G6 and G7 baselines for the new fixtures.

### Open questions

1. Does `@threadLocal` also apply to a future top-level `static mut`, if one is added?

## Layer 1: `std::net`

Raw TCP with the conveniences every user needs. Load balancers, proxies and non-HTTP protocols
(Redis, databases, custom binary protocols) build on this layer directly.

### Shape

```ignis
import Net from "std::net";

function main(): Result<i32, Io::IoError> {
  let listener: Net::TcpListener = Net::TcpListener::bind("127.0.0.1:8080")!;

  let greeting: String = String::create("hello\n");

  while (true) {
    let mut stream: Net::TcpStream = listener.accept()!;
    stream.writeAll(&greeting)!;
    // `stream` closes its socket when dropped.
  }
}
```

### Types

| Type | Role |
| --- | --- |
| `IpAddress` | Enum with `V4` and `V6` payloads. Parsing from and formatting to text. |
| `SocketAddress` | An `IpAddress` and a port. |
| `TcpListener` | `bind`, `accept` (the peer address is `peerAddress` on the stream), `localAddress`, `setNonBlocking`. Closes on drop. |
| `TcpStream` | `connect`, `connectTimeout`, `read`, `write`, `writeAll`, `shutdown`, `peerAddress`, `setNoDelay`, `setReadTimeout`, `setWriteTimeout`, `setNonBlocking`. Closes on drop. |
| `UdpSocket` | Out of the first slice. |

Every fallible operation returns `Result<_, Io::IoError>`. Timeouts take `Time::Duration`.

### Non-blocking support from day one

The first users will be blocking, but `setNonBlocking` and the `Io::ErrorKind::WOULD_BLOCK` error
ship in the first slice. Step 5 builds on exactly that, so layer 1 does not change shape when the
reactor arrives.

### Pitfalls the implementation has to handle

| Pitfall | Handling |
| --- | --- |
| Writing to a socket the peer closed raises `SIGPIPE`, which kills the process by default | Send with `MSG_NOSIGNAL` on Linux; `SO_NOSIGPIPE` on macOS. Report `EPIPE` as an error instead. |
| `EINTR` on `accept`, `read`, `write`, `connect` | Retry inside the call, except where a timeout has expired. |
| `accept` returns a socket that inherits nothing useful, and leaks into child processes | Use `accept4` with `SOCK_CLOEXEC` on Linux; set `FD_CLOEXEC` elsewhere. |
| A partial `write` | `writeAll` loops until every byte is written or an error occurs. |
| Name resolution (`getaddrinfo`) blocks and has its own error codes | First slice accepts numeric addresses only. A blocking `resolve` comes later, with its own error type. |
| `Io::ErrorKind` has no network variants | Adding `CONNECTION_REFUSED`, `CONNECTION_RESET`, `ADDRESS_IN_USE`, `BROKEN_PIPE`, `NOT_CONNECTED` changes an exported enum: an exhaustive `match` in user code stops compiling. Decide whether to add them or keep them under `OTHER` with `raw`. |

### Platforms

Linux first. Socket constants and structure layouts differ on macOS; they go behind
`@configFlag(@platform(...))` like the existing `errno` bindings in `std/io/error.ign`.

## Step 2: channel and thread pool

`Mutex` and `Condvar` exist; a channel and a fixed-size thread pool are built on them, in Ignis,
with no compiler change. The pool takes `@sendable` closures, the same way `Thread::spawn` does.

## Layer 2: `std::http`

The equivalent of Node's `http` module: requests, responses, headers, bodies. Anything that speaks
HTTP but is not a web application (a proxy, a client, a test harness) uses this layer.

### Sans-IO core

The parser never reads a socket. It receives bytes and returns what they mean:

```ignis
let mut parser: Http::RequestParser = Http::RequestParser::new(limits);

while (true) {
  let count: u64 = stream.read(&mut buffer)!;

  match (parser.feed(buffer.slice(0, count))) {
    Http::ParseEvent::NEED_MORE => continue,
    Http::ParseEvent::HEAD(head) => ...,
    Http::ParseEvent::BODY_CHUNK(bytes) => ...,
    Http::ParseEvent::DONE => break,
    Http::ParseEvent::MALFORMED(reason) => ...,
  }
}
```

The serializer works the same way in the other direction: it turns a response head and body into
bytes and leaves writing them to the caller.

Why: the protocol (chunked encoding, keep-alive, limits, malformed input) is written once and works
under any I/O model. Only the driver changes if the concurrency model changes. The parser is also
testable without a network: feeding a request one byte at a time is a plain unit test.

### Driver

A thin blocking driver on top: `Http::Server` accepts connections, runs the parser, and calls a
handler with a request and a response. This is the API most users of layer 2 see.

### Limits

The parser enforces maximum sizes for the request line, each header, the header count and the
body, and reports a violation as `MALFORMED`. Without them, a client can make the server allocate
without bound.

### First slice

HTTP/1.1 server side, keep-alive, `Content-Length` and chunked bodies. Out of scope: HTTP/2,
WebSockets, TLS (Transport Layer Security), compression, a client.

## Layer 3: framework

Minimally opinionated and extensible, in the spirit of Express. Routes and middleware receive the
request, the response and the next function.

### The `next` signature

In Express, `next` is a closure the middleware calls and then keeps using `req` and `res`. In Ignis,
a closure that captures `&mut` of the request and the response would hold both borrows for as long
as it exists, and the borrow checker would reject any use of them in the middleware. So `next`
receives them back as parameters:

```ignis
function logger(
  request: &mut Http::Request,
  response: &mut Http::Response,
  next: (&mut Http::Request, &mut Http::Response) -> Result<void, AppError>,
): Result<void, AppError> {
  next(request, response)!;
  // `request` and `response` are usable again here.
  Io::println(`${request.method()} ${request.path()} ${response.status()}`);
  return Result::OK(());
}
```

An error that reaches the router becomes a `500` response.

### Open questions

1. Lives in `std`, or in a separate package once packages exist?
2. Error type: one framework error type, or generic over the application's error?

## Event-driven I/O (open)

Steps 5 and 6. Both candidate models need the same foundation; the decision between them is
deferred.

### Shared foundation

| Piece | Needed by both because |
| --- | --- |
| Thread-local storage (step 0) | The current scheduler or executor is per thread. |
| Non-blocking sockets (layer 1) | Readiness-based I/O never blocks the thread. |
| Reactor (step 5) | Maps a file descriptor to a wake callback, `() -> void`. In the stackful model it resumes a coroutine; in the stackless model it wakes a task. |
| Sans-IO HTTP core (layer 2) | Only the driver depends on the I/O model. |

### Candidates

| | Stackful coroutines | Stackless futures (`async`/`await`) |
| --- | --- | --- |
| Language change | None | Function-to-state-machine transform in the compiler |
| User code | Looks blocking; layers 1-3 reused as they are | Every I/O function changes signature; a second I/O API next to the blocking one |
| Compiler risk | Low | High: locals live across `await` interact with the borrow checker, drop schedules (G7) and self-references |
| Cost per task | One stack per coroutine (`mmap` with a guard page) | Only the state the function keeps |
| Portability | Context switch in assembly per architecture; Ignis `asm` supports x86-64 only today | Portable C |
| Two-step rule | Not affected | `std` can use `async` only after promotion |

### Constraints if stackful is chosen

- Coroutines stay on the thread that created them (one scheduler per thread) in the first version.
  A coroutine that migrates between threads reads a different thread-local after resuming, and C
  compilers may have cached a thread-local address from before the switch.
- A blocking FFI call inside a coroutine stalls every coroutine on that thread.
- A deep recursion hits the guard page and crashes; the stack size has to be configurable.

## Out of scope

- TLS (Transport Layer Security). It needs a cryptography dependency; it gets its own design.
- HTTP/2, WebSockets, UDP, DNS beyond a blocking resolver.
- Windows.

## Next step

Step 0: thread-local storage, as its own pull request.
