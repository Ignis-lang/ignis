---
title: HTTP
description: The std::http request parser, response serializer and blocking HTTP/1.1 server — framing rules, limits and how a handler works.
section: std
order: 4
status: stable
---

`std::http` speaks HTTP/1.1 on the server side. It has two layers:

| Layer | What it is | Use it when |
| --- | --- | --- |
| Sans-IO core | A request parser and a response serializer that never touch a socket | You build a proxy, a test harness, or your own server driver |
| `Http::Server` | A blocking driver that accepts connections and calls your handler | You just want to serve requests |

## A minimal server

A handler is a record that implements `Http::Handler`. Outside the `Http` namespace the trait is
reached by its path, `@implements(Http::Handler)`.

```ignis
import Http from "std::http";
import IpAddress, SocketAddress from "std::net";

@implements(Http::Handler)
record Greeter {
    public greeting: static str;

    handle(&self, request: &Http::Request, response: &mut Http::Response): void {
        if (!request.head.target.equals("/")) {
            response.setStatus(404);
            return;
        }

        response.setHeader("Content-Type", "text/plain");
        response.writeStr(self.greeting);
    }
}

function main(): i32 {
    let address: SocketAddress = SocketAddress::create(IpAddress::v4(127, 0, 0, 1), 8080);
    let Result::OK(server) = Http::Server::bind(&address, Http::ServerConfig::new()) else {
        return 1;
    };

    return server.run<Greeter>(Greeter { greeting: "hello" }).isOk() ? 0 : 1;
}
```

`Server::bind` takes an address and a `ServerConfig`. Port 0 picks a free port, and `localAddress()`
tells you which one.

## Request and Response

The handler receives a complete request and fills in a response.

| `Http::Request` | Description |
| --- | --- |
| `head` | The `RequestHead`: `method`, `target`, `version`, `headers` |
| `body` | The whole body as bytes, with chunked coding already removed |
| `target()` | The request target as received, such as `/search?q=1` |
| `header(name)` | The first header named `name`, ignoring ASCII case: `Option<str>` |
| `bodyText()` | A copy of the body as text, or `NONE` if it is not valid UTF-8 |

| `Http::Response` | Description |
| --- | --- |
| `Response::new()` | An empty 200 response with no headers |
| `setStatus(status)` | Sets the status; the reason phrase is the standard one |
| `setHeader(name, value)` | Replaces every header with that name |
| `addHeader(name, value)` | Appends a header and keeps any with the same name |
| `write(&bytes)` / `writeStr(text)` | Appends to the body |

The driver writes the response after `handle` returns. It adds `Content-Length` itself, and a
`Content-Length` or `Transfer-Encoding` you set is dropped. If the head cannot be serialized at
all — a status outside 100–999, a header value holding CR, LF or another control byte, or a header
name that is not a token, such as one with a space or a colon — the driver sends an empty 500
instead.

## ServerConfig

| Field | Default | Meaning |
| --- | --- | --- |
| `threads` | 8 | Threads accepting and serving connections; each serves one connection at a time. 0 counts as 1 |
| `limits` | `Limits::new()` | Parser limits applied to every request |
| `idleTimeout` | 30 seconds | How long a read may wait for the client. When it expires the connection closes without a response |
| `maxRequestsPerConnection` | 0 | Requests served on one connection before the server closes it. 0 means no limit |

## Threads and shared state

`Server::run` shares **one handler among all its threads**, so the handler must be `Send` and `Sync`
and `handle` takes `&self`. State that a handler changes has to sit behind a `Mutex` or an atomic.

`run` returns only when its workers stop. A worker leaves its loop only when `accept` fails with
`EBADF` or `EINVAL`; a failure it can recover from, such as `EMFILE` or `ENFILE`, is retried and
does not end the server. There is no way to stop a running server yet, so in practice it blocks for
as long as the process lives.

If your state cannot be shared, call `Http::acceptConnections` on threads of your own. Each thread
then gets its own handler, and the listener is shared (for example through an `Arc`). `std::web`
builds one router per thread this way. `Http::serveConnection` serves a single accepted connection
on the calling thread, which is handy in tests.

## What the server does for you

- A request reaches the handler once its whole body is in, bounded by `Limits.maxBody`.
  `Expect: 100-continue` is not answered; the client sends the body after its own wait.
- A `HEAD` request gets the head with the length the body would have, and no body.
- The connection stays open unless the request does not keep it alive (`RequestHead.keepAlive`),
  the response lists `Connection: close`, or `maxRequestsPerConnection` is reached. When it closes,
  the response carries `Connection: close`.
- A malformed request gets an empty response with the parser's suggested status and
  `Connection: close`, and then the connection ends.
- Before closing, the server shuts its writing side and discards what the client still sends, for
  up to 2 seconds or 1 048 576 bytes, so the close does not reset the connection before the client
  reads the response.

## Using the parser directly

`Http::RequestParser` is sans-IO. `feed(&bytes)` or `feedStr(text)` copies bytes in, and `next()`
reports what they mean.

| `ParseEvent` | Meaning |
| --- | --- |
| `NEED_MORE` | The bytes fed so far are not enough to report anything else |
| `HEAD(RequestHead)` | A request head arrived. Reported once per request |
| `BODY_CHUNK(Vector<u8>)` | A piece of the body. Zero or more per request |
| `DONE` | The request is complete |
| `MALFORMED(ParseError)` | The request is invalid. This is terminal |

After `DONE` the parser starts on the next request of the same connection. Bytes already fed past
the end of a request belong to the next one, so keep-alive and pipelining need nothing special.
Events own their payloads, so a `RequestHead` or a chunk stays valid after more bytes are fed.

`MALFORMED` is terminal: every later `next()` reports the same error, because the rest of the
connection cannot be framed. The `ParseError` carries the status to answer with (400, 413, 414, 431,
501 or 505) before closing. Malformed input never panics.

### Message syntax

- Lines end with CRLF. A bare LF is accepted too (RFC 9112 section 2.2). A CR anywhere else is
  malformed. Empty lines before a request line are skipped.
- The request line is `method SP target SP HTTP/1.x`, with exactly one space between the parts.
  The method is a case-sensitive token; anything other than the nine standard methods becomes
  `Method::OTHER`. The target is kept as received and must be visible ASCII.
- The version is `HTTP/<digit>.<digit>`, eight characters exactly. A version spelled that way but
  other than `HTTP/1.0` or `HTTP/1.1` is `UNSUPPORTED_VERSION` (505); anything else, such as
  `HTTP/2` or `HTTP/1`, is not a version at all and makes the request line
  `INVALID_REQUEST_LINE`.
- A header line is `name: value`. Whitespace before the colon, obsolete line folding and control
  bytes in the value are `INVALID_HEADER`. Names keep their casing, and lookups ignore ASCII case.

### Body framing (RFC 9112 section 6.3)

| Head | Body |
| --- | --- |
| `Transfer-Encoding: chunked` | Chunked, reported without the chunk framing |
| Any other `Transfer-Encoding` | `UNSUPPORTED_TRANSFER_CODING` (501) |
| `Transfer-Encoding` and `Content-Length` together | `CONFLICTING_LENGTH` (400), to prevent request smuggling |
| `Content-Length: n` | Exactly `n` bytes |
| `Content-Length` values that disagree | `CONFLICTING_LENGTH` (400) |
| A `Content-Length` that is not a count | `INVALID_CONTENT_LENGTH` (400) |
| Neither | No body |

Repeated `Content-Length` fields, or a comma list in one, are accepted when every value is the same.
Chunk extensions are ignored. Trailer fields after the last chunk are checked like headers, counted
against `maxHeaders`, and discarded.

### Limits

| Limit | Default | Violation |
| --- | --- | --- |
| `maxRequestLine` | 8192 | `REQUEST_LINE_TOO_LONG` (414) |
| `maxHeaderLine` | 8192 | `HEADER_TOO_LONG` (431); `INVALID_CHUNK` for a chunk-size line |
| `maxHeaders` | 100 | `TOO_MANY_HEADERS` (431) |
| `maxBody` | 1048576 | `BODY_TOO_LARGE` (413) |

Line lengths exclude the terminator, and a length exactly at the limit is accepted. A line is
rejected as soon as it grows past its limit, and a body as soon as its declared size passes
`maxBody`, so a client cannot make the parser buffer without bound.

## Headers

`Http::Headers` is an ordered list of fields shared by requests and responses.

| Method | Description |
| --- | --- |
| `Headers::new()` | An empty list |
| `get(name)` | The first value for `name`, ignoring ASCII case: `Option<str>` |
| `getAll(name)` | Every value for `name`: `Vector<String>` |
| `contains(name)` | Whether a field with that name exists |
| `hasToken(name, token)` | Whether the comma-separated value of `name` lists `token` |
| `add(name, value)` | Appends a field |
| `set(name, value)` | Replaces every field with that name by one |
| `remove(name)` | Removes the fields with that name and returns how many |
| `length()`, `isEmpty()` | How many fields |
| `nameAt(i)`, `valueAt(i)`, `fieldAt(i)` | Fields by position |

## Media types

`Http::MediaType::parse(text)` reads a `Content-Type` value (RFC 9110 section 8.3.1) and returns
`Option<MediaType>`. Malformed input gives `NONE`. The type, subtype and parameter names are
lowercased, parameter values keep their case, and a quoted value loses its quotes and escapes.
`essence()` is `type/subtype`, and `parameter("charset")` finds a parameter ignoring case.

## Writing responses by hand

When you use the parser without `Http::Server`, build a `ResponseHead` and serialize it yourself.

| Function | Appends |
| --- | --- |
| `Http::writeHead` | `HTTP/1.1 <status> <reason>`, every header, and the blank line |
| `Http::writeResponse` | A head with `Content-Length`, then the body. It rejects a head that already carries `Content-Length` or `Transfer-Encoding` |
| `Http::writeBody` | Body bytes unchanged |
| `Http::writeChunk` | One chunk of a chunked body |
| `Http::writeLastChunk` | The chunk that ends a chunked body |

`writeResponse` writes `Content-Length` itself, so it refuses a head that already carries
`Content-Length` or `Transfer-Encoding`, with `FRAMING_HEADER_PRESENT`. For a chunked body, add
`Transfer-Encoding: chunked` to the head, write the head with `writeHead`, call `writeChunk` for
each piece, and finish with `writeLastChunk`. A header name that is not a token, or a header value
or reason phrase containing CR or LF, could split the response, so it is a `SerializeError` and
nothing is appended. Byte inputs are `&Vector<u8>`.

```ignis
import Http from "std::http";

function answer(request: str, out: &mut Vector<u8>): i32 {
    let mut parser: Http::RequestParser = Http::RequestParser::new(Http::Limits::new());
    parser.feedStr(request);

    let Http::ParseEvent::HEAD(head) = parser.next() else {
        return 1;
    };

    let mut response: Http::ResponseHead = Http::ResponseHead::create(200);
    response.headers.add("Content-Type", "text/plain");
    let body: Vector<u8> = head.target.toBytes();

    return Http::writeResponse(&response, &body, out).isOk() ? 0 : 1;
}
```
