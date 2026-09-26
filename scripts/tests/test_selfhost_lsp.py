#!/usr/bin/env python3
"""Drives `<compiler> lsp` over stdio the way an editor does.

Spawns the language server, speaks JSON-RPC with Content-Length framing, and
checks the lifecycle (initialize, shutdown, exit and its exit code), the error
responses, the diagnostics published for an open buffer (a type error
reported at the right UTF-16 range, then cleared once the buffer is fixed or
closed), the hovers answered for it, and go-to-definition, find-references
and the document outline over a two-file project.
The erroneous text only ever exists in the editor buffer, so the diagnostic
also proves analysis reads open documents instead of the disk.

The compiler comes from `--compiler PATH` or `$IGNIS_LSP_COMPILER`. Without
one, or when the path is not an executable file, every test is skipped. The
standard library root comes from `$IGNIS_STD_PATH`, defaulting to this
repository's `std/`. The host compiler (`ignis lsp`) answers the same script,
which makes it usable as a reference.

Usage:
  IGNIS_LSP_COMPILER=build/bootstrap/stage2/ignis python3 scripts/tests/test_selfhost_lsp.py
  python3 scripts/tests/test_selfhost_lsp.py --compiler build/bootstrap/stage1/ignis -v
"""

import json
import os
import queue
import shutil
import subprocess
import sys
import tempfile
import threading
import unittest
from pathlib import Path
from urllib.parse import unquote, urlparse

REPOSITORY_ROOT = Path(__file__).resolve().parent.parent.parent
MESSAGE_TIMEOUT_SECONDS = 120
MAX_MESSAGE_BYTES = 64 * 1024 * 1024

# The type error sits after a string holding a two-unit emoji and a two-byte
# letter on the same line, so its UTF-16 column (31) differs from its byte
# column (34). The host spans this error the same way, so the script also
# holds against `ignis lsp`.
BROKEN_SOURCE = (
  "function measure(label: str): i32 {\n"
  "  return 3;\n"
  "}\n"
  "\n"
  "function main(): i32 {\n"
  '  let label: str = "héllo 😀"; let count: i32 = label;\n'
  "  return count;\n"
  "}\n"
)
FIXED_SOURCE = BROKEN_SOURCE.replace("let count: i32 = label;", "let count: i32 = measure(label);")
EXPECTED_RANGE = {"start": {"line": 5, "character": 31}, "end": {"line": 5, "character": 54}}
EXPECTED_CODE = "A0045"
EXPECTED_MESSAGE = "Type mismatch: expected 'i32', found 'str'"

# The hover sources: `main.ign` uses a record, a documented function and a
# function imported from `util.ign`. `count` is declared after a string with
# a two-unit emoji and a two-byte letter, so its UTF-16 column differs from
# its byte column.
HOVER_UTIL_SOURCE = (
  "/// Doubles a value.\n"
  "export function double(value: i32): i32 {\n"
  "  return value * 2;\n"
  "}\n"
)
HOVER_SOURCE = (
  'import double from "./util";\n'
  "\n"
  "record Point {\n"
  "  /// Horizontal.\n"
  "  public x: i32;\n"
  "  public y: i32;\n"
  "}\n"
  "\n"
  "/// Adds two numbers.\n"
  "function add(left: i32, right: i32): i32 {\n"
  "  return left + right;\n"
  "}\n"
  "\n"
  "function main(): i32 {\n"
  '  let label: str = "héllo 😀"; let count: i32 = add(1, 2);\n'
  "  let point: Point = Point { x: count, y: 2 };\n"
  "\n"
  "  return point.x + double(count);\n"
  "}\n"
)
HOVER_COUNT = "main\n\n```ignis\nlet count: i32\n```\n\n*Defined in `main.ign`*"
HOVER_ADD = (
  "main\n\n```ignis\nfunction add(left: i32, right: i32): i32\n```\n\n---\n\n"
  "Adds two numbers.\n\n*Defined in `main.ign`*"
)
HOVER_FIELD = (
  "main\n\n```ignis\nx: i32\n```\n\n(field of `Point`)\n\n---\n\n"
  "Horizontal.\n\n*Defined in `main.ign`*"
)
HOVER_DOUBLE = (
  "util\n\n```ignis\nfunction double(value: i32): i32\n```\n\n---\n\n"
  "Doubles a value.\n\n*Defined in `util.ign`*"
)


# The navigation sources: `main.ign` declares a constant after a string with
# a two-unit emoji and a two-byte letter on the same line, a namespace, a
# record with fields and methods and an enum, and calls a function of
# `util.ign`, one of the standard library, a method and a field.
NAVIGATION_UTIL_SOURCE = HOVER_UTIL_SOURCE
NAVIGATION_SOURCE = (
  'import double from "./util";\n'
  'import Io from "std::io";\n'
  "\n"
  'const GREETING: str = "héllo 😀"; const SCALE: i32 = 3;\n'
  "\n"
  "namespace Shapes {\n"
  "  const LIMIT: i32 = 4;\n"
  "\n"
  "  function area(side: i32): i32 {\n"
  "    return side * side;\n"
  "  }\n"
  "}\n"
  "\n"
  "record Point {\n"
  "  public x: i32;\n"
  "  public y: i32;\n"
  "\n"
  "  public length(&self): i32 {\n"
  "    return self.x + self.y;\n"
  "  }\n"
  "\n"
  "  public static origin(): Point {\n"
  "    return Point { x: 0, y: 0 };\n"
  "  }\n"
  "}\n"
  "\n"
  "enum Mode {\n"
  "  FAST,\n"
  "  SLOW,\n"
  "}\n"
  "\n"
  "function main(): i32 {\n"
  '  let label: str = "héllo 😀"; let count: i32 = double(SCALE);\n'
  "  let point: Point = Point::origin();\n"
  "  Io::println(label);\n"
  "\n"
  "  return point.x + point.length() + double(count) + Shapes::area(Shapes::LIMIT);\n"
  "}\n"
)

# The outline of `NAVIGATION_SOURCE`: (name, SymbolKind, children).
NAVIGATION_OUTLINE = [
  ("GREETING", 14, []),
  ("SCALE", 14, []),
  ("Shapes", 3, [("LIMIT", 14, []), ("area", 12, [])]),
  ("Point", 23, [("x", 8, []), ("y", 8, []), ("length", 6, []), ("origin", 6, [])]),
  ("Mode", 10, [("FAST", 22, []), ("SLOW", 22, [])]),
  ("main", 12, []),
]


def position_of(text, needle, occurrence=1, delta=0):
  """The LSP position `delta` characters into the `occurrence`th `needle`."""
  offset = -1

  for _ in range(occurrence):
    offset = text.index(needle, offset + 1)

  offset += delta
  line_start = text.rfind("\n", 0, offset) + 1
  column = len(text[line_start:offset].encode("utf-16-le")) // 2
  return {"line": text.count("\n", 0, offset), "character": column}


def range_of(text, needle, occurrence=1, delta=0, length=None):
  """The LSP range of `length` characters (all of `needle` by default) from
  `delta` characters into the `occurrence`th `needle`."""
  if length is None:
    length = len(needle) - delta

  return {
    "start": position_of(text, needle, occurrence, delta),
    "end": position_of(text, needle, occurrence, delta + length),
  }


def span_of(text, first, last):
  """The LSP range from the first `first` to the end of the first `last`
  after it."""
  start = text.index(first)
  end = text.index(last, start) + len(last)
  return range_of(text[:end], first, length=end - start)


def text_in(text, lsp_range):
  """The characters of `text` an LSP range covers."""
  lines = text.split("\n")

  def offset(position):
    line = lines[position["line"]]
    units = 0
    column = 0

    while units < position["character"] and column < len(line):
      units += len(line[column].encode("utf-16-le")) // 2
      column += 1

    return sum(len(previous) + 1 for previous in lines[:position["line"]]) + column

  return text[offset(lsp_range["start"]):offset(lsp_range["end"])]


def outline_of(symbols):
  """The (name, kind, children) tree of a DocumentSymbol list."""
  return [(symbol["name"], symbol["kind"], outline_of(symbol.get("children", []))) for symbol in symbols]


def take_compiler_argument(argv):
  """Removes `--compiler PATH` from argv and returns PATH, or None."""
  if "--compiler" not in argv:
    return None

  index = argv.index("--compiler")

  if index + 1 >= len(argv):
    raise SystemExit("--compiler needs a path")

  path = argv[index + 1]
  del argv[index:index + 2]
  return path


COMPILER = take_compiler_argument(sys.argv) or os.environ.get("IGNIS_LSP_COMPILER")


def compiler_path():
  if not COMPILER:
    return None

  path = Path(COMPILER)

  if not path.is_file() or not os.access(path, os.X_OK):
    return None

  return path.resolve()


def frame(body):
  return b"Content-Length: " + str(len(body)).encode("ascii") + b"\r\n\r\n" + body


def read_frame(stream):
  """Reads one framed body from a binary stream, or None at end of input."""
  length = None

  while True:
    line = stream.readline()

    if not line:
      return None

    line = line.rstrip(b"\r\n")

    if not line:
      break

    name, _, value = line.partition(b":")

    if name.strip().lower() == b"content-length":
      length = int(value.strip())

  if length is None:
    raise AssertionError("a message arrived without Content-Length")

  body = stream.read(length)

  if len(body) != length:
    return None

  return body


class LanguageServer:
  """One `lsp` process and a thread that collects what it writes."""

  def __init__(self, compiler, workspace):
    environment = dict(os.environ)
    environment.setdefault("IGNIS_STD_PATH", str(REPOSITORY_ROOT / "std"))

    self.stderr = tempfile.TemporaryFile()
    self.process = subprocess.Popen(
      [str(compiler), "lsp"],
      stdin=subprocess.PIPE,
      stdout=subprocess.PIPE,
      stderr=self.stderr,
      cwd=workspace,
      env=environment,
    )
    self.messages = queue.Queue()
    self.write_lock = threading.Lock()
    self.reader = threading.Thread(target=self.collect, daemon=True)
    self.reader.start()

  def collect(self):
    while True:
      body = read_frame(self.process.stdout)

      if body is None:
        self.messages.put(None)
        return

      message = json.loads(body.decode("utf-8"))

      # The host server registers file watchers with a request of its own.
      if "method" in message and "id" in message:
        self.send({"jsonrpc": "2.0", "id": message["id"], "result": None})
        continue

      self.messages.put(message)

  def send_raw(self, body):
    with self.write_lock:
      self.process.stdin.write(frame(body))
      self.process.stdin.flush()

  def send(self, message):
    self.send_raw(json.dumps(message, ensure_ascii=False).encode("utf-8"))

  def request(self, request_id, method, params=None):
    message = {"jsonrpc": "2.0", "id": request_id, "method": method}

    if params is not None:
      message["params"] = params

    self.send(message)

  def notify(self, method, params=None):
    message = {"jsonrpc": "2.0", "method": method}

    if params is not None:
      message["params"] = params

    self.send(message)

  def next_message(self, accept):
    """The next message `accept` returns true for. Others are dropped."""
    while True:
      try:
        message = self.messages.get(timeout=MESSAGE_TIMEOUT_SECONDS)
      except queue.Empty:
        raise AssertionError("the server sent nothing for %d seconds; stderr:\n%s" % (
          MESSAGE_TIMEOUT_SECONDS, self.stderr_text()))

      if message is None:
        raise AssertionError("the server closed its output; stderr:\n%s" % self.stderr_text())

      if accept(message):
        return message

  def response(self, request_id):
    return self.next_message(lambda message: "method" not in message and message.get("id") == request_id)

  def published(self, uri):
    return self.next_message(
      lambda message: message.get("method") == "textDocument/publishDiagnostics"
      and message["params"]["uri"] == uri
    )["params"]

  def initialize(self, workspace):
    self.request(1, "initialize", {
      "processId": os.getpid(),
      "rootUri": Path(workspace).as_uri(),
      "capabilities": {},
    })
    result = self.response(1)["result"]
    self.notify("initialized", {})
    return result

  def hover(self, request_id, uri, position):
    self.request(request_id, "textDocument/hover", {"textDocument": {"uri": uri}, "position": position})
    return self.response(request_id)["result"]

  def definition(self, request_id, uri, position):
    self.request(request_id, "textDocument/definition", {"textDocument": {"uri": uri}, "position": position})
    return self.response(request_id)["result"]

  def references(self, request_id, uri, position, include_declaration):
    self.request(request_id, "textDocument/references", {
      "textDocument": {"uri": uri},
      "position": position,
      "context": {"includeDeclaration": include_declaration},
    })
    return self.response(request_id)["result"]

  def document_symbols(self, request_id, uri):
    self.request(request_id, "textDocument/documentSymbol", {"textDocument": {"uri": uri}})
    return self.response(request_id)["result"]

  def wait(self):
    self.process.stdin.close()

    try:
      return self.process.wait(timeout=MESSAGE_TIMEOUT_SECONDS)
    except subprocess.TimeoutExpired:
      self.process.kill()
      raise AssertionError("the server did not exit")

  def stderr_text(self):
    self.stderr.seek(0)
    return self.stderr.read().decode("utf-8", "replace")

  def close(self):
    if self.process.poll() is None:
      self.process.kill()
      self.process.wait()

    self.process.stdout.close()
    self.stderr.close()


@unittest.skipIf(compiler_path() is None, "no compiler: pass --compiler PATH or set IGNIS_LSP_COMPILER")
class LanguageServerTest(unittest.TestCase):

  def setUp(self):
    self.workspace = tempfile.mkdtemp(prefix="ignis-lsp-test-")
    self.addCleanup(shutil.rmtree, self.workspace, True)
    self.server = LanguageServer(compiler_path(), self.workspace)
    self.addCleanup(self.server.close)

  def source_file(self, name, text):
    path = Path(self.workspace) / name
    path.write_text(text, encoding="utf-8")
    return path.resolve().as_uri()

  def test_diagnostics_follow_the_buffer_and_the_session_ends_cleanly(self):
    uri = self.source_file("main.ign", FIXED_SOURCE)

    capabilities = self.server.initialize(self.workspace)["capabilities"]
    self.assertEqual(capabilities["textDocumentSync"], 1)

    self.server.notify("textDocument/didOpen", {
      "textDocument": {"uri": uri, "languageId": "ignis", "version": 1, "text": BROKEN_SOURCE},
    })
    opened = self.server.published(uri)

    if "-v" in sys.argv:
      print("\npublished after didOpen: " + json.dumps(opened, ensure_ascii=False), file=sys.stderr)

    self.assertEqual(len(opened["diagnostics"]), 1, opened)
    diagnostic = opened["diagnostics"][0]
    self.assertEqual(diagnostic["range"], EXPECTED_RANGE)
    self.assertEqual(diagnostic["severity"], 1)
    self.assertEqual(diagnostic["code"], EXPECTED_CODE)
    self.assertEqual(diagnostic["source"], "ignis")
    self.assertEqual(diagnostic["message"], EXPECTED_MESSAGE)

    self.server.notify("textDocument/didChange", {
      "textDocument": {"uri": uri, "version": 2},
      "contentChanges": [{"text": FIXED_SOURCE}],
    })
    changed = self.server.published(uri)

    if "-v" in sys.argv:
      print("published after didChange: " + json.dumps(changed, ensure_ascii=False), file=sys.stderr)

    self.assertEqual(changed["diagnostics"], [])

    self.server.request(2, "shutdown")
    self.assertIsNone(self.server.response(2)["result"])

    self.server.notify("exit")
    self.assertEqual(self.server.wait(), 0)

  def test_closing_a_document_clears_its_diagnostics(self):
    uri = self.source_file("main.ign", FIXED_SOURCE)
    self.server.initialize(self.workspace)

    self.server.notify("textDocument/didOpen", {
      "textDocument": {"uri": uri, "languageId": "ignis", "version": 1, "text": BROKEN_SOURCE},
    })
    opened = self.server.published(uri)
    self.assertEqual(len(opened["diagnostics"]), 1, opened)

    # The host clears a closed file's diagnostics too:
    # crates/ignis_lsp/src/server.rs, `did_close`.
    self.server.notify("textDocument/didClose", {"textDocument": {"uri": uri}})
    closed = self.server.published(uri)
    self.assertEqual(closed["diagnostics"], [])
    self.assertNotIn("version", closed)

    self.server.request(2, "shutdown")
    self.assertIsNone(self.server.response(2)["result"])

    self.server.notify("exit")
    self.assertEqual(self.server.wait(), 0)

  def assert_hover(self, request_id, uri, text, needle, expected, occurrence=1, delta=0):
    result = self.server.hover(request_id, uri, position_of(text, needle, occurrence, delta))

    if "-v" in sys.argv:
      print("hover %r: %s" % (needle, json.dumps(result, ensure_ascii=False)), file=sys.stderr)

    self.assertIsNotNone(result, needle)
    self.assertEqual(result["contents"], {"kind": "markdown", "value": expected})
    return result

  def test_hovers_describe_what_is_under_the_cursor(self):
    self.source_file("util.ign", HOVER_UTIL_SOURCE)
    uri = self.source_file("main.ign", HOVER_SOURCE)

    capabilities = self.server.initialize(self.workspace)["capabilities"]
    self.assertTrue(capabilities["hoverProvider"])

    self.server.notify("textDocument/didOpen", {
      "textDocument": {"uri": uri, "languageId": "ignis", "version": 1, "text": HOVER_SOURCE},
    })

    declared = self.assert_hover(2, uri, HOVER_SOURCE, "count", HOVER_COUNT, delta=1)
    self.assertEqual(declared["range"], {
      "start": position_of(HOVER_SOURCE, "count"),
      "end": position_of(HOVER_SOURCE, "count", delta=5),
    })
    self.assertNotEqual(declared["range"]["start"]["character"], HOVER_SOURCE.split("\n")[14].index("count"))

    self.assert_hover(3, uri, HOVER_SOURCE, "count", HOVER_COUNT, occurrence=2)
    self.assert_hover(4, uri, HOVER_SOURCE, "add(1", HOVER_ADD)
    self.assert_hover(5, uri, HOVER_SOURCE, "point.x", HOVER_FIELD, delta=6)
    self.assert_hover(6, uri, HOVER_SOURCE, "double(count)", HOVER_DOUBLE, delta=2)

    self.assertIsNone(self.server.hover(7, uri, {"line": 16, "character": 0}))
    self.assertIsNone(self.server.hover(8, uri, position_of(HOVER_SOURCE, "  return point", delta=1)))

    self.server.request(9, "shutdown")
    self.assertIsNone(self.server.response(9)["result"])

    self.server.notify("exit")
    self.assertEqual(self.server.wait(), 0)

  def test_hovers_survive_an_edit_that_breaks_parsing(self):
    self.source_file("util.ign", HOVER_UTIL_SOURCE)
    uri = self.source_file("main.ign", HOVER_SOURCE)
    self.server.initialize(self.workspace)

    self.server.notify("textDocument/didOpen", {
      "textDocument": {"uri": uri, "languageId": "ignis", "version": 1, "text": HOVER_SOURCE},
    })
    self.assert_hover(2, uri, HOVER_SOURCE, "add(1", HOVER_ADD)

    broken = HOVER_SOURCE + "function broken(\n"
    self.server.notify("textDocument/didChange", {
      "textDocument": {"uri": uri, "version": 2},
      "contentChanges": [{"text": broken}],
    })
    self.assert_hover(3, uri, broken, "add(1", HOVER_ADD)

    self.server.request(4, "shutdown")
    self.assertIsNone(self.server.response(4)["result"])

    self.server.notify("exit")
    self.assertEqual(self.server.wait(), 0)

  def test_hovers_after_an_edit_before_them_move_with_the_text(self):
    self.source_file("util.ign", HOVER_UTIL_SOURCE)
    uri = self.source_file("main.ign", HOVER_SOURCE)
    self.server.initialize(self.workspace)

    self.server.notify("textDocument/didOpen", {
      "textDocument": {"uri": uri, "languageId": "ignis", "version": 1, "text": HOVER_SOURCE},
    })
    self.assert_hover(2, uri, HOVER_SOURCE, "add(1", HOVER_ADD)

    broken = "function broken(\n" + HOVER_SOURCE
    self.server.notify("textDocument/didChange", {
      "textDocument": {"uri": uri, "version": 2},
      "contentChanges": [{"text": broken}],
    })
    moved = self.assert_hover(3, uri, broken, "add(1", HOVER_ADD, delta=1)
    self.assertEqual(moved["range"], {
      "start": position_of(broken, "add(1"),
      "end": position_of(broken, "add(1", delta=3),
    })
    self.assertIsNone(self.server.hover(4, uri, position_of(broken, "broken", delta=2)))

    self.server.request(5, "shutdown")
    self.assertIsNone(self.server.response(5)["result"])

    self.server.notify("exit")
    self.assertEqual(self.server.wait(), 0)

  def open_navigation_project(self):
    """Writes the navigation sources, opens `main.ign` and returns the URIs
    of `main.ign` and `util.ign`."""
    util_uri = self.source_file("util.ign", NAVIGATION_UTIL_SOURCE)
    uri = self.source_file("main.ign", NAVIGATION_SOURCE)
    capabilities = self.server.initialize(self.workspace)["capabilities"]
    self.assertTrue(capabilities["definitionProvider"])
    self.assertTrue(capabilities["referencesProvider"])
    self.assertIn("documentSymbolProvider", capabilities)

    self.server.notify("textDocument/didOpen", {
      "textDocument": {"uri": uri, "languageId": "ignis", "version": 1, "text": NAVIGATION_SOURCE},
    })
    return uri, util_uri

  def finish(self, request_id):
    self.server.request(request_id, "shutdown")
    self.assertIsNone(self.server.response(request_id)["result"])

    self.server.notify("exit")
    self.assertEqual(self.server.wait(), 0)

  def show(self, label, result):
    if "-v" in sys.argv:
      print("%s: %s" % (label, json.dumps(result, ensure_ascii=False)), file=sys.stderr)

  def test_definitions_lead_to_declarations_in_the_project_and_std(self):
    uri, util_uri = self.open_navigation_project()
    source = NAVIGATION_SOURCE

    local = self.server.definition(2, uri, position_of(source, "count)", delta=1))
    self.show("definition of a local", local)
    self.assertEqual(local, {"uri": uri, "range": span_of(source, "let count", "double(SCALE);")})

    imported = self.server.definition(3, uri, position_of(source, "double(SCALE)", delta=2))
    self.show("definition of an imported function", imported)
    self.assertEqual(imported["uri"], util_uri)
    self.assertIn("double(value: i32): i32", text_in(NAVIGATION_UTIL_SOURCE, imported["range"]))

    method = self.server.definition(4, uri, position_of(source, "point.length", delta=7))
    self.show("definition of a method", method)
    self.assertEqual(method, {"uri": uri, "range": span_of(source, "length(&self)", "self.y;\n  }")})

    field = self.server.definition(5, uri, position_of(source, "point.x", delta=6))
    self.show("definition of a field", field)
    self.assertEqual(field, {"uri": uri, "range": range_of(source, "x: i32;")})

    constant = self.server.definition(6, uri, position_of(source, "SCALE)", delta=1))
    self.show("definition of a constant", constant)
    self.assertEqual(constant, {"uri": uri, "range": range_of(source, "const SCALE: i32 = 3;")})

    standard = self.server.definition(7, uri, position_of(source, "Io::println", delta=6))
    self.show("definition of a std function", standard)
    self.assertIsNotNone(standard)
    self.assertTrue(standard["uri"].endswith("/std/io/mod.ign"), standard["uri"])
    io_source = Path(unquote(urlparse(standard["uri"]).path)).read_text(encoding="utf-8")
    self.assertIn("println(", text_in(io_source, standard["range"]))

    self.assertIsNone(self.server.definition(8, uri, position_of(source, "  let point", delta=1)))
    self.assertIsNone(self.server.definition(9, uri, {"line": 2, "character": 0}))
    self.finish(10)

  def test_references_follow_includeDeclaration_across_files(self):
    uri, util_uri = self.open_navigation_project()
    source = NAVIGATION_SOURCE
    uses = [
      {"uri": uri, "range": range_of(source, "double from", length=6)},
      {"uri": uri, "range": range_of(source, "double(SCALE)", length=6)},
      {"uri": uri, "range": range_of(source, "double(count)", length=6)},
    ]
    declaration = {"uri": util_uri, "range": range_of(NAVIGATION_UTIL_SOURCE, "double(value", length=6)}

    with_declaration = self.server.references(2, uri, position_of(source, "double(count)", delta=3), True)
    self.show("references with the declaration", with_declaration)
    self.assertEqual(with_declaration, uses + [declaration])

    without_declaration = self.server.references(3, uri, position_of(source, "double(count)", delta=3), False)
    self.assertEqual(without_declaration, uses)

    count_uses = [{"uri": uri, "range": range_of(source, "count)", length=5)}]
    count_declaration = {"uri": uri, "range": range_of(source, "count: i32", length=5)}
    self.assertEqual(self.server.references(4, uri, position_of(source, "count)"), True), [count_declaration] + count_uses)
    self.assertEqual(self.server.references(5, uri, position_of(source, "count: i32", delta=2), False), count_uses)

    field = self.server.references(6, uri, position_of(source, "point.x", delta=6), False)
    self.show("references of a field", field)
    self.assertEqual(field, [
      {"uri": uri, "range": range_of(source, "self.x", delta=5)},
      {"uri": uri, "range": range_of(source, "x: 0", length=1)},
      {"uri": uri, "range": range_of(source, "point.x", delta=6)},
    ])

    self.assertIsNone(self.server.references(7, uri, position_of(source, "  return point", delta=1), True))
    self.finish(8)

  def test_document_symbols_nest_members_with_their_kinds(self):
    uri, _ = self.open_navigation_project()
    source = NAVIGATION_SOURCE

    symbols = self.server.document_symbols(2, uri)
    self.show("document symbols", symbols)
    self.assertEqual(outline_of(symbols), NAVIGATION_OUTLINE)

    by_name = {symbol["name"]: symbol for symbol in symbols}
    self.assertEqual(by_name["Point"]["range"], span_of(source, "record Point", "}\n}"))
    self.assertEqual(by_name["Point"]["selectionRange"], range_of(source, "Point {", length=5))
    self.assertEqual(by_name["SCALE"]["range"], range_of(source, "const SCALE: i32 = 3;"))
    self.assertEqual(by_name["SCALE"]["selectionRange"], range_of(source, "SCALE:", length=5))

    fields = {symbol["name"]: symbol for symbol in by_name["Point"]["children"]}
    self.assertEqual(fields["x"]["range"], range_of(source, "x: i32;"))
    self.assertEqual(fields["x"]["selectionRange"], range_of(source, "x: i32;", length=1))
    self.finish(3)

  def test_navigation_after_an_edit_that_breaks_parsing_moves_with_the_text(self):
    uri, util_uri = self.open_navigation_project()
    self.assertEqual(self.server.definition(2, uri, position_of(NAVIGATION_SOURCE, "double(SCALE)"))["uri"], util_uri)

    broken = "function broken(\n" + NAVIGATION_SOURCE
    self.server.notify("textDocument/didChange", {
      "textDocument": {"uri": uri, "version": 2},
      "contentChanges": [{"text": broken}],
    })

    local = self.server.definition(3, uri, position_of(broken, "count)", delta=1))
    self.assertEqual(local, {"uri": uri, "range": span_of(broken, "let count", "double(SCALE);")})

    uses = self.server.references(4, uri, position_of(broken, "count)"), False)
    self.assertEqual(uses, [{"uri": uri, "range": range_of(broken, "count)", length=5)}])

    symbols = self.server.document_symbols(5, uri)
    self.assertEqual(outline_of(symbols), NAVIGATION_OUTLINE)
    self.assertEqual(
      {symbol["name"]: symbol for symbol in symbols}["Point"]["range"],
      span_of(broken, "record Point", "}\n}"),
    )

    self.assertIsNone(self.server.definition(6, uri, position_of(broken, "broken", delta=2)))
    self.finish(7)

  def test_errors_answer_bad_messages_and_exit_without_shutdown_fails(self):
    self.server.initialize(self.workspace)

    self.server.request(2, "textDocument/unknownRequest", {})
    self.assertEqual(self.server.response(2)["error"]["code"], -32601)

    self.server.send_raw(b'{"jsonrpc": "2.0", "id": ')
    parse_error = self.server.next_message(lambda message: "error" in message and message.get("id") is None)
    self.assertEqual(parse_error["error"]["code"], -32700)

    self.server.notify("$/unknownNotification", {})
    self.server.request(3, "textDocument/unknownRequest", {})
    self.assertEqual(self.server.response(3)["error"]["code"], -32601)

    self.server.notify("exit")
    self.assertEqual(self.server.wait(), 1)

  def test_an_oversized_message_is_skipped_and_the_session_goes_on(self):
    self.server.initialize(self.workspace)

    self.server.send_raw(b" " * (MAX_MESSAGE_BYTES + 1))
    oversized = self.server.next_message(lambda message: "error" in message and message.get("id") is None)
    self.assertEqual(oversized["error"]["code"], -32600)

    self.server.request(2, "shutdown")
    self.assertIsNone(self.server.response(2)["result"])

    self.server.notify("exit")
    self.assertEqual(self.server.wait(), 0)


if __name__ == "__main__":
  unittest.main()
