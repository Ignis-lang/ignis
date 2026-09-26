#!/usr/bin/env python3
"""Drives `<compiler> lsp` over stdio the way an editor does.

Spawns the language server, speaks JSON-RPC with Content-Length framing, and
checks the lifecycle (initialize, shutdown, exit and its exit code), the error
responses, and the diagnostics published for an open buffer: a type error
reported at the right UTF-16 range, then cleared once the buffer is fixed or
closed.
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
