#!/usr/bin/env python3
"""One-off, idempotent migration of the Rust e2e literal corpus into fixtures.

Reads every `e2e_test`, `e2e_test_allow_leak`, `e2e_workspace_std_test`,
`e2e_error_test`, `e2e_ownership_error_test` and `e2e_warning_test` case out of
`crates/ignis_driver/tests/e2e_ok.rs` / `e2e_err.rs` and writes each one as a
standalone `.ign` fixture under `test_cases/e2e/ok` or `test_cases/e2e/err`,
with the header option that reproduces the helper's mode:

  e2e_test                 -> (no header)
  e2e_test_allow_leak      -> // e2e: allow-leak
  e2e_workspace_std_test   -> // e2e: std
  e2e_error_test           -> // e2e: err
  e2e_ownership_error_test -> // e2e: err
  e2e_warning_test         -> // e2e: warn

Running it again with the corpus unchanged rewrites the same bytes, so it is
safe to re-run after editing a Rust literal before it is deleted.

A case whose helper call is not a plain `("name", r#"source"#)` or
`("name", "source")` literal is reported instead of migrated; none exist in
the current corpus, so `--list-skipped` reports an empty list today.

`--verify` compares every fixture's `__snapshots__/<name>.snap` against the
body (the part after the second `---`) of the insta snapshot the case used to
have, and reports every difference. Run it only after the fixture baselines
have been generated with `ignis test --update-snapshots e2e::`.

Two known exceptions to a mechanical port, both found while generating the
first baseline set:

- `callback_any_all`, `callback_for_each_by_value` and `callback_reduce`
  define top-level functions named `any`, `all`, `forEach` and `reduce`. A
  Rust `e2e_test` compiles with no std loaded, so those names never collided
  with anything; every fixture compiles with the project's std auto-loaded
  (`std::vector` is in `[auto_load]`), and `Vector`'s methods of the same
  names collide with the free functions at bind time
  (`Function 'X' is already defined`), a duplicate-definition check that does
  not appear to respect method-vs-free-function scoping. The migrated
  fixtures rename the free functions (`anyMatches`/`allMatch`/`forEachValue`/
  `reduceValues`) to route around the collision; this is a content-only edit
  applied by hand after the initial `migrate()` run, not reproduced by
  re-running it, since the observable behavior (exit code) does not depend on
  the helper's name.
- `enum_drop_custom` and `enum_drop_manual` apply `@implements(Drop)` directly
  to an enum with a manual `drop()` method. Under the project's std-loaded,
  multi-module fixture pipeline, the emitted `struct Resource` is missing the
  `__ignis_drop_state` field that the same enum's drop-glue code still reads,
  a codegen/binder inconsistency isolated to a Drop-implementing enum (every
  Drop-implementing record in the corpus is unaffected). Both cases still
  pass as plain Rust tests (`common::compile_and_run`, no std loaded), so they
  were kept there instead of migrated; `migrate()` does not write fixtures for
  them.
"""

import argparse
import re
from dataclasses import dataclass
from pathlib import Path

SCRIPT_DIR = Path(__file__).resolve().parent
REPO_ROOT = SCRIPT_DIR.parent

CORPUS_OK = "ok"
CORPUS_ERR = "err"

CORPUS_FILE = {
  CORPUS_OK: REPO_ROOT / "crates" / "ignis_driver" / "tests" / "e2e_ok.rs",
  CORPUS_ERR: REPO_ROOT / "crates" / "ignis_driver" / "tests" / "e2e_err.rs",
}

FIXTURE_DIR = {
  CORPUS_OK: REPO_ROOT / "test_cases" / "e2e" / "ok",
  CORPUS_ERR: REPO_ROOT / "test_cases" / "e2e" / "err",
}

SNAPSHOT_PREFIX = {CORPUS_OK: "e2e_ok__", CORPUS_ERR: "e2e_err__"}

INSTA_SNAPSHOT_DIR = REPO_ROOT / "crates" / "ignis_driver" / "tests" / "snapshots"

# Fixture header line each helper's mode maps to; `None` is the plain program
# mode, which needs no header at all.
OK_HEADERS = {
  "e2e_test": None,
  "e2e_test_allow_leak": "// e2e: allow-leak",
  "e2e_workspace_std_test": "// e2e: std",
}

ERR_HEADERS = {
  "e2e_error_test": "// e2e: err",
  "e2e_ownership_error_test": "// e2e: err",
  "e2e_warning_test": "// e2e: warn",
}

HEADERS_BY_CORPUS = {
  CORPUS_OK: OK_HEADERS,
  CORPUS_ERR: ERR_HEADERS,
}

CASE_HELPERS = {
  CORPUS_OK: set(OK_HEADERS),
  CORPUS_ERR: set(ERR_HEADERS),
}


@dataclass
class Case:
  name: str
  helper: str
  source: str | None
  skip_reason: str | None = None


class SourceScanner:
  """Minimal scanner over the Rust corpus, sufficient for the call shapes used."""

  def __init__(self, text: str):
    self.text = text
    self.position = 0

  def skip_trivia(self) -> None:
    while self.position < len(self.text):
      character = self.text[self.position]

      if character.isspace():
        self.position += 1
        continue

      if self.text.startswith("//", self.position):
        end = self.text.find("\n", self.position)
        self.position = len(self.text) if end == -1 else end + 1
        continue

      break

  def read_plain_string(self) -> str | None:
    if self.position >= len(self.text) or self.text[self.position] != '"':
      return None

    self.position += 1
    characters = []

    while self.position < len(self.text):
      character = self.text[self.position]

      if character == "\\":
        characters.append(self.text[self.position:self.position + 2])
        self.position += 2
        continue

      if character == '"':
        self.position += 1
        return "".join(characters)

      characters.append(character)
      self.position += 1

    return None

  def read_raw_string(self) -> str | None:
    if self.position >= len(self.text) or self.text[self.position] != "r":
      return None

    cursor = self.position + 1
    hashes = 0

    while cursor < len(self.text) and self.text[cursor] == "#":
      hashes += 1
      cursor += 1

    if cursor >= len(self.text) or self.text[cursor] != '"':
      return None

    terminator = '"' + "#" * hashes
    end = self.text.find(terminator, cursor + 1)

    if end == -1:
      return None

    value = self.text[cursor + 1:end]
    self.position = end + len(terminator)

    return value


def unescape_rust_string(value: str) -> str:
  escapes = {"n": "\n", "t": "\t", "r": "\r", "0": "\0", "\\": "\\", '"': '"', "'": "'"}
  characters = []
  index = 0

  while index < len(value):
    character = value[index]

    if character == "\\" and index + 1 < len(value):
      following = value[index + 1]

      if following in escapes:
        characters.append(escapes[following])
        index += 2
        continue

    characters.append(character)
    index += 1

  return "".join(characters)


def extract_cases(
  corpus_path: Path,
  helpers: set[str],
) -> list[Case]:
  text = corpus_path.read_text(encoding="utf-8")
  pattern = re.compile(r"(?<![A-Za-z0-9_])(" + "|".join(helpers) + r")\s*\(")
  cases: list[Case] = []

  for match in pattern.finditer(text):
    helper = match.group(1)
    preceding = text.rfind("\n", 0, match.start())

    # `fn e2e_test(` declares the helper instead of calling it.
    if text[preceding + 1:match.start()].rstrip().endswith("fn"):
      continue

    scanner = SourceScanner(text)
    scanner.position = match.end()
    scanner.skip_trivia()

    raw_name = scanner.read_plain_string()

    if raw_name is None:
      continue

    name = unescape_rust_string(raw_name)

    scanner.skip_trivia()

    if scanner.position >= len(text) or text[scanner.position] != ",":
      cases.append(Case(name, helper, None, skip_reason="source not literal"))
      continue

    scanner.position += 1
    scanner.skip_trivia()

    source = scanner.read_raw_string()

    if source is None:
      source = scanner.read_plain_string()
      source = unescape_rust_string(source) if source is not None else None

    if source is None:
      cases.append(Case(name, helper, None, skip_reason="source not literal"))
      continue

    cases.append(Case(name, helper, source))

  return cases


def read_snapshot_body(snapshot_path: Path) -> str | None:
  """Reads an insta snapshot's body: everything after its second `---` line."""
  if not snapshot_path.is_file():
    return None

  text = snapshot_path.read_text(encoding="utf-8")
  lines = text.split("\n")

  if not lines or lines[0].strip() != "---":
    return None

  for index in range(1, len(lines)):
    if lines[index].strip() == "---":
      return "\n".join(lines[index + 1:]).rstrip("\n")

  return None

# `(corpus, case_name) -> reason`: cases that stay as plain Rust tests instead
# of a fixture, because they hit a real, pre-existing compiler defect that
# only the project's std-loaded, multi-module fixture pipeline exposes (see
# the module docstring). Fixing that defect is out of scope for a corpus
# migration; excluding these two keeps the fixture corpus green.
EXCLUDED_CASES = {
  (CORPUS_OK, "enum_drop_custom"): (
    "a Drop-implementing enum's struct is emitted without the __ignis_drop_state "
    "field its own drop-glue reads, only under the fixture pipeline's std-loaded, "
    "multi-module compile"
  ),
  (CORPUS_OK, "enum_drop_manual"): (
    "a Drop-implementing enum's struct is emitted without the __ignis_drop_state "
    "field its own drop-glue reads, only under the fixture pipeline's std-loaded, "
    "multi-module compile"
  ),
}


def extract_corpus_cases(corpus: str) -> list["Case"]:
  return extract_cases(CORPUS_FILE[corpus], CASE_HELPERS[corpus])


def fixture_source(raw_source: str) -> str:
  """Strips the leading/trailing blank lines a Rust raw string literal carries.

  `r#"\\nfunction main() ...\\n"#` brackets the source with the newlines that
  sit right after `r#"` and right before `"#`; a fixture file should not
  reproduce those, only the code between them, with a single trailing newline.
  """
  return raw_source.strip("\n") + "\n"


def fixture_path(
  corpus: str,
  case_name: str,
) -> Path:
  return FIXTURE_DIR[corpus] / f"{case_name}.ign"


def fixture_snapshot_path(
  corpus: str,
  case_name: str,
) -> Path:
  return FIXTURE_DIR[corpus] / "__snapshots__" / f"{case_name}.snap"


def insta_snapshot_path(
  corpus: str,
  case_name: str,
) -> Path:
  return INSTA_SNAPSHOT_DIR / f"{SNAPSHOT_PREFIX[corpus]}{case_name}.snap"


def write_fixture(
  corpus: str,
  case: "Case",
) -> Path:
  header = HEADERS_BY_CORPUS[corpus][case.helper]
  body = fixture_source(case.source)
  content = f"{header}\n{body}" if header else body

  path = fixture_path(corpus, case.name)
  path.parent.mkdir(parents=True, exist_ok=True)
  path.write_text(content, encoding="utf-8")

  return path


def migrate() -> tuple[list[tuple[str, str, Path]], list[tuple[str, str, str, str]]]:
  """Writes every literal case as a fixture; returns (written, skipped)."""
  written = []
  skipped = []

  for corpus in (CORPUS_OK, CORPUS_ERR):
    for case in extract_corpus_cases(corpus):
      if case.skip_reason is not None:
        skipped.append((corpus, case.name, case.helper, case.skip_reason))
        continue

      if (corpus, case.name) in EXCLUDED_CASES:
        continue

      path = write_fixture(corpus, case)
      written.append((corpus, case.name, path))

  return written, skipped


def verify() -> list[str]:
  """Compares every migrated fixture's snapshot against its insta original."""
  differences = []

  for corpus in (CORPUS_OK, CORPUS_ERR):
    for case in extract_corpus_cases(corpus):
      if case.skip_reason is not None or (corpus, case.name) in EXCLUDED_CASES:
        continue

      expected = read_snapshot_body(insta_snapshot_path(corpus, case.name))

      if expected is None:
        differences.append(
          f"{corpus}/{case.name}: could not read the insta snapshot at "
          f"{insta_snapshot_path(corpus, case.name)}"
        )
        continue

      snapshot_path = fixture_snapshot_path(corpus, case.name)

      if not snapshot_path.is_file():
        differences.append(f"{corpus}/{case.name}: fixture snapshot missing at {snapshot_path}")
        continue

      actual = snapshot_path.read_text(encoding="utf-8")

      if actual != expected:
        differences.append(
          f"{corpus}/{case.name}: byte mismatch at {snapshot_path}\n"
          f"  expected (insta): {expected!r}\n"
          f"  actual (fixture): {actual!r}"
        )

  return differences


def main() -> int:
  parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
  parser.add_argument("--verify", action="store_true", help="compare fixture snapshots against the insta originals")
  parser.add_argument(
    "--list-skipped", action="store_true", help="only list the cases that could not be extracted, then exit"
  )
  args = parser.parse_args()

  if args.list_skipped:
    skipped = []

    for corpus in (CORPUS_OK, CORPUS_ERR):
      for case in extract_corpus_cases(corpus):
        if case.skip_reason is not None:
          skipped.append((corpus, case.name, case.helper, case.skip_reason))

    if not skipped:
      print("No cases were skipped: every helper call in the corpus is a plain string literal.")
      return 0

    for corpus, name, helper, reason in skipped:
      print(f"{corpus}/{name} ({helper}): {reason}")

    return 1

  if args.verify:
    differences = verify()

    if not differences:
      print("Every migrated fixture snapshot matches its insta original byte for byte.")
      return 0

    print(f"{len(differences)} fixture snapshot(s) differ from their insta original:\n")
    for difference in differences:
      print(difference)
      print()

    return 1

  written, skipped = migrate()

  by_corpus: dict[str, int] = {}
  for corpus, _, _ in written:
    by_corpus[corpus] = by_corpus.get(corpus, 0) + 1

  print(f"Wrote {len(written)} fixture(s): " + ", ".join(f"{corpus}={count}" for corpus, count in by_corpus.items()))

  if skipped:
    print(f"\n{len(skipped)} case(s) could not be extracted and were not migrated:")
    for corpus, name, helper, reason in skipped:
      print(f"  {corpus}/{name} ({helper}): {reason}")

    return 1

  return 0


if __name__ == "__main__":
  raise SystemExit(main())
