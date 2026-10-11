#!/usr/bin/env python3
"""Check that a definition gets the same symbol in every program.

Two programs are compiled to objects with `ignis build -c`. The second one
declares everything the first one uses, plus definitions of its own placed
ahead of them, so every definition id the first program sees is shifted in
the second. A symbol scheme derived from what a definition is names the shared
functions identically in both objects; one derived from the order a program
was analyzed in does not.

The check is that every symbol the first object defines is also defined by the
second, which holds exactly when the shared definitions keep their names.

Exit status: 0 when the symbols match, 1 when the first object defines a symbol
the second does not, 2 on a usage or toolchain error.
"""

import argparse
import os
import shutil
import subprocess
import sys
import uuid

SCRIPTS_DIR = os.path.dirname(os.path.abspath(__file__))
PROJECT_ROOT = os.path.dirname(SCRIPTS_DIR)
# Every program and object is written under the project's build directory.
WORK_ROOT = os.path.join(PROJECT_ROOT, "build", "symbol-stability")

SHARED = """
function describe(values: &Vector<i32>): String {
  let mut text: String = String::create("values:");
  let mut index: u64 = 0;

  while (index < values.length()) {
    if (let Option::SOME(value) = values.get(index)) {
      let piece: String = ` ${*value}`;
      text.pushStr(piece.toStr());
    }

    index += 1;
  }

  return text;
}
"""

FIRST = """import Vector from "std::vector";
""" + SHARED + """
function main(): i32 {
  let mut values: Vector<i32> = Vector::new<i32>();
  values.push(4);
  values.push(2);

  return describe(&values).length() as i32;
}
"""

# The same uses as FIRST, with definitions of its own ahead of them.
SECOND = """import Vector from "std::vector";

record Padding {
  public first: i64;
  public second: i64;
}

function padding(value: i64): Padding {
  return Padding { first: value, second: value + 1 };
}

function morePadding(values: &Vector<u64>): u64 {
  return values.length();
}
""" + SHARED + """
function main(): i32 {
  let mut values: Vector<i32> = Vector::new<i32>();
  values.push(4);
  values.push(2);

  let mut wide: Vector<u64> = Vector::new<u64>();
  wide.push(7);

  let extra: Padding = padding(1);
  let maybe: Option<i32> = Option::SOME(3);

  return describe(&values).length() as i32 + (extra.second as i32) + (morePadding(&wide) as i32) + (maybe.isSome() ? 1 : 0);
}
"""


def defined_symbols(nm_output):
    """The names an object defines, from `nm` output, local labels excluded."""
    names = set()
    for line in nm_output.splitlines():
        parts = line.split()
        if len(parts) != 3:
            continue
        _, kind, name = parts
        if kind in "UuvwVw" or name.startswith(".L"):
            continue
        names.add(name)
    return names


def missing_symbols(first, second):
    """The symbols `first` defines that `second` does not, sorted."""
    return sorted(first - second)


def compile_object(compiler, std_path, directory, source):
    os.makedirs(directory)
    with open(os.path.join(directory, "main.ign"), "w") as handle:
        handle.write(source)
    result = subprocess.run(
        [compiler, "build", "main.ign", "--std-path", std_path, "-O", "0", "-c", "-o", "main.o"],
        cwd=directory,
        capture_output=True,
        text=True,
    )
    if result.returncode != 0:
        return None, result.stdout + result.stderr
    listing = subprocess.run(["nm", os.path.join(directory, "main.o")], capture_output=True, text=True, check=True)
    return defined_symbols(listing.stdout), ""


def main(argv):
    parser = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    parser.add_argument("--compiler", required=True, help="ignis compiler binary")
    parser.add_argument("--std-path", default=os.path.join(PROJECT_ROOT, "std"))
    parser.add_argument("--keep", action="store_true", help="keep the programs and objects")
    arguments = parser.parse_args(argv)

    compiler = os.path.abspath(arguments.compiler)
    std_path = os.path.abspath(arguments.std_path)
    run_root = os.path.join(WORK_ROOT, uuid.uuid4().hex)

    try:
        first, first_error = compile_object(compiler, std_path, os.path.join(run_root, "first"), FIRST)
        if first is None:
            print("symbol stability: the first program did not compile:", file=sys.stderr)
            print(first_error, file=sys.stderr)
            return 2
        second, second_error = compile_object(compiler, std_path, os.path.join(run_root, "second"), SECOND)
        if second is None:
            print("symbol stability: the second program did not compile:", file=sys.stderr)
            print(second_error, file=sys.stderr)
            return 2

        missing = missing_symbols(first, second)
        if missing:
            print(f"symbol stability: {len(missing)} of {len(first)} symbols of the first program are named "
                  f"differently in the second, for example:", file=sys.stderr)
            for name in missing[:10]:
                print(f"  {name}", file=sys.stderr)
            return 1

        print(f"symbol stability: all {len(first)} symbols of the first program keep their names in the second")
        return 0
    finally:
        if not arguments.keep:
            shutil.rmtree(run_root, ignore_errors=True)


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
