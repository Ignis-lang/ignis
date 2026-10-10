#!/usr/bin/env python3
"""Check the platform constants written in std/libc/platform.ign.

On x86_64 Linux the standard library writes the value of every C platform
constant it uses (`O_RDONLY`, `ENOENT`, `TIOCGWINSZ`, ...) in Ignis, so no
backend needs a C header to know it. This script is what keeps those values
honest: it compiles a C program against the headers std/manifest.toml lists,
prints the real value of every constant, and compares it with the Ignis one.

It also checks that the three places naming the constants agree: the literal
table for x86_64 Linux, the table that reads header macros on every other
target, and the extern block that declares those macros.

Exit status: 0 when every value matches (or the host is not x86_64 Linux, which
the literal table does not describe), 1 on any mismatch or missing constant,
2 on a usage or toolchain error.
"""

import argparse
import os
import platform
import re
import subprocess
import sys
import tempfile
import tomllib

SCRIPTS_DIR = os.path.dirname(os.path.abspath(__file__))
PROJECT_ROOT = os.path.dirname(SCRIPTS_DIR)
# The C program is written and run under the project's build directory, never
# outside the checkout.
WORK_ROOT = os.path.join(PROJECT_ROOT, "build", "platform-constants")

LITERAL_CONDITION = '@configFlag(@platform("linux") && @arch("x86_64"))'
FALLBACK_CONDITION = '@configFlag(!(@platform("linux") && @arch("x86_64")))'

CONSTANT = re.compile(r"^\s*const ([A-Za-z_][A-Za-z_0-9]*): ([A-Za-z_:0-9]+)(?: = ([^;]+))?;\s*$")


def block_after(source, header):
    """The lines between `header` and the `}` that closes the block it opens."""
    start = source.find(header)
    if start < 0:
        raise ValueError(f"missing block: {header}")
    lines = source[start:].split("\n")[1:]
    body = []
    for line in lines:
        if line.startswith("}"):
            return body
        body.append(line)
    raise ValueError(f"unterminated block: {header}")


def constants_in(lines):
    """`(name, type, initializer)` for every constant declared in `lines`."""
    found = []
    for line in lines:
        match = CONSTANT.match(line)
        if match:
            found.append((match.group(1), match.group(2), match.group(3)))
    return found


def parse_platform(source):
    """The literal table, the header-reading table and the extern block."""
    literal = constants_in(block_after(source, LITERAL_CONDITION + "\nexport namespace Platform {"))
    fallback = constants_in(block_after(source, FALLBACK_CONDITION + "\nexport namespace Platform {"))
    externs = constants_in(block_after(source, FALLBACK_CONDITION + "\nextern __platform_headers {"))
    return literal, fallback, externs


def parse_value(text):
    """The integer an Ignis integer literal denotes."""
    cleaned = text.strip().replace("_", "")
    return int(cleaned, 0)


def consistency_errors(literal, fallback, externs):
    """Every way the three declarations of the constants disagree."""
    errors = []
    literal_names = [name for name, _, _ in literal]
    for label, other in (("header-reading Platform", fallback), ("extern __platform_headers", externs)):
        other_names = [name for name, _, _ in other]
        if other_names != literal_names:
            missing = sorted(set(literal_names) - set(other_names))
            extra = sorted(set(other_names) - set(literal_names))
            errors.append(f"{label} does not list the literal table's constants in the same order"
                          f" (missing: {missing or 'none'}, extra: {extra or 'none'})")
    literal_types = {name: ty for name, ty, _ in literal}
    for label, other in (("header-reading Platform", fallback), ("extern __platform_headers", externs)):
        for name, ty, _ in other:
            if name in literal_types and literal_types[name] != ty:
                errors.append(f"{name}: {label} declares {ty}, the literal table {literal_types[name]}")
    for name, _, initializer in fallback:
        if initializer != f"__platform_headers::{name}":
            errors.append(f"{name}: the header-reading table has to read __platform_headers::{name}")
    for name, _, initializer in externs:
        if initializer is not None:
            errors.append(f"{name}: an extern constant takes no initializer")
    if len(set(literal_names)) != len(literal_names):
        errors.append("the literal table names a constant twice")
    return errors


def manifest_headers(manifest_path):
    """Every system header the manifest's link sections include."""
    with open(manifest_path, "rb") as handle:
        manifest = tomllib.load(handle)
    headers = []
    for section in manifest.get("linking", {}).values():
        if not isinstance(section, dict) or section.get("header_quoted"):
            continue
        for header in section.get("headers", []):
            headers.append(header)
        if "header" in section:
            headers.append(section["header"])
    return headers


def host_values(names, headers, cc):
    """The value each name has in C on this host, compiled with `cc`."""
    lines = [f"#include <{header}>" for header in headers]
    lines.append("#include <stdio.h>")
    lines.append("int main(void) {")
    for name in names:
        lines.append(f'  printf("%s %lld\\n", "{name}", (long long)({name}));')
    lines.append("  return 0;")
    lines.append("}")
    os.makedirs(WORK_ROOT, exist_ok=True)
    with tempfile.TemporaryDirectory(prefix="run-", dir=WORK_ROOT) as work:
        source = os.path.join(work, "constants.c")
        binary = os.path.join(work, "constants")
        with open(source, "w") as handle:
            handle.write("\n".join(lines) + "\n")
        compiled = subprocess.run([cc, "-std=gnu11", "-o", binary, source], capture_output=True, text=True)
        if compiled.returncode != 0:
            return None, compiled.stderr
        ran = subprocess.run([binary], capture_output=True, text=True, check=True)
    values = {}
    for line in ran.stdout.splitlines():
        name, value = line.split()
        values[name] = int(value)
    return values, ""


def main(argv):
    parser = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    parser.add_argument("--platform-file", default=os.path.join(PROJECT_ROOT, "std", "libc", "platform.ign"))
    parser.add_argument("--manifest", default=os.path.join(PROJECT_ROOT, "std", "manifest.toml"))
    parser.add_argument("--cc", default=os.environ.get("CC", "gcc"))
    parser.add_argument("--any-host", action="store_true",
                        help="compare even when the host is not x86_64 Linux (for tests)")
    arguments = parser.parse_args(argv)

    with open(arguments.platform_file) as handle:
        source = handle.read()
    try:
        literal, fallback, externs = parse_platform(source)
    except ValueError as error:
        print(f"platform constants: {error}", file=sys.stderr)
        return 2

    errors = consistency_errors(literal, fallback, externs)
    for error in errors:
        print(f"platform constants: {error}", file=sys.stderr)
    if errors:
        return 1

    if not arguments.any_host and (platform.system() != "Linux" or platform.machine() != "x86_64"):
        print(f"platform constants: skipped, the literal table describes x86_64 Linux and this host is "
              f"{platform.machine()} {platform.system()}")
        return 0

    names = [name for name, _, _ in literal]
    values, compiler_error = host_values(names, manifest_headers(arguments.manifest), arguments.cc)
    if values is None:
        print("platform constants: the C program naming every constant did not compile:", file=sys.stderr)
        print(compiler_error, file=sys.stderr)
        return 1

    mismatches = 0
    for name, _, initializer in literal:
        expected = values[name]
        written = parse_value(initializer)
        if written != expected:
            mismatches += 1
            print(f"platform constants: {name} is {written} in std/libc/platform.ign, {expected} in C",
                  file=sys.stderr)
    if mismatches:
        return 1

    print(f"platform constants: {len(names)} constants match the C headers")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
