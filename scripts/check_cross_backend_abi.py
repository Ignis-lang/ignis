#!/usr/bin/env python3
"""Check that code built by different backends calls each other correctly.

Ignis-to-Ignis calls cross objects built by different backends when a program
links the std archive (`ignis build --std-archive`): the archive is always
built with C, the program with whichever backend `--target` names. This check
copies std, adds `scripts/abi_probe/abiprobe.ign` as `std::abiprobe` (functions
with every argument and result shape the platform ABI classifies differently),
and builds `scripts/abi_probe/main.ign`, which calls each of them, three ways:

- whole program with C, the reference;
- against the std archive with C;
- against the std archive with QBE.

Every build has to succeed, exit 0 and print exactly `all shapes agree`. A
shape whose arguments or result cross the boundary differently prints its name
instead, or crashes.

Exit status: 0 when the three builds agree, 1 when one fails or disagrees, 2 on
a usage or setup error (no compiler, no `qbe` on PATH).
"""

import argparse
import os
import shutil
import subprocess
import sys
import uuid

SCRIPTS_DIR = os.path.dirname(os.path.abspath(__file__))
PROJECT_ROOT = os.path.dirname(SCRIPTS_DIR)
PROBE_DIR = os.path.join(SCRIPTS_DIR, "abi_probe")
# Everything the check writes stays under the project's build directory.
WORK_ROOT = os.path.join(PROJECT_ROOT, "build", "cross-backend-abi")

EXPECTED_STDOUT = "all shapes agree\n"
MANIFEST_LINE = 'abiprobe = "abiprobe/mod.ign"\n'

PROJECT_FILE = """[package]
name = "abi-probe"
version = "0.1.0"

[ignis]
std = true

[build]
source_dir = "src"
entry = "main.ign"
out_dir = "build"
"""

# (label, target, extra build flags)
BUILDS = [
    ("whole-program C", "c", []),
    ("std archive, C program", "c", ["--std-archive"]),
    ("std archive, QBE program", "qbe", ["--std-archive"]),
]


def add_manifest_module(manifest_text):
    """`manifest_text` with the probe module registered under `[modules]`."""
    lines = manifest_text.splitlines(keepends=True)
    for index, line in enumerate(lines):
        if line.strip() == "[modules]":
            lines.insert(index + 1, MANIFEST_LINE)
            return "".join(lines)
    raise ValueError("the std manifest has no [modules] section")


def prepare(work, std_path):
    """Write the std copy with the probe module and the program project."""
    std_copy = os.path.join(work, "std")
    shutil.copytree(std_path, std_copy)
    os.makedirs(os.path.join(std_copy, "abiprobe"))
    shutil.copyfile(os.path.join(PROBE_DIR, "abiprobe.ign"), os.path.join(std_copy, "abiprobe", "mod.ign"))

    manifest_path = os.path.join(std_copy, "manifest.toml")
    with open(manifest_path, encoding="utf-8") as handle:
        manifest = handle.read()
    with open(manifest_path, "w", encoding="utf-8") as handle:
        handle.write(add_manifest_module(manifest))

    app = os.path.join(work, "app")
    os.makedirs(os.path.join(app, "src"))
    with open(os.path.join(app, "ignis.toml"), "w", encoding="utf-8") as handle:
        handle.write(PROJECT_FILE)
    shutil.copyfile(os.path.join(PROBE_DIR, "main.ign"), os.path.join(app, "src", "main.ign"))
    return std_copy, app


def outcome_problem(label, build, run):
    """What went wrong with one build and its run, or None when it agrees."""
    if build.returncode != 0:
        return f"{label}: the build failed\n{build.stdout}{build.stderr}"
    if run.returncode != 0 or run.stdout != EXPECTED_STDOUT:
        return (f"{label}: exit {run.returncode}, expected 0 and `all shapes agree`\n"
                f"stdout:\n{run.stdout}stderr:\n{run.stderr}")
    return None


def main(argv):
    parser = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    parser.add_argument("--compiler", required=True, help="ignis compiler binary")
    parser.add_argument("--std-path", default=os.path.join(PROJECT_ROOT, "std"))
    parser.add_argument("--keep", action="store_true", help="keep the generated projects")
    arguments = parser.parse_args(argv)

    compiler = os.path.abspath(arguments.compiler)
    if not os.access(compiler, os.X_OK):
        print(f"cross-backend ABI: no compiler at {compiler}", file=sys.stderr)
        return 2
    if shutil.which("qbe") is None:
        print("cross-backend ABI: `qbe` is not on PATH", file=sys.stderr)
        return 2

    work = os.path.join(WORK_ROOT, uuid.uuid4().hex)
    os.makedirs(work)

    try:
        std_copy, app = prepare(work, os.path.abspath(arguments.std_path))
        problems = []

        for index, (label, target, flags) in enumerate(BUILDS):
            out_dir = os.path.join(work, f"out-{index}")
            binary = os.path.join(work, f"bin-{index}")
            build = subprocess.run(
                [compiler, "build", "--std-path", std_copy, "--target", target, *flags,
                 "--out-dir", out_dir, "-o", binary],
                cwd=app,
                capture_output=True,
                text=True,
            )
            run = subprocess.CompletedProcess([], 1, "", "")
            if build.returncode == 0:
                run = subprocess.run([binary], capture_output=True, text=True)
            problem = outcome_problem(label, build, run)
            if problem:
                problems.append(problem)

        for problem in problems:
            print(f"cross-backend ABI: {problem}", file=sys.stderr)
        if problems:
            return 1

        print(f"cross-backend ABI: all shapes agree in {len(BUILDS)} builds")
        return 0
    finally:
        if not arguments.keep:
            shutil.rmtree(work, ignore_errors=True)


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
