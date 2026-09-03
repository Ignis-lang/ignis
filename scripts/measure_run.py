#!/usr/bin/env python3
"""Run a command and record its peak resident set size and wall time.

The child inherits stdin, stdout and stderr, so this wrapper can sit inside an
existing pipeline without changing what the command prints. Resource usage is
read from `getrusage(RUSAGE_CHILDREN)` after the child is reaped, which covers
the child and every descendant it waited for; on Linux `ru_maxrss` is in kB.
GNU `time` is not required.

The wrapper exits with the child's exit code (128 + signal when it was killed),
so callers keep their existing failure handling.
"""

import argparse
import json
import os
import resource
import subprocess
import sys
import time
from pathlib import Path


def measure(command: list[str], cwd: str | None) -> dict:
  started = time.monotonic()
  process = subprocess.Popen(command, cwd=cwd)
  return_code = process.wait()
  wall_seconds = time.monotonic() - started

  usage = resource.getrusage(resource.RUSAGE_CHILDREN)

  return {
    "command": command,
    "cwd": os.path.abspath(cwd) if cwd else os.getcwd(),
    "exit_code": return_code,
    "wall_s": round(wall_seconds, 3),
    "rss_kb": int(usage.ru_maxrss),
  }


def main() -> int:
  parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
  parser.add_argument("--cwd", help="working directory for the command")
  parser.add_argument("--out", help="write the measurement as JSON to this path")
  parser.add_argument("--label", help="free-form name stored in the JSON output")
  parser.add_argument("command", nargs=argparse.REMAINDER, help="command to run, after `--`")

  args = parser.parse_args()

  command = args.command
  if command and command[0] == "--":
    command = command[1:]

  if not command:
    parser.error("no command given (use `-- <command> [args...]`)")

  if args.cwd and not Path(args.cwd).is_dir():
    parser.error(f"--cwd is not a directory: {args.cwd}")

  measurement = measure(command, args.cwd)

  if args.label:
    measurement["label"] = args.label

  if args.out:
    out_path = Path(args.out)
    out_path.parent.mkdir(parents=True, exist_ok=True)
    out_path.write_text(json.dumps(measurement, indent=2) + "\n", encoding="utf-8")

  print(
    f"[measure] {args.label or command[0]}: "
    f"rss {measurement['rss_kb']} kB, wall {measurement['wall_s']} s, exit {measurement['exit_code']}",
    file=sys.stderr,
  )

  return_code = measurement["exit_code"]

  return 128 - return_code if return_code < 0 else return_code


if __name__ == "__main__":
  sys.exit(main())
