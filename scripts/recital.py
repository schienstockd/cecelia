#!/usr/bin/env python3
"""CLI wrapper for `cecelia.effectiveness.recital.run_recital`.

Reads `git diff --staged` (or from stdin with `--stdin`), spawns both reviewers, emits log
events, prints the recital body to stdout. Agent captures stdout and appends to the commit
message body.

Usage:
    pixi run recital

    # Custom diff (e.g. reviewing a specific commit):
    git show <sha> | python scripts/recital.py --stdin

Exit codes:
    0 — recital produced successfully (both reviewers ran or short-circuited cleanly).
    1 — one or more reviewer subprocesses failed hard. Recital still printed with an error
        note in the failed reviewer's section; log records the attempt with an `error` field.
    2 — invocation error (bad args, no staged diff to review).
"""
from __future__ import annotations

import argparse
import subprocess
import sys

from cecelia.effectiveness.recital import RecitalError, run_recital


def _parse_args() -> argparse.Namespace:
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument("--stdin", action="store_true", help="Read the diff from stdin instead of `git diff --staged`.")
    return p.parse_args()


def _get_diff(from_stdin: bool) -> str:
    if from_stdin:
        return sys.stdin.read()
    try:
        return subprocess.check_output(
            ["git", "diff", "--staged"], text=True, encoding="utf-8"
        )
    except subprocess.CalledProcessError as e:
        print(f"recital: `git diff --staged` failed: {e}", file=sys.stderr)
        raise SystemExit(2)


def main() -> int:
    args = _parse_args()
    diff = _get_diff(args.stdin)
    if not diff.strip():
        print("recital: no staged diff — nothing to review.", file=sys.stderr)
        return 2

    try:
        recital = run_recital(diff)
    except RecitalError as e:
        # A hard failure that run_recital itself couldn't recover from — unusual, since each
        # reviewer's error is caught inside `_run_reviewer` and folded into its section.
        print(f"recital: {e}", file=sys.stderr)
        return 1

    print(recital)
    return 0


if __name__ == "__main__":
    sys.exit(main())
