#!/usr/bin/env python3
"""Read the effectiveness jsonl and rewrite `docs/ai-assist/EFFECTIVENESS.md`.

Runs on demand — `pixi run audit-rollup`. Prints the changed file's path to stdout so a
downstream `git diff` reviews it easily. Not auto-committed; the user reviews the diff and
commits if the update is meaningful (per docs/todo/EFFECTIVENESS_LOG_PLAN.md §Rollup mechanism).
"""

from __future__ import annotations

import pathlib
import sys

from cecelia.effectiveness import read_events, render_rollup

# `docs/ai-assist/EFFECTIVENESS.md` relative to the repo root (`.parent.parent` from scripts/).
_TARGET = pathlib.Path(__file__).resolve().parent.parent / "docs" / "ai-assist" / "EFFECTIVENESS.md"


def main() -> int:
    md = render_rollup(read_events())
    _TARGET.parent.mkdir(parents=True, exist_ok=True)
    _TARGET.write_text(md, encoding="utf-8")
    print(_TARGET)
    return 0


if __name__ == "__main__":
    sys.exit(main())
