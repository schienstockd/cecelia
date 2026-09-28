#!/usr/bin/env python3
"""Regenerate `docs/ai-assist/CLAUDE_MD_EVAL.md` from the effectiveness log.

Reads every `claude_md_eval_*` row from `~/.cecelia-effectiveness/events.jsonl` (or
`$CECELIA_EFFECTIVENESS_LOG`) and rewrites the markdown. `pixi run claude-md-eval`
also fires this at the end of each pass; use this standalone entry to re-render
without spawning any agents.

Prints the target path to stdout so a downstream `git diff` reviews it easily. Never
auto-committed — the user reviews and commits if the change is meaningful.
"""
from __future__ import annotations

import importlib.util as _importlib_util
import pathlib
import sys

_REPO = pathlib.Path(__file__).resolve().parents[2]
_ROLLUP_PATH = _REPO / "scripts" / "claude_md_eval" / "rollup.py"

sys.path.insert(0, str(_REPO / "python"))
from cecelia.effectiveness import read_events  # noqa: E402
from cecelia.utils.atomic_io import write_atomic  # noqa: E402

# `rollup` isn't a package — spec-load so this works whether or not scripts/ is on the
# Python path. Same pattern as run_suite's load of run_prompt.
_spec = _importlib_util.spec_from_file_location("claude_md_eval_rollup", _ROLLUP_PATH)
_rollup = _importlib_util.module_from_spec(_spec)
_spec.loader.exec_module(_rollup)

_TARGET = _REPO / "docs" / "ai-assist" / "CLAUDE_MD_EVAL.md"


def render_to_file(target: pathlib.Path = _TARGET) -> pathlib.Path:
    """Render + write. Returns the target path. Used by run_suite's post-pass hook.

    Uses `write_atomic` — a kill during the post-pass render otherwise truncates the
    committed docs artifact in place. Bonus: the atomic-io family is exactly what the
    eval framework's own `utf-8-json-write` prompt scores agents on.
    """
    md = _rollup.render_eval_rollup(read_events())
    target.parent.mkdir(parents=True, exist_ok=True)
    with write_atomic(target) as f:
        f.write(md)
    return target


def main() -> int:
    target = render_to_file()
    print(target)
    return 0


if __name__ == "__main__":
    sys.exit(main())
