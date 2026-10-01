"""Is the CLAUDE.md eval still running? The warning the recital console shows when it isn't.

The weekly pass writes one run record per pass to `eval-runs/` beside the effectiveness log
(`scripts/claude_md_eval/record.py`). A stopped timer or a pass that keeps failing is otherwise
silent, so the console says so. It warns, never blocks. Design:
docs/todo/CLAUDE_MD_EVAL_SUPERVISOR_PLAN.md → Decision 13.
"""
from __future__ import annotations

import datetime as _dt
import json
import pathlib

from .log import default_log_path

#: The pass runs weekly; two days' slack covers a late timer or a rerun the next morning.
CADENCE_DAYS = 7
SLACK_DAYS = 2


def eval_store(log_path: pathlib.Path | None = None) -> pathlib.Path:
    """Where run records live: `eval-runs/` beside the effectiveness log. The one definition."""
    return (log_path or default_log_path()).parent / "eval-runs"


def eval_record_warning(store: pathlib.Path | None = None, *, today: _dt.date | None = None,
                        cadence_days: int = CADENCE_DAYS) -> str | None:
    """One line to show, or None when the newest record is recent and passed.

    No store at all is silent: the eval was never set up here, which is not a regression.
    """
    store = store or eval_store()
    dated = []
    for path in store.glob("*.json") if store.is_dir() else []:
        try:
            dated.append((_dt.date.fromisoformat(path.stem), path))
        except ValueError:
            continue
    if not dated:
        return None
    newest, path = max(dated)
    today = today or _dt.datetime.now(_dt.timezone.utc).date()
    age = (today - newest).days
    try:
        failed = json.loads(path.read_text(encoding="utf-8")).get("kind") == "failure"
    except (OSError, ValueError):
        failed = False
    if age > cadence_days + SLACK_DAYS:
        return f"CLAUDE.md eval: no run record for {age} days (newest {newest}); is the timer running?"
    if failed:
        return f"CLAUDE.md eval: the {newest} pass failed; see {path}"
    return None
