"""Git-context helpers shared between recital and the commit hook.

Kept together so a fresh subagent looking for "how does this code discover the current PR"
finds ONE canonical answer, rather than two copies that could drift on the next change (e.g.
adding a fallback to `GITHUB_PR_NUMBER` env for CI). Convention-check reviewer previously
would catch a duplicate — this module is the pre-emptive extraction.
"""
from __future__ import annotations

import shutil
import subprocess


def current_pr() -> str | None:
    """Best-effort PR-number capture via `gh pr view --json number`.

    Returns `"#N"` on success, `None` on any failure — no `gh`, not in a PR branch, offline,
    `gh` not authenticated, timeout. Callers treat None as "no PR context available" and log
    the row with `pr: null`; the presence of a PR is a nice-to-have, not required.
    """
    gh = shutil.which("gh")
    if gh is None:
        return None
    try:
        result = subprocess.run(
            [gh, "pr", "view", "--json", "number", "-q", ".number"],
            capture_output=True, text=True, timeout=10.0, check=False, encoding="utf-8",
        )
    except (subprocess.TimeoutExpired, OSError):
        return None
    if result.returncode != 0:
        return None
    n = (result.stdout or "").strip()
    return f"#{n}" if n.isdigit() else None
