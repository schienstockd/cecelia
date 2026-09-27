"""Git-context helpers shared between recital and the commit hook.

Kept together so a fresh subagent looking for "how does this code discover the current PR"
finds ONE canonical answer, rather than two copies that could drift on the next change (e.g.
adding a fallback to `GITHUB_PR_NUMBER` env for CI). Convention-check reviewer previously
would catch a duplicate — this module is the pre-emptive extraction.
"""
from __future__ import annotations

import re
import shutil
import subprocess

_SHA_RE = re.compile(r"^[0-9a-f]{40}$")


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


def pr_for_branch(branch: str) -> str | None:
    """Best-effort PR-number lookup for a specific branch via `gh pr list --head <branch>`.

    Returns `"#N"` on success (the first PR opened for that branch, any state — merged,
    closed, open); `None` on any failure (no `gh`, no PR ever opened for the branch, offline,
    `gh` not authenticated, timeout).

    Companion to `current_pr()`: same shape (same fallback ladder, same 10s timeout, same
    encoding, same None-on-any-failure contract), different key. `current_pr()` answers
    "what PR am I on right now"; `pr_for_branch()` answers "what PR was ever opened for
    this branch name" — the latter's value is the rollup's cross-time join key when a row
    was written before its PR opened.
    """
    gh = shutil.which("gh")
    if gh is None:
        return None
    try:
        result = subprocess.run(
            [gh, "pr", "list", "--head", branch, "--state", "all", "--json", "number",
             "-q", ".[0].number"],
            capture_output=True, text=True, timeout=10.0, check=False, encoding="utf-8",
        )
    except (subprocess.TimeoutExpired, OSError):
        return None
    if result.returncode != 0:
        return None
    n = (result.stdout or "").strip()
    return f"#{n}" if n.isdigit() else None


def current_branch() -> str | None:
    """Best-effort branch name capture via `git rev-parse --abbrev-ref HEAD`.

    Returns the branch name on success, `None` on any failure — not a repo, detached HEAD
    (returns literal `"HEAD"` from git, which we treat as no-branch), `git` not installed,
    timeout.

    Recorded on every row so the rollup can join to a PR after the fact via
    `gh pr list --head <branch> --state all`. The rollup's PR resolution needs this because
    findings land pre-commit (before `pr` is knowable from `gh pr view`), and until the
    branch has a PR opened the row is `pr: null`. Branch-name → PR is a stable, one-shot
    lookup at rollup time — see `docs/todo/EFFECTIVENESS_LOG_PLAN.md`.
    """
    git = shutil.which("git")
    if git is None:
        return None
    try:
        result = subprocess.run(
            [git, "rev-parse", "--abbrev-ref", "HEAD"],
            capture_output=True, text=True, timeout=10.0, check=False, encoding="utf-8",
        )
    except (subprocess.TimeoutExpired, OSError):
        return None
    if result.returncode != 0:
        return None
    branch = (result.stdout or "").strip()
    if not branch or branch == "HEAD":
        return None  # detached HEAD → no branch to join on later
    return branch


def current_head_sha() -> str | None:
    """Best-effort HEAD commit SHA capture via `git rev-parse HEAD`.

    Returns the full 40-char SHA on success, `None` on any failure — not a repo, empty repo,
    `git` not installed, timeout. Used to anchor recital `_run` rows to the tree the review
    ran against; the commit hook enforces that findings-carrying commits have a matching
    `_run` row with `commit == HEAD-at-hook-time` (i.e. the parent SHA of the commit being
    made).
    """
    git = shutil.which("git")
    if git is None:
        return None
    try:
        result = subprocess.run(
            [git, "rev-parse", "HEAD"],
            capture_output=True, text=True, timeout=10.0, check=False, encoding="utf-8",
        )
    except (subprocess.TimeoutExpired, OSError):
        return None
    if result.returncode != 0:
        return None
    sha = (result.stdout or "").strip()
    return sha if _SHA_RE.match(sha) else None
