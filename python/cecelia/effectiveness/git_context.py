"""Git-context helpers shared between recital and the commit hook — PR / branch / SHA discovery,
and the one unified-diff parser (`parse_diff`) the mechanical recital checks build on.

Kept together so a fresh subagent looking for "how does this code discover the current PR"
finds ONE canonical answer, rather than two copies that could drift on the next change (e.g.
adding a fallback to `GITHUB_PR_NUMBER` env for CI). Convention-check reviewer previously
would catch a duplicate — this module is the pre-emptive extraction.
"""
from __future__ import annotations

import json
import re
import shutil
import subprocess
from dataclasses import dataclass, field
from pathlib import Path

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
    made) AND the same branch — parallel worktrees share a base SHA, so SHA alone isn't an
    identity (see the hook's `_same_change`).
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


def repo_root() -> Path:
    """`git rev-parse --show-toplevel`, or cwd as a last resort (not a repo, `git` missing)."""
    git = shutil.which("git")
    if git is not None:
        try:
            result = subprocess.run(
                [git, "rev-parse", "--show-toplevel"],
                capture_output=True, text=True, timeout=10.0, check=False, encoding="utf-8",
            )
        except (subprocess.TimeoutExpired, OSError):
            result = None
        if result is not None and result.returncode == 0 and result.stdout.strip():
            return Path(result.stdout.strip())
    return Path.cwd()


def main_checkout(cwd: str | None = None) -> Path | None:
    """The main checkout every worktree of this repo belongs to (the parent of git's common dir),
    whichever worktree `cwd` is in. None when `cwd` is not in a repo."""
    common = git_output("rev-parse", "--path-format=absolute", "--git-common-dir", cwd=cwd)
    return Path(common).parent if common else None


def merged_prs_since(since: str, cwd: str | None = None) -> list[dict] | None:
    """PRs merged on or after `since` (a date) via `gh pr list --search merged:>=…`, each with
    `number`, `headRefName`, `headRefOid` (the head it merged at) and `mergeCommit`.

    None on any failure (no `gh`, offline, not authenticated, timeout), so a caller can tell "no
    PRs" from "couldn't ask". Same ladder as `pr_for_branch`, with a longer timeout for the list.
    """
    gh = shutil.which("gh")
    if gh is None:
        return None
    try:
        result = subprocess.run(
            [gh, "pr", "list", "--state", "merged", "--search", f"merged:>={since}",
             "--json", "number,headRefName,headRefOid,mergeCommit", "--limit", "200"],
            capture_output=True, text=True, timeout=60.0, check=False, encoding="utf-8", cwd=cwd,
        )
    except (subprocess.TimeoutExpired, OSError):
        return None
    if result.returncode != 0:
        return None
    try:
        return json.loads(result.stdout or "[]")
    except ValueError:
        return None


def merged_pr_heads(branch: str, cwd: str | None = None) -> list[dict] | None:
    """Merged PRs opened from `branch`, each with `number` and `headRefOid` (the head it merged at).

    A squash merge leaves no ancestry link to main, so "is this branch's work merged?" is answered
    by comparing a local HEAD against these heads. None on any failure, so a caller can tell "no
    merged PR" (`[]`) from "couldn't ask". Same ladder as `pr_for_branch`.
    """
    gh = shutil.which("gh")
    if gh is None:
        return None
    try:
        result = subprocess.run(
            [gh, "pr", "list", "--head", branch, "--state", "merged",
             "--json", "number,headRefOid"],
            capture_output=True, text=True, timeout=10.0, check=False, encoding="utf-8", cwd=cwd,
        )
    except (subprocess.TimeoutExpired, OSError):
        return None
    if result.returncode != 0:
        return None
    try:
        return json.loads(result.stdout or "[]")
    except ValueError:
        return None


def git_output(*args: str, cwd: str | None = None) -> str | None:
    """`git <args>` stdout (stripped), or None on any failure — not a repo, `git` missing, non-zero
    exit, timeout. For one-off git-state questions (the commit hook's `core.hooksPath` /
    in-progress-merge checks) so they don't grow a second subprocess wrapper. `cwd` defaults to
    the process cwd."""
    git = shutil.which("git")
    if git is None:
        return None
    try:
        result = subprocess.run(
            [git, *args], capture_output=True, text=True, timeout=10.0, check=False,
            encoding="utf-8", cwd=cwd,
        )
    except (subprocess.TimeoutExpired, OSError):
        return None
    return result.stdout.strip() if result.returncode == 0 else None


#: `@@ -a,b +c,d @@` — only the new-side start matters; `,d` is absent for one-line hunks.
_HUNK_HEADER = re.compile(r"^@@ -\d+(?:,\d+)? \+(\d+)(?:,\d+)? @@")
_FILE_HEADER = re.compile(r"^diff --git a/(\S+) b/(\S+)$")


@dataclass(frozen=True)
class DiffLine:
    """One body line of a hunk. `kind` is `+` / `-` / ` `; `lineno` is the new-side line number
    (None for a removed line)."""
    kind: str
    text: str
    lineno: int | None


@dataclass
class FileDiff:
    """One file's slice of a unified diff: its path (new side), whether the diff creates or
    deletes it, and its hunks as ordered line lists."""
    path: str
    is_new: bool = False
    is_deleted: bool = False
    hunks: list[list[DiffLine]] = field(default_factory=list)

    @property
    def added(self) -> list[DiffLine]:
        return [ln for h in self.hunks for ln in h if ln.kind == "+"]

    @property
    def removed(self) -> list[DiffLine]:
        return [ln for h in self.hunks for ln in h if ln.kind == "-"]


def parse_diff(diff: str) -> list[FileDiff]:
    """Split a `git diff` into per-file hunks. The one line-level diff parser in the package —
    recital's mechanical checks build on it rather than regexing the raw text."""
    files: list[FileDiff] = []
    cur: FileDiff | None = None
    new_no = 0
    in_hunk = False
    for raw in diff.splitlines():
        if m := _FILE_HEADER.match(raw):
            cur = FileDiff(path=m.group(2))
            files.append(cur)
            in_hunk = False
            continue
        if cur is None:
            continue
        if m := _HUNK_HEADER.match(raw):
            cur.hunks.append([])
            new_no = int(m.group(1))
            in_hunk = True
            continue
        if not in_hunk:
            if raw.startswith("new file mode"):
                cur.is_new = True
            elif raw.startswith("deleted file mode"):
                cur.is_deleted = True
            continue
        kind, text = raw[:1], raw[1:]
        if kind == "+" or kind == " ":
            cur.hunks[-1].append(DiffLine(kind, text, new_no))
            new_no += 1
        elif kind == "-":
            cur.hunks[-1].append(DiffLine(kind, text, None))
        # `\ No newline at end of file` and anything else: not a body line.
    return files
