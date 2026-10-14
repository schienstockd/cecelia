"""Rows for the agents pane of `pixi run recital-console`: one per live Claude Code session.

Each row joins four sources: the session itself (`claude_sessions`), the git worktree its
current folder is in, the effectiveness log for that worktree's branch (recital rounds, open
findings) and the branch's newest PR with its CI state (`gh`, fetched in the background by
`PrCache` because one call takes ~3 s). Gathering (`agent_rows`) is kept apart from the pure joins
(`branch_recital`, `ci_state`, `pr_by_branch`, `sort_rows`), so tests drive those with made-up
rows. Design: `docs/todo/AGENTS_PANE_PLAN.md`.
"""
from __future__ import annotations

import dataclasses
import hashlib
import os
import threading
import time
import typing as _t

from .claude_sessions import Session
from .git_context import git_output, recent_prs
from .log import resolved_findings
from .palette import PALETTE

#: One recital round writes one of these (`pixi run recital` spawns the fanout audit every time).
ROUND_EVENT = "fanout_audit_run"

#: Sort rank by session status: a session that is idle is waiting for Dominik, so it comes first.
_STATUS_RANK = {"idle": 0, "busy": 2}

#: Check conclusions (CheckRun) and states (StatusContext) that mean CI failed.
_CI_FAILED = frozenset({"FAILURE", "TIMED_OUT", "CANCELLED", "ACTION_REQUIRED", "STARTUP_FAILURE",
                        "ERROR"})


#: A session's badge is one of these shapes in one of these colours (Okabe-Ito, CVD-safe): 28
#: combinations, so ten sessions rarely share one — and the name beside it settles it when they do.
BADGE_SHAPES = ("●", "■", "▲", "◆")
BADGE_SWATCHES = ("vermillion", "orange", "yellow", "bluish_green", "sky_blue", "blue",
                  "reddish_purple")


def session_badge(session_id: str, *, use_colour: bool = True) -> str:
    """The session's fixed shape-and-colour mark, derived from its id, so the agents pane and the
    session's own statusline (`scripts/statusline_badge.py`) draw the same one."""
    n = int(hashlib.sha1(session_id.encode("utf-8")).hexdigest(), 16)
    shape = BADGE_SHAPES[n % len(BADGE_SHAPES)]
    if not use_colour:
        return shape
    return f"{PALETTE[BADGE_SWATCHES[n // len(BADGE_SHAPES) % len(BADGE_SWATCHES)]]}{shape}\033[0m"


@dataclasses.dataclass(frozen=True)
class AgentRow:
    name: str
    status: str
    worktree: str | None    # top-level folder name of the git checkout; None outside any repo
    branch: str | None
    rounds: int = 0
    open_findings: int = 0
    readable: bool = True
    pr: int | None = None
    pr_state: str | None = None    # "OPEN" / "MERGED" / "CLOSED"
    ci: str | None = None          # "fail" / "running" / "pass"; None = no checks or no PR
    conflict: bool = False         # the open PR conflicts with its base (GitHub says CONFLICTING)
    #: Its turn is over and its last reply asked Dominik something — waiting for him, even while
    #: a background command (a CI watch) keeps the status at "shell".
    asks: bool = False
    judge: bool = False            # opened by `pixi run judge-review`, whose queue waits on it
    recital: bool = False          # `pixi run recital` is running under it — shown as `recite`
    auto: bool = False             # `judge-review --auto`'s fix agent: unattended, named `auto-fix`
    #: The branch's slugged findings, each with its outcome ("unresolved" until tagged) and the
    #: resolution row (None while unresolved) — the detail view (→) lists them.
    findings: tuple[tuple[dict, str, dict | None], ...] = ()
    session_id: str = ""
    recap: tuple[str, str] | None = None   # (timestamp, text): Claude Code's recap of the session
    context_pct: float | None = None       # % of the context window in use; None = unknown

    @property
    def done(self) -> bool:
        """The PR merged and the session isn't working: the window can be closed — after which
        `prune-worktrees` can remove its worktree (a session sitting in it keeps it IN USE)."""
        return self.pr_state == "MERGED" and self.status != "busy"


def branch_recital(events: _t.Iterable[dict], branch: str
                   ) -> tuple[int, list[tuple[dict, str, dict | None]]]:
    """(recital rounds, findings) for `branch`. Each finding is `(finding row, outcome,
    resolution row)`, outcome "unresolved" until the commit hook logs one — the open findings it
    blocks on. Unresolved first, then newest first. The join is `log.resolved_findings`."""
    rows = [e for e in events if e.get("branch") == branch]
    out = [(f, ((res.get("payload") or {}).get("outcome") or "no_outcome") if res else "unresolved", res)
           for f, res in resolved_findings(rows, branch).values()]
    out.sort(key=lambda t: t[0].get("ts") or "", reverse=True)
    out.sort(key=lambda t: t[1] != "unresolved")
    return sum(e.get("event") == ROUND_EVENT for e in rows), out


def ci_state(rollup: _t.Iterable[dict]) -> str | None:
    """One word for a PR's `statusCheckRollup`: any failed check → "fail", else any unfinished →
    "running", else "pass"; None when there are no checks."""
    checks = list(rollup)
    if not checks:
        return None
    outcomes = {(c.get("conclusion") or c.get("state") or "").upper() for c in checks}
    if outcomes & _CI_FAILED:
        return "fail"
    if any(c.get("status", "COMPLETED") != "COMPLETED" or c.get("state") in ("PENDING", "EXPECTED")
           for c in checks):
        return "running"
    return "pass"


def pr_by_branch(prs: _t.Iterable[dict]) -> dict[str, dict]:
    """Newest PR per head branch, from a newest-first PR list."""
    out: dict[str, dict] = {}
    for pr in prs:
        out.setdefault(pr.get("headRefName") or "", pr)
    return out


def _rank(r: AgentRow) -> float:
    """Asking Dominik something (-1), then idle — waiting for him (0) — unless its PR's CI is still
    running: then it waits on CI and will be woken by it (0.75). Done (merged, close the window)
    sits between the two. A judge-review session waiting for him (-1 or 0) comes before all of
    them: the review queue is blocked until he deals with it."""
    if r.asks:
        rank = -1.0
    elif r.done:
        rank = 0.5
    elif r.status == "idle" and r.ci == "running" and r.pr_state == "OPEN" and not r.conflict:
        rank = 0.75
    else:
        rank = _STATUS_RANK.get(r.status, 1)
    return -2 if r.judge and rank <= 0 else rank


def sort_rows(rows: _t.Iterable[AgentRow]) -> list[AgentRow]:
    """A judge-review session waiting for Dominik first (its queue waits on it), then a session
    asking him something, then idle (waiting for him) — idle with failed CI or a merge conflict
    before the rest — then done (merged, close the window), idle waiting on running CI, other
    states, busy; unreadable rows last. Within each, judge-review sessions lead."""
    return sorted(rows, key=lambda r: (not r.readable, _rank(r), not (r.ci == "fail" or r.conflict),
                                       not r.judge, r.name))


class PrCache:
    """`recent_prs`, refreshed in a background thread at most every `max_age` seconds, so the
    ~3 s `gh` call never stalls a repaint. `get()` returns the last good list ([] before the first
    one lands, or when `gh` is unavailable) and starts a refresh when it is stale."""

    def __init__(self, max_age: float = 60.0, fetch: _t.Callable[[], list[dict] | None] = recent_prs):
        self._max_age, self._fetch = max_age, fetch
        self._prs: list[dict] = []
        self._at = float("-inf")
        self._lock = threading.Lock()
        self._thread: threading.Thread | None = None

    def _run(self) -> None:
        prs = self._fetch()
        with self._lock:
            if prs is not None:
                self._prs = prs
            self._at = time.monotonic()

    def get(self) -> list[dict]:
        with self._lock:
            stale = time.monotonic() - self._at >= self._max_age
            if stale and (self._thread is None or not self._thread.is_alive()):
                self._thread = threading.Thread(target=self._run, daemon=True)
                self._thread.start()
            return self._prs


def _checkout(s: Session) -> str | None:
    """Top folder of the newest git checkout the session worked in (its shell may since have
    been reset to the workspace root, which is no checkout)."""
    for cwd in s.recent_cwds or ((s.cwd,) if s.cwd else ()):
        top = git_output("rev-parse", "--show-toplevel", cwd=cwd)
        if top:
            return top
    return None


def agent_rows(sessions: _t.Iterable[Session], events: _t.Sequence[dict],
               prs: _t.Iterable[dict] = ()) -> list[AgentRow]:
    """One sorted row per session. Runs two `git` calls per readable session; `prs` is a
    newest-first PR list (`recent_prs`, usually through `PrCache`)."""
    by_branch = pr_by_branch(prs)
    rows = []
    for s in sessions:
        top = _checkout(s)
        branch = (git_output("branch", "--show-current", cwd=top) or None) if top else None
        rounds, findings = branch_recital(events, branch) if branch else (0, [])
        pr = by_branch.get(branch) if branch else None
        rows.append(AgentRow(s.name, s.status, os.path.basename(top) if top else None, branch,
                             rounds, sum(o == "unresolved" for _, o, _ in findings), s.readable,
                             pr=pr.get("number") if pr else None,
                             pr_state=pr.get("state") if pr else None,
                             ci=ci_state(pr.get("statusCheckRollup") or ()) if pr else None,
                             conflict=bool(pr and pr.get("state") == "OPEN"
                                           and pr.get("mergeable") == "CONFLICTING"),
                             findings=tuple(findings), session_id=s.session_id, recap=s.recap,
                             context_pct=s.context_pct, asks=s.asks and s.status != "busy", judge=s.judge,
                             recital=s.recital, auto=s.auto))
    return sort_rows(rows)
