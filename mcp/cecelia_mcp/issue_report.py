"""report_issue's working parts: a bug report a session drafts instead of working around Cecelia.

Why it exists. A session that hits a Cecelia bug or a missing capability had no sanctioned way out.
"Report fixes with the patch" named an action with no destination, so the helpful move was to keep
going: hand-edit core files or ccid.json, or script around the API. Each update wiped those edits and
none of them reached the maintainer. This gives the session a cheap, legitimate exit:

  * a DRAFT on this machine (`<config_dir>/issue-reports/`), complete and unredacted — it never
    leaves the machine unless the user sends it;
  * a prefilled link to the public bug form, with paths, the project and the image redacted and the
    log cut to fit GitHub's URL limit. The user reviews it in the form and submits it. Nothing here
    posts anything.

The `workaround` field is the signal worth counting: `proceeding-unasked` is a gap someone routed
around without being told to. The drafts directory is that log.

Pure functions plus one writer; the server attaches the context (version, the run's params, the log).
"""
from __future__ import annotations

import hashlib
import json
import os
import re
import time
from pathlib import Path
from urllib.parse import urlencode

from cecelia.utils.atomic_io import write_atomic

REPO_URL = "https://github.com/schienstockd/cecelia"

KINDS = ("bug", "missing", "unclear-docs", "unexpected-result")
# What the session does next. `proceeding-unasked` is the one to count (see the module docstring).
WORKAROUNDS = ("stopped", "proceeding-user-asked", "proceeding-unasked")

# GitHub rejects very long URLs (~8 KB); stay under with room for the encoding. Same cap as the
# frontend's problemReportUrl (frontend/src/lib/links.ts → REPORT_URL_MAX).
URL_MAX = 7500
LOG_TAIL_MAX = 3000       # bytes of task log kept in the link before cutting further
PARAMS_MAX = 1500

# Appended to a failure a session is deciding what to do about. One line, the same everywhere.
REPORT_HINT = ("If this looks like a Cecelia bug rather than a wrong argument, call report_issue and "
               "tell the user — don't work around it.")

_PATH_RE = re.compile(r"(?:[A-Za-z]:\\|~[/\\]|/)(?:[^\s\"'`<>|,;:()\[\]{}]+[/\\])+[^\s\"'`<>|,;:()\[\]{}]*")


def redact(text: str, names: list[str] | tuple[str, ...] = ()) -> str:
    """`text` safe for a public issue: every absolute path → `<path>`, the user's home and login name →
    `<user>`, and each of `names` (project / image names and uids, set names) → `<name>`."""
    if not text:
        return text
    out = _PATH_RE.sub("<path>", text)
    home = os.path.expanduser("~")
    if home and home not in ("~", "/"):
        out = out.replace(home, "<path>")
    for n in sorted({n for n in names if n and len(n) >= 3}, key=len, reverse=True):
        out = re.sub(re.escape(n), "<name>", out, flags=re.IGNORECASE)
    user = os.environ.get("USER") or os.environ.get("USERNAME") or ""
    if len(user) >= 3:
        out = re.sub(rf"\b{re.escape(user)}\b", "<user>", out)
    return out


def fingerprint(kind: str, tried: str, observed: str, expected: str, fun: str) -> str:
    """Same report, same hash — whitespace and case don't make it new."""
    norm = lambda s: " ".join((s or "").lower().split())   # noqa: E731
    return hashlib.sha1("\n".join(map(norm, (kind, tried, observed, expected, fun))).encode()).hexdigest()[:12]


def _cut(text: str, limit: int) -> str:
    if len(text) <= limit:
        return text
    return f"… (first {len(text) - limit} characters cut)\n" + text[-limit:]


def _what_happened(r: dict) -> str:
    lines = [f"**Kind:** {r['kind']}", ""]
    if r.get("fun"):
        lines += [f"**Step:** `{r['fun']}`", ""]
    lines += ["**What was tried**", r["tried"], "", "**Observed**", r["observed"], "",
              "**Expected**", r["expected"], "",
              f"**Workaround:** {r['workaround']}" + (f" — {r['workaround_note']}" if r.get("workaround_note") else "")]
    return "\n".join(lines)


def draft_markdown(r: dict) -> str:
    """The full, unredacted draft saved on this machine."""
    parts = [f"# {r['title']}", "", f"Drafted by a Claude session, {r['at']} · report {r['id']}", "",
             _what_happened(r), "",
             f"**Cecelia:** {r.get('version') or 'unknown'} · **OS:** {r.get('platform') or 'unknown'}"]
    if r.get("params") is not None:
        parts += ["", "**Last run's settings**", "```json", json.dumps(r["params"], indent=1), "```"]
    if r.get("log"):
        parts += ["", "**Log tail**", "```", r["log"], "```"]
    if r.get("patch"):
        parts += ["", "**Suggested patch** (a hypothesis — verify before applying)", "```diff", r["patch"], "```"]
    return "\n".join(parts) + "\n"


def pixi_platform() -> str:
    """This machine as a pixi platform label (`linux-64`, `osx-arm64`, `win-64`) — the same words the
    app's "Something wrong?" report uses, from `_current_pixi_platform` (api/src/system_api.jl)."""
    import platform
    import sys
    if sys.platform == "darwin":
        return "osx-arm64" if platform.machine() in ("arm64", "aarch64") else "osx-64"
    if sys.platform.startswith("win"):
        return "win-64"
    if sys.platform.startswith("linux"):
        return "linux-64"
    return "unknown"


def _os_label(platform: str) -> str:
    """The bug form's OS dropdown word — `issueOsLabel` (frontend/src/lib/links.ts)."""
    p = platform or ""
    return "Linux" if p.startswith("linux") else "macOS" if p.startswith("osx") else \
        "Windows" if p.startswith("win") else ""


def issue_url(r: dict, names: list[str]) -> str:
    """A prefilled link to the public bug form — redacted, and cut to fit under `URL_MAX`. Drops the
    patch first, then the settings, then shortens the log, so the description always survives."""
    red = lambda s: redact(s, names)   # noqa: E731
    what = red(_what_happened(r)) + f"\n\n---\nDrafted by a Claude session (report {r['id']})."
    params = red(json.dumps(r["params"], indent=1)) if r.get("params") is not None else ""
    log = red(r.get("log") or "")
    patch = red(r.get("patch") or "")
    os_label = _os_label(r.get("platform") or "")

    def build(params: str, log: str, patch: str) -> str:
        body = what
        if params:
            body += f"\n\nLast run's settings:\n```json\n{_cut(params, PARAMS_MAX)}\n```"
        if patch:
            body += f"\n\nSuggested patch:\n```diff\n{patch}\n```"
        q = {"template": "bug_report.yml", "title": red(r["title"]), "what-happened": body}
        if r.get("version"):
            q["version"] = r["version"]
        if os_label:
            q["os"] = os_label
        if log:
            q["logs"] = log
        return f"{REPO_URL}/issues/new?{urlencode(q)}"

    log = _cut(log, LOG_TAIL_MAX)
    url = build(params, log, patch)
    if len(url) > URL_MAX and patch:
        url = build(params, log, "")
    if len(url) > URL_MAX and params:
        url = build("", log, "")
    while len(url) > URL_MAX and log:
        log = _cut(log, len(log) // 2) if len(log) > 200 else ""
        url = build("", log, "")
    return url


def save_draft(directory: str | os.PathLike, r: dict) -> str:
    """Write the draft as `<directory>/<date>-<id>.md`; returns its path."""
    d = Path(directory)
    d.mkdir(parents=True, exist_ok=True)
    path = d / f"{time.strftime('%Y-%m-%d')}-{r['id']}.md"
    with write_atomic(path) as f:
        f.write(draft_markdown(r))
    return str(path)


def failure_text(e: BaseException) -> str:
    """What a tool's failure says to the model, from the exception that ended it.

    The hint goes only where the fault is likely Cecelia's: a server error (HTTP 5xx) or a crash in
    this server's own code. A refusal (4xx: a name taken, no such population), a wrong argument
    (`ValueError`), or an unreachable app is the session's to fix, and hinting there would teach it
    to report its own mistakes."""
    from cecelia_mcp.client import ApiError, DisallowedRoute
    if isinstance(e, ApiError):
        return f"{e} {REPORT_HINT}" if e.status >= 500 else str(e)
    if isinstance(e, (DisallowedRoute, ValueError)):
        return str(e)
    return f"internal error — {type(e).__name__}: {e}. {REPORT_HINT}"


FAILED_RUN_HINT = "A run failed: get_task_log has its error. " + REPORT_HINT


def note_failed_runs(out: dict, failed: bool) -> dict:
    """`out` with `onFailure` set when a run in it failed — a failed task is the other place a session
    decides whether to work around Cecelia."""
    return {**out, "onFailure": FAILED_RUN_HINT} if failed else out
