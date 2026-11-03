"""Changelog check — a changed default needs a re-run note (`docs/RELEASING.md` → *Read this first*).

A task's default decides what a fresh run computes, so changing one changes results the next time
anyone re-runs old work, silently. The release notes open with those changes (*Read this first*), but
only if somebody wrote the line. This check warns when the staged diff changes a default and neither
the diff nor an earlier commit on the branch touched `CHANGELOG.md`. Advisory; never blocks.

What counts as a changed default (a removed and an added line in one hunk):
- a task spec (`app/src/tasks/**.json`): a `"default":` value, or a parameter's value inside a group's
  `default` block (any `"<key>": <scalar>` whose key isn't spec metadata like `label` or `max`);
- Python task code (`python/cecelia/**`, `app/src/tasks/**.py`, not tests): the fallback in
  `<params>.get('<key>', <default>)` for the same key.
A new parameter on an existing task is listed too, more softly: before it existed the code ran on
some value, often a library's own implicit default, so its default can move results with no old line
to compare against.
"""
from __future__ import annotations

import fnmatch
import re
import time
import typing as _t
from dataclasses import dataclass

from .git_context import FileDiff, parse_diff
from .git_context import git_output as _git_output
from .log import append_event

_TITLE = "Changelog check"
CHANGELOG = "CHANGELOG.md"
_SPEC_GLOB = "app/src/tasks/*.json"
_PY_GLOBS = ("python/cecelia/*.py", "app/src/tasks/*.py")
_NOT = ("*/test/*", "*/tests/*", "*/test_*.py")
_JSON_DEFAULT = re.compile(r'"default"\s*:\s*([^\s\[{].*?),?\s*$')
#: `"<key>": <scalar>` — a value inside a group's `default` block (one model entry's params).
_SPEC_KEY = re.compile(r'^\s*"key"\s*:\s*"(\w+)"')
_JSON_SCALAR = re.compile(r'^\s*"(\w+)"\s*:\s*([^\s\[{].*?),?\s*$')
#: Spec fields that describe a parameter, not a value a run uses.
_SPEC_META = frozenset({"default", "key", "label", "tip", "title", "description", "type", "min", "max", "step",
                        "vis", "showIf", "options", "unit", "units", "placeholder", "help", "hidden", "order",
                        "name", "fun", "category", "group", "accepts", "advisor", "advanced", "section", "icon",
                        "useWhen", "notWhen", "env", "hpc"})
_PY_FALLBACK = re.compile(r"""\.get\(\s*['"](\w+)['"]\s*,\s*([^)]+?)\s*\)""")


@dataclass(frozen=True)
class Change:
    path: str
    line: int
    old: str
    new: str
    key: str = ""
    new_param: bool = False


def _is(path: str, globs: _t.Sequence[str]) -> bool:
    return any(fnmatch.fnmatch(path, g) for g in globs) and not any(fnmatch.fnmatch(path, x) for x in _NOT)


def _spec_changes(fd: FileDiff) -> list[Change]:
    out = []
    removed_keys = {m.group(1) for h in fd.hunks for ln in h if ln.kind == "-" and (m := _SPEC_KEY.match(ln.text))}
    for hunk in fd.hunks:
        for ln in hunk:   # a parameter this spec didn't have: what did the code do before it?
            m = _SPEC_KEY.match(ln.text) if ln.kind == "+" else None
            if m and m.group(1) not in removed_keys:
                out.append(Change(fd.path, ln.lineno or 0, "", "", m.group(1), new_param=True))
    for hunk in fd.hunks:
        was = {m.group(1): m.group(2) for ln in hunk if ln.kind == "-" and (m := _JSON_SCALAR.match(ln.text))
               and m.group(1) not in _SPEC_META}
        for ln in (ln for ln in hunk if ln.kind == "+"):
            m = _JSON_SCALAR.match(ln.text)
            if m and m.group(1) in was and was[m.group(1)] != m.group(2):
                out.append(Change(fd.path, ln.lineno or 0, was[m.group(1)], m.group(2), m.group(1)))
        old = [m.group(1) for ln in hunk if ln.kind == "-" and (m := _JSON_DEFAULT.search(ln.text))]
        new = [(ln, m.group(1)) for ln in hunk if ln.kind == "+" and (m := _JSON_DEFAULT.search(ln.text))]
        for (ln, value), was in zip(new, old):   # in hunk order: the n-th removed default became the n-th added
            if value != was:
                out.append(Change(fd.path, ln.lineno or 0, was, value))
    return out


def _py_changes(fd: FileDiff) -> list[Change]:
    out = []
    for hunk in fd.hunks:
        old = {m.group(1): m.group(2) for ln in hunk if ln.kind == "-" for m in _PY_FALLBACK.finditer(ln.text)}
        for ln in (ln for ln in hunk if ln.kind == "+"):
            for m in _PY_FALLBACK.finditer(ln.text):
                key, value = m.group(1), m.group(2)
                if key in old and old[key] != value:
                    out.append(Change(fd.path, ln.lineno or 0, old[key], value, key))
    return out


def default_changes(diff: str) -> list[Change]:
    """Every default the diff changes, in diff order."""
    out: list[Change] = []
    for fd in parse_diff(diff):
        if fd.is_new or fd.is_deleted:
            continue
        if _is(fd.path, (_SPEC_GLOB,)):
            out += _spec_changes(fd)
        elif _is(fd.path, _PY_GLOBS):
            out += _py_changes(fd)
    return out


def _branch_touched_changelog() -> bool:
    """An earlier commit on this branch (since it left `origin/main`) touched `CHANGELOG.md`."""
    base = _git_output("merge-base", "HEAD", "origin/main")
    if not base:
        return False
    return bool((_git_output("diff", "--name-only", base.strip(), "HEAD", "--", CHANGELOG) or "").strip())


def format_section(changes: _t.Sequence[Change], *, noted: bool) -> str:
    if not changes:
        return f"_{_TITLE}: skipped — no default changed_"
    if noted:
        return f"_{_TITLE}: run — {len(changes)} default change(s), `{CHANGELOG}` updated_"
    lines = [f"_{_TITLE} (evidence):_", ""]
    for c in changes:
        what = f"`{c.key}` " if c.key else ""
        lines.append(f"- `{c.path}:{c.line}` — new parameter {what}on an existing task: is its default what ran "
                     f"before it existed?" if c.new_param else f"- `{c.path}:{c.line}` — default {what}`{c.old}` → `{c.new}`.")
    lines += ["", f"A changed default changes results when old work is re-run. If this one does, add a line "
                  f"under *Read this first* in `{CHANGELOG}` → Unreleased (`docs/RELEASING.md`).", "",
              f"_{_TITLE}: run_"]
    return "\n".join(lines)


def run_changelog_check(
    diff: str,
    *,
    branch_noted: _t.Callable[[], bool] | None = None,
    pr: str | None = None,
    commit: str | None = None,
    branch: str | None = None,
) -> str:
    """Check the staged diff, emit one `changelog_check_run` event, return the markdown section."""
    start = time.monotonic()
    changes = default_changes(diff)
    noted = bool(changes) and (any(fd.path == CHANGELOG for fd in parse_diff(diff))
                               or (branch_noted or _branch_touched_changelog)())
    warned = [] if noted else changes
    append_event(
        "changelog_check_run",
        {"defaults_changed": len(changes), "warnings_emitted": len(warned), "changelog_noted": noted,
         "findings": [{"path": c.path, "line": c.line, "old": c.old, "new": c.new, "key": c.key,
                       **({"new_param": True} if c.new_param else {})} for c in warned],
         "duration_s": round(time.monotonic() - start, 3)},
        pr=pr, commit=commit, branch=branch,
    )
    return format_section(changes, noted=noted)
