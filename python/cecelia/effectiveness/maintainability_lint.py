"""Maintainability lint — mechanical checks for the `docs/MAINTAINABILITY.md` rules a regex can see.

Plan: `docs/todo/MAINTAINABILITY_ENFORCEMENT_PLAN.md`. Advisory only; never blocks. Every trigger
looks at what the diff DOES, never at what a touched file already is, so old debt stays quiet:

- **Size** — a task file (`app/src/tasks/<category>/…`) crosses 200 lines, or grows ≥20 past it.
- **Incident history** — an added comment line carrying a dataset uid, a dated authorship, an
  internal phase code, or a commit SHA. A comment defending an adjacent numeric constant is
  measurement provenance, not history, and is skipped.

A `MAINT-EXEMPT: <reason>` comment on the line (or the one above) silences an incident finding —
same shape as `INVENTORY-EXEMPT:` / `COHORT-EXEMPT:` / `DASK-OK:`.

Each sub-check earned its place on a replay over merged PRs before it was wired in; the hit rates,
and the sub-checks the replay cut, are in the plan → *Replay results*.
"""
from __future__ import annotations

import re
import time
import typing as _t
from dataclasses import dataclass

from .git_context import DiffLine, FileDiff, parse_diff
from .git_context import git_output as _git_output
from .log import append_event

_TITLE = "Maintainability lint"

_CODE_EXT = (".py", ".jl", ".ts", ".vue", ".js")
_HASH_COMMENT_EXT = (".py", ".jl")

#: Tests may carry dataset uids and dated fixtures on purpose.
_NOT_SOURCE = re.compile(r"(\.test\.|\.spec\.|/tests?/|/test_[^/]*$|_test\.jl$)")

_TASKS_ROOT = "app/src/tasks/"
#: Framework, not task files: the scheduler / chain / task-spec machinery and the test tasks.
_TASK_FRAMEWORK_DIRS = ("chain/", "scheduler/", "task/", "testTasks/")
SIZE_LIMIT = 200
SIZE_GROWTH = 20

_EXEMPT = re.compile(r"MAINT-EXEMPT:\s*\S")

#: Incident-history patterns, one name each so the replay can report (and cut) them separately.
_INCIDENT_PATTERNS: dict[str, re.Pattern[str]] = {
    "dated_authorship": re.compile(r"\b[A-Z][a-z]+,? \d{4}-\d{2}-\d{2}\b"),
    "phase_code": re.compile(r"\b(?:P\d{1,2} slice \d+|Phase [A-Z]\d*,? [A-Z]\d+)\b"),
    "commit_sha": re.compile(r"\bcommit [0-9a-f]{7,40}\b"),
}
#: A six-character project / image uid (real ones the replay found are pinned in
#: `test_maintainability_lint.py` → `DatasetUidTest.REAL`). Any 6-char alnum token is far too broad,
#: so require both cases and either a digit or ≥3 case flips (random uids have ~4; `setAll` /
#: `useApi` have 1–2). Then drop the word shapes the replay found: camel-case words whose every
#: segment has a vowel (`GitHub`, `SetBar`, `UiMark`; a random uid has a vowel-less segment),
#: `show3D`, `cpSAM2`.
_UID_TOKEN = re.compile(r"(?<![\w./-])[A-Za-z0-9]{6}(?![\w/-])")
_KNOWN_WORDS = frozenset({"UInt16", "UInt32", "UInt64", "Int128", "Float3"})
_CAMEL_SEGMENT = re.compile(r"[A-Z]?[a-z]+")
_WORD_WITH_DIGITS = re.compile(r"^[a-z]+(?:\d[A-Z]|[A-Z]{2,}\d?)$")

#: A numeric constant a provenance comment may defend: `const X = 0.12`, `X = 0.12`.
_NUMERIC_CONST = re.compile(r"^\s*(?:export\s+)?(?:const\s+)?[A-Za-z_][\w]*\s*(?::[^=]+)?=\s*-?[\d.]")


@dataclass(frozen=True)
class Finding:
    check: str      # "size" / "incident:<pattern>"
    path: str
    line: int | None
    detail: str


def _is_source(path: str) -> bool:
    return path.endswith(_CODE_EXT) and not _NOT_SOURCE.search(path)


def comment_text(path: str, line: str) -> str | None:
    """The comment part of one source line, or None. Line-level only: full-line comments plus a
    trailing `# …` / `// …` outside an obvious string. Docstring bodies aren't seen."""
    s = line.strip()
    if path.endswith(_HASH_COMMENT_EXT):
        if s.startswith("#"):
            return s.lstrip("#").strip()
        m = re.search(r"\s#\s(.*)$", line)
        return m.group(1) if m and line[: m.start()].count('"') % 2 == 0 else None
    if s.startswith(("//", "*", "/*")):
        return s.lstrip("/*").strip()
    m = re.search(r"\s//\s(.*)$", line)
    return m.group(1) if m and line[: m.start()].count("'") % 2 == 0 and "://" not in line else None


def _case_flips(tok: str) -> int:
    letters = [c for c in tok if c.isalpha()]
    return sum(a.isupper() != b.isupper() for a, b in zip(letters, letters[1:]))


def _is_camel_word(tok: str) -> bool:
    segs = _CAMEL_SEGMENT.findall(tok)
    return "".join(segs) == tok and all(re.search(r"[aeiouy]", sg, re.I) for sg in segs)


def looks_like_uid(tok: str) -> bool:
    if tok in _KNOWN_WORDS or not (any(c.islower() for c in tok) and any(c.isupper() for c in tok)):
        return False
    if any(c.isdigit() for c in tok):
        return not _WORD_WITH_DIGITS.match(tok)
    return _case_flips(tok) >= 3 and not _is_camel_word(tok)


def _defends_constant(hunk: list[DiffLine], i: int) -> bool:
    """True if the comment at `hunk[i]` sits on, or up to two lines above, a numeric constant —
    measurement provenance, protected by `MAINTAINABILITY.md` → *No incident history*."""
    return any(_NUMERIC_CONST.match(hunk[j].text) for j in range(i, min(i + 3, len(hunk)))
               if hunk[j].kind != "-")


def _exempt(hunk: list[DiffLine], i: int) -> bool:
    return any(_EXEMPT.search(hunk[j].text) for j in (i - 1, i) if j >= 0 and hunk[j].kind != "-")


def incident_findings(fd: FileDiff) -> list[Finding]:
    """Incident history in the comment lines this diff adds to one source file."""
    if not _is_source(fd.path):
        return []
    out: list[Finding] = []
    for hunk in fd.hunks:
        for i, ln in enumerate(hunk):
            if ln.kind != "+" or (text := comment_text(fd.path, ln.text)) is None:
                continue
            if _exempt(hunk, i) or _defends_constant(hunk, i):
                continue
            for name, pat in _INCIDENT_PATTERNS.items():
                if m := pat.search(text):
                    out.append(Finding(f"incident:{name}", fd.path, ln.lineno, m.group(0)))
            for tok in _UID_TOKEN.findall(text):
                if looks_like_uid(tok):
                    out.append(Finding("incident:dataset_uid", fd.path, ln.lineno, tok))
    return out


def is_task_file(path: str) -> bool:
    """A file under a task category dir (`app/src/tasks/<category>/…`); top-level framework files
    (`task.jl`, `chain.jl`, …) and the framework dirs don't count."""
    if not (path.startswith(_TASKS_ROOT) and path.endswith(".jl")):
        return False
    rel = path[len(_TASKS_ROOT):]
    return "/" in rel and not rel.startswith(_TASK_FRAMEWORK_DIRS)


def size_finding(fd: FileDiff, after_lines: int) -> Finding | None:
    """A task file this diff pushes past `SIZE_LIMIT`, or grows by ≥`SIZE_GROWTH` beyond it."""
    if not is_task_file(fd.path) or fd.is_deleted:
        return None
    before = after_lines - len(fd.added) + len(fd.removed)
    if after_lines <= SIZE_LIMIT:
        return None
    if before <= SIZE_LIMIT:
        return Finding("size", fd.path, None, f"{before} → {after_lines} lines, crosses {SIZE_LIMIT}")
    if after_lines - before >= SIZE_GROWTH:
        return Finding("size", fd.path, None, f"{before} → {after_lines} lines, +{after_lines - before} past {SIZE_LIMIT}")
    return None


def lint_diff(diff: str, *, after_lines: _t.Callable[[str], int | None]) -> list[Finding]:
    """Every finding for one diff. `after_lines(path)` = the file's line count once the diff is
    applied (None if absent) — a seam so a replay can drive it against historical commits."""
    out: list[Finding] = []
    for fd in parse_diff(diff):
        out += incident_findings(fd)
        if (n := after_lines(fd.path)) is not None and (f := size_finding(fd, n)):
            out.append(f)
    return out


def _staged_lines(path: str) -> int | None:
    """Line count of the staged (index) copy — what the commit will contain, unstaged edits aside."""
    text = _git_output("show", f":{path}")
    return None if text is None else len(text.splitlines())


def format_section(findings: _t.Sequence[Finding], *, files_checked: int) -> str:
    """Standard reservations section — evidence bullets + `_<Title>: <verdict>_` tail."""
    if files_checked == 0:
        return f"_{_TITLE}: skipped — no source files_"
    if not findings:
        return f"_{_TITLE}: run — nothing flagged_"
    lines = [f"_{_TITLE} (evidence):_", ""]
    for f in findings:
        if f.check == "size":
            lines.append(
                f"- `{f.path}` — {f.detail}. Split it along its responsibilities "
                f"(`docs/MAINTAINABILITY.md` → *Split rule*)."
            )
        else:
            kind = f.check.split(":", 1)[1].replace("_", " ")
            lines.append(
                f"- `{f.path}:{f.line}` — comment carries a {kind} (`{f.detail}`). Incident history "
                f"goes in the PR or `CHANGELOG.md`, not source; if it's load-bearing, mark the line "
                f"`MAINT-EXEMPT: <reason>`."
            )
    lines += ["", f"_{_TITLE}: run_"]
    return "\n".join(lines)


def run_maintainability_lint(
    diff: str,
    *,
    after_lines: _t.Callable[[str], int | None] | None = None,
    pr: str | None = None,
    commit: str | None = None,
    branch: str | None = None,
) -> str:
    """Lint the staged diff, emit one `maintainability_lint_run` event, return the markdown section."""
    start = time.monotonic()
    counter = after_lines or _staged_lines
    files_checked = sum(_is_source(fd.path) for fd in parse_diff(diff))
    findings = lint_diff(diff, after_lines=counter) if files_checked else []
    append_event(
        "maintainability_lint_run",
        {
            "files_checked": files_checked,
            "warnings_emitted": len(findings),
            # Which locations, not just how many — so the log can say later whether they were acted on.
            "findings": [{"check": f.check, "path": f.path, "line": f.line, "detail": f.detail}
                         for f in findings],
            "duration_s": round(time.monotonic() - start, 3),
        },
        pr=pr,
        commit=commit,
        branch=branch,
    )
    return format_section(findings, files_checked=files_checked)
