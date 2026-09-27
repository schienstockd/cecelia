"""Citation-currency check — a mechanical third recital step.

Governance docs sometimes cite specific code artifacts as their enforcement mechanism
("Enforced by `python/cecelia/tests/test_zarr_access_convention.py`", "ratchets it to
zero via `frontend/src/utils/setupOrder.ts`", etc.). When the cited code changes but the
citing doc doesn't, the doc's claim about the code silently goes stale — a fourth drift
category beyond canonical→projection, review-fanout, and reviewer-effectiveness.

This check is **mechanical, not a reviewer subagent** — a decision from the governance-layer
audit (Item 4), gated on Item 2's headroom verdict (no third recital-time subagent while the
two existing reviewers are uncalibrated). Design in
`docs/archive/governance_layer_audit.md` §Item 4.

Advisory only; never blocks. Sub-second in prod (~40 docs, one `git ls-files`, per-file
regex scan). No cache — the citation list rebuilds every run because a cached list is
exactly the recursive drift the audit named as out of scope.
"""
from __future__ import annotations

import re
import subprocess
import time
import typing as _t
from pathlib import Path

from .log import append_event


#: Docs that state enforcement rules. We scan these for backticked code citations.
#: `docs/archive/**` is frozen by policy (stale claims there are expected, not warnings).
#: `docs/todo/**` is in-flight; its citations are aspirational, not enforcement claims.
_SCAN_ROOTS = ("docs", "CLAUDE.md", "INVENTORY.md")
_SKIP_PREFIXES = ("docs/archive/", "docs/todo/")

#: A line qualifies as an enforcement-claim line if it matches any of these phrases. The
#: extraction then harvests backticked code tokens on that line. Case-insensitive. The
#: optional `\*\*` around "enforced" catches the markdown-bold form used in
#: `docs/MAINTAINABILITY.md` — `**Enforced** by ...`.
_ENFORCEMENT_PATTERNS = re.compile(
    r"(?:\*\*)?enforced(?:\*\*)?\s+by|ratchets? .* to zero|pre-commit hook",
    re.IGNORECASE,
)

#: Backticked tokens that look like code files. Restricted to a specific extension whitelist
#: so we don't chase testset names (`zarr-access ratchet`) or bare symbol names
#: (`misplacedTooltips`) in v1. Coverage caveat in the module docstring header.
_CODE_TOKEN = re.compile(r"`([A-Za-z0-9_./\-]+\.(?:py|jl|ts|tsx|js|mjs|jsx))`")

#: `diff --git a/<path> b/<path>` — extract touched files from the staged diff string. Also
#: catches `+++ b/<path>` as a fallback for renames / edge cases (matches either).
_DIFF_FILE = re.compile(r"^(?:diff --git a/|\+\+\+ b/)([^\s]+)", re.MULTILINE)


class Citation(_t.NamedTuple):
    citing_doc: str  # repo-relative path
    line: int  # 1-indexed


def _list_repo_files(repo_root: Path) -> list[str]:
    """Enumerate tracked files via `git ls-files`. Returns repo-relative POSIX paths."""
    result = subprocess.run(
        ["git", "ls-files"],
        cwd=repo_root,
        capture_output=True,
        text=True,
        check=False,
        encoding="utf-8",
    )
    if result.returncode != 0:
        return []
    return [ln.strip() for ln in result.stdout.splitlines() if ln.strip()]


def _iter_scan_paths(repo_root: Path, repo_files: list[str]) -> _t.Iterator[str]:
    """Yield repo-relative paths of docs to scan."""
    for rel in repo_files:
        if any(rel.startswith(skip) for skip in _SKIP_PREFIXES):
            continue
        if rel == "CLAUDE.md" or rel == "INVENTORY.md":
            yield rel
        elif rel.startswith("docs/") and rel.endswith(".md"):
            yield rel


def _resolve_token(token: str, repo_root: Path, index_by_basename: dict[str, list[str]]) -> str | None:
    """Resolve a backticked code token to a repo-relative path, or None if ambiguous / missing.

    - Full path (`/` present): accept iff the file exists on disk.
    - Bare basename: accept iff exactly one tracked file has that basename.
    """
    if "/" in token:
        return token if (repo_root / token).exists() else None
    hits = index_by_basename.get(token, [])
    return hits[0] if len(hits) == 1 else None


def build_citation_index(
    repo_root: Path,
    *,
    repo_files: list[str] | None = None,
) -> dict[str, list[Citation]]:
    """Scan governance docs and return `{cited_repo_path: [Citation, ...]}`.

    - `repo_files`: optional pre-computed list (test seam). Defaults to `git ls-files` output.
    """
    files = repo_files if repo_files is not None else _list_repo_files(repo_root)
    index_by_basename: dict[str, list[str]] = {}
    for rel in files:
        index_by_basename.setdefault(rel.rsplit("/", 1)[-1], []).append(rel)

    citations: dict[str, list[Citation]] = {}
    for doc_rel in _iter_scan_paths(repo_root, files):
        doc_path = repo_root / doc_rel
        try:
            text = doc_path.read_text(encoding="utf-8")
        except (OSError, UnicodeDecodeError):
            continue
        for lineno, line in enumerate(text.splitlines(), start=1):
            if not _ENFORCEMENT_PATTERNS.search(line):
                continue
            for match in _CODE_TOKEN.finditer(line):
                token = match.group(1)
                resolved = _resolve_token(token, repo_root, index_by_basename)
                if resolved is None:
                    continue
                citations.setdefault(resolved, []).append(Citation(doc_rel, lineno))
    return citations


def touched_files_from_diff(diff: str) -> set[str]:
    """Extract repo-relative paths from a staged-diff string."""
    return {m.group(1) for m in _DIFF_FILE.finditer(diff)}


def find_stale_citations(
    index: dict[str, list[Citation]],
    touched: set[str],
) -> list[tuple[str, list[Citation]]]:
    """Return `[(cited_file, [citations, ...]), ...]` for touched files where NO citing doc is
    also touched. Ordered by cited-file path for stable output."""
    stale: list[tuple[str, list[Citation]]] = []
    for cited in sorted(touched & set(index)):
        cites = index[cited]
        citing_docs = {c.citing_doc for c in cites}
        if citing_docs.isdisjoint(touched):
            stale.append((cited, cites))
    return stale


def _format_citations(cites: _t.Sequence[Citation]) -> str:
    """Comma-joined `path:line` pairs — no fancy grouping, just readable."""
    return ", ".join(f"`{c.citing_doc}:{c.line}`" for c in cites)


def format_section(
    stale: _t.Sequence[tuple[str, _t.Sequence[Citation]]],
    *,
    touched_count: int,
    indexed_count: int,
    title: str = "Citation-currency check",
) -> str:
    """Format as the standard reservations section (title header + evidence + tail line).

    Tail-line vocabulary mirrors the fanout/convention pattern so
    `.claude/hooks/check_commit_recital.py`'s "did the heading print" grep works with the
    same regex shape (`_<Title>: <verdict>_`)."""
    if indexed_count == 0:
        return f"_{title}: skipped — no citations indexed_"
    if touched_count == 0:
        return f"_{title}: skipped — no code changes_"
    if not stale:
        return f"_{title}: run — no stale citations_"

    lines = [f"_{title} (evidence):_", ""]
    for cited, cites in stale:
        lines.append(
            f"- `{cited}` cited from {_format_citations(cites)} — none touched. "
            f"Skim whether the docs still describe what the code enforces."
        )
    lines.append("")
    lines.append(f"_{title}: run_")
    return "\n".join(lines)


def run_citation_check(
    diff: str,
    *,
    repo_root: Path | None = None,
    pr: str | None = None,
    repo_files: list[str] | None = None,
) -> str:
    """Full pipeline: build index, extract touched files, find stales, format section, emit event.

    Returns the markdown section (safe to append to the recital body).
    """
    repo_root = repo_root or _default_repo_root()
    start = time.monotonic()
    index = build_citation_index(repo_root, repo_files=repo_files)
    touched = touched_files_from_diff(diff)
    stale = find_stale_citations(index, touched)
    duration = time.monotonic() - start

    append_event(
        "citation_currency_run",
        {
            "citations_indexed": len(index),
            "staged_files_checked": len(touched),
            "warnings_emitted": len(stale),
            "duration_s": round(duration, 3),
        },
        pr=pr,
    )
    return format_section(stale, touched_count=len(touched), indexed_count=len(index))


def _default_repo_root() -> Path:
    """`git rev-parse --show-toplevel`, or cwd as a last resort."""
    result = subprocess.run(
        ["git", "rev-parse", "--show-toplevel"],
        capture_output=True,
        text=True,
        check=False,
        encoding="utf-8",
    )
    if result.returncode == 0 and result.stdout.strip():
        return Path(result.stdout.strip())
    return Path.cwd()
