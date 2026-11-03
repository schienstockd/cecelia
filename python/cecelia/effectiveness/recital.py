"""Recital orchestrator — the single entry point for pre-commit reviewer discipline.

Spawns the reviewers (fanout audit + convention check, plus the result review when the diff touches
a result path) via `claude -p` subprocess, runs the two
mechanical checks (inventory coverage, maintainability lint), emits log
events atomically with the runs, and produces formatted recital text ready to paste into the
commit message. Replaces the manual "agent spawns subagents, agent formats, agent emits"
protocol previously described in `CLAUDE.md` — by centralising the whole thing in code, none
of the steps can be silently skipped in autonomous mode. Documented failure that motivated
this: four consecutive reviewer runs in one session where the parent agent skipped the log
emission step every time, leaving zero rows in `~/.cecelia-effectiveness/events.jsonl`.

Usage (from an agent, via the CLI wrapper):

    pixi run recital

which reads `git diff --staged`, spawns the reviewers, emits `_run` events and one
`_finding` event per outcome-tagged reviewer bullet, and prints the recital body — with a
`[slug]` prefixed to each outcome-tagged finding line so the author can paste the slug into
the commit message inside a `[slug: outcome]` pair. Full design in
`docs/todo/FINDINGS_EMISSION_PLAN.md`.

Test seam: `run_recital(diff, claude_runner=fake)` bypasses real claude spawning. Same pattern
as `api/test/suite/kiwi_turn.jl`'s fake engine.

Not built here (deferred to later phases of FINDINGS_EMISSION_PLAN.md):
- Escape-valve detection (docs-only / no-additions / tests-only). v1 always spawns both;
  reviewers' own short-circuit handles empty cases.
- Hook parses `[slug: outcome]` pairs and writes `_finding_resolved` rows (P3).
- Rollup renders per-finding rows with PR links (P2).
"""
from __future__ import annotations

import fnmatch
import hashlib
import json
import os
import pathlib
import re
import shutil
import subprocess
import sys
import time
import typing as _t

from .inventory_coverage import run_inventory_check
from .maintainability_lint import run_maintainability_lint
from .log import append_event

#: Path to each reviewer's spec doc. `claude -p` reads it itself — the reviewer prompt is
#: the single source of truth.
_FANOUT_DOC = "docs/ai-assist/FANOUT_AUDIT.md"
_CONVENTION_DOC = "docs/ai-assist/CONVENTION_CHECK.md"
_RESULT_DOC = "docs/ai-assist/RESULT_REVIEW.md"


#: Heads a reviewer's section when it errored: the recital still prints, stamp included, but that
#: reviewer didn't run (`judge-review --auto` stops on it rather than commit unreviewed).
ERROR_MARKER = "RECITAL SCRIPT ERROR"

#: One reviewer's error block as `_run_reviewer` writes it: the marker line, then its message up to
#: the next italic line (the section's `_<title>: not run …_` tail) or the end.
_ERROR_BLOCK = re.compile(r"> \*\*" + re.escape(ERROR_MARKER) + r"\*\* — (.*?)(?=\n\n_|\Z)", re.S)


def reviewer_errors(body: str) -> list[str]:
    """The reviewer error messages in a recital body, and nothing else: the reviewers' findings sit
    in the same body and may quote anything (a limit's wording included)."""
    return _ERROR_BLOCK.findall(body)


class RecitalError(RuntimeError):
    """Raised when a reviewer subprocess fails hard (non-zero exit, timeout, missing CLI).

    Emission still happens in the finally-block above the raise, so the log records the
    attempt even when the reviewer failed.
    """


#: The outcome-tag-requiring markers per mechanism, per CLAUDE.md → Git & commits (only these
#: produce `_finding` rows, keeping the pending↔resolution contract 1:1 with the hook's tag-count
#: check). The hook's `_FINDING_MARKERS` is built from this.
MARKERS: dict[str, tuple[str, ...]] = {
    "fanout": ("confirmed",),
    "convention": ("should reuse", "wrong home"),
    "result": ("reproduced",),
}

#: Advisory markers — no outcome tag required and no `_finding` row, but still a finding the
#: reader should see: each becomes an `_advisory` row the console lists.
ADVISORY_MARKERS: dict[str, tuple[str, ...]] = {
    "fanout": ("plausible",),
    "convention": ("potential duplicate",),
    "result": ("inferred", "display"),
}

#: Advisory markers that still get a printed slug, so the author MAY tag one fixed. The weekly bug
#: sweep re-checks every untagged `plausible` note (the same slug is its key); about half are fixed
#: at commit, and a `[slug: fixed_pre_commit]` lets it skip those. Optional: the hook never asks.
TAGGABLE_ADVISORY: dict[str, tuple[str, ...]] = {"fanout": ("plausible",), "convention": (), "result": ("inferred",)}

_OPTIONAL_TAG_NOTE = ("_{Marker} notes need no tag. If you fixed one, tag it `[<slug>: fixed_pre_commit]` "
                      "so the weekly judge doesn't re-check it._")

#: Bullet-line grammar the reviewer prompts ask for:
#:   `- **file:LINE** — <prose> [**marker**]`
#: Reviewers drift from it in practice — line ranges (`:245-246`), line lists (`:11, :309`,
#: `:44,49`), backtick-wrapped paths, `**:` instead of `** —`. A strict regex silently dropped
#: every finding from 2026-09-28 on (11 of 29 across session history), so the parser is
#: tolerant: ANY bullet line carrying the marker is a finding. The location is best-effort —
#: first `file:LINE` wins, so `foo.jl:42` slugs the same with or without a trailing range.
#: A finding slug as `_slug` writes it — the one grammar recital and the commit hook match on.
SLUG_PATTERN = r"(?:fanout|conv|result)-[0-9a-f]{8}"
_BULLET_RE = re.compile(rf"^[ \t]*[-*][ \t]+(?!\[{SLUG_PATTERN}\] )(.+)$", re.MULTILINE)
_LEAD_LOCATION_RE = re.compile(
    r"^\*\*`?([^*:`\s]+):(\d+)[^*]*?`?\*\*\s*[—–:-]?\s*(.*)$"
)
_ANY_LOCATION_RE = re.compile(r"`?([^\s*:`(),]+\.[A-Za-z0-9]+):(\d+)")


def _marker_tag_re(marker: str) -> re.Pattern:
    """The bracketed marker tag, tolerating trailing text inside the brackets — reviewers write
    `[**confirmed**]` but also `[**confirmed**, no action]` (session 290e2f6e, 2026-09-30: the
    exact-match parser dropped it, no slug, no log row, no warning)."""
    return re.compile(rf"\[\*\*{re.escape(marker)}\*\*[^\]\n]*\]")


#: Inline code span. A marker QUOTED in backticks is prose about the grammar, not a verdict — a
#: reviewer writing "counts `[**confirmed**, …]` the same way" became a finding (2026-09-30).
_CODE_SPAN_RE = re.compile(r"`[^`\n]*`")


def outside_code(text: str) -> str:
    """`text` with inline code spans blanked to spaces — same length, so match offsets still
    index the original. Public: the commit hook counts markers the same way (a marker quoted in
    backticks is prose, not a verdict, in a commit message too)."""
    return _CODE_SPAN_RE.sub(lambda m: " " * len(m.group(0)), text)


def _marker_bold_re(marker: str) -> re.Pattern:
    """Any bold marker, bracketed or not — the tripwire's count, so an unforeseen shape still
    surfaces as a PARSE WARNING instead of vanishing."""
    return re.compile(rf"\*\*{re.escape(marker)}\*\*")


class Finding(_t.NamedTuple):
    """A single outcome-tag-requiring finding parsed from a reviewer's output."""

    mechanism: str  # "fanout" | "convention" | "result"
    file: str
    line: int
    desc: str
    marker: str  # the literal marker text (e.g. "confirmed" / "should reuse")
    slug: str  # deterministic id — see `_slug`


def _slug(mechanism: str, file: str, line: int, marker: str, desc: str = "") -> str:
    """Deterministic short id for a finding, per Decision 3 of FINDINGS_EMISSION_PLAN.md.

    Same (mechanism, file, line, marker) → same slug across runs, so the author can quote a
    stale slug from a re-run of the recital without breaking correlation. sha1 truncated to
    8 hex chars — collision-safe at this cardinality (findings-per-PR ~O(10)); prefixed with
    a short mechanism tag for human readability in commit messages. `desc` joins the key only
    when no location parsed (`line == 0`), so location-less findings don't all collide.
    """
    key = f"{mechanism}|{file}|{line}|{marker}"
    if line == 0:
        key += f"|{desc}"
    digest = hashlib.sha1(key.encode("utf-8")).hexdigest()[:8]
    prefix = {"fanout": "fanout", "convention": "conv"}.get(mechanism, mechanism)
    return f"{prefix}-{digest}"


def _parse_bullet(body: str, mechanism: str, markers: _t.Sequence[str] | None = None) -> Finding | None:
    """Parse one bullet body (text after `- `); None if it carries none of `markers` (default:
    the mechanism's outcome markers). A bullet names one verdict, so the first marker wins."""
    for marker in (MARKERS[mechanism] if markers is None else markers):
        tags = list(_marker_tag_re(marker).finditer(outside_code(body)))
        if tags:
            break
    else:
        return None
    text = body
    for t in reversed(tags):  # cut only the real tags; a quoted one stays in the description
        text = text[:t.start()] + " " + text[t.end():]
    text = text.strip()
    m = _LEAD_LOCATION_RE.match(text)
    if m:
        file, line, desc = m.group(1), int(m.group(2)), m.group(3).strip()
    else:
        loc = _ANY_LOCATION_RE.search(text)
        file, line = (loc.group(1), int(loc.group(2))) if loc else ("?", 0)
        desc = text
    return Finding(
        mechanism=mechanism, file=file, line=line, desc=desc, marker=marker,
        slug=_slug(mechanism, file, line, marker, desc),
    )


def _parse_findings(output: str, mechanism: str,
                    markers: _t.Sequence[str] | None = None) -> list[Finding]:
    """Extract findings from a reviewer's raw output — outcome-tag-requiring ones by default,
    or those tagged with `markers` (e.g. `ADVISORY_MARKERS[mechanism]`).

    Every bullet line carrying one of the markers is one finding (see `_BULLET_RE`);
    commentary and short-circuits carry no marker and are ignored. Order-preserving.
    """
    findings = []
    for m in _BULLET_RE.finditer(output):
        f = _parse_bullet(m.group(1), mechanism, markers)
        if f is not None:
            findings.append(f)
    return findings


def _inject_slugs(output: str, mechanism: str) -> str:
    """Prefix each finding bullet — and each `TAGGABLE_ADVISORY` one — with its slug, so the author
    can copy it verbatim into a `[slug: outcome]` pair. The reviewer's own text is kept as-is (ranges
    and all). Bullets already carrying a slug are skipped by `_BULLET_RE`."""
    markers = (*MARKERS[mechanism], *TAGGABLE_ADVISORY[mechanism])

    def _sub(m: re.Match) -> str:
        f = _parse_bullet(m.group(1), mechanism, markers)
        return m.group(0) if f is None else f"- [{f.slug}] {m.group(1)}"

    return _BULLET_RE.sub(_sub, output)


def _unparsed_marker_count(output: str, findings: _t.Sequence[Finding],
                           markers: _t.Sequence[str]) -> int:
    """Bold `markers` occurrences that did NOT become a finding — off a bullet line, or in a
    shape the tag pattern doesn't know. Tripwire for the next grammar drift: surfaced in the
    recital body instead of dropping silently."""
    bold = sum(len(_marker_bold_re(mk).findall(outside_code(output))) for mk in markers)
    return bold - len(findings)


from .claude_cli import MEASURED_MODEL, RateLimited, failure_text, read_result, resolve_claude_bin  # noqa: E402
from .claude_cli import models as _models  # noqa: E402
from .claude_cli import tokens as _tokens  # noqa: E402
from .git_context import current_branch as _current_branch  # noqa: E402
from .git_context import current_head_sha as _current_head_sha  # noqa: E402
from .git_context import current_pr as _current_pr  # noqa: E402  (kept near use for clarity)
from .git_context import git_dir as _git_dir  # noqa: E402
from .agent_sandbox import SANDBOX_SETTINGS, WORKTREE_ROOT, agent_env  # noqa: E402
from .git_context import parse_diff as _parse_diff  # noqa: E402
from .git_context import repo_root as _repo_root  # noqa: E402


def _resolve_claude_bin() -> str:
    """The `claude` CLI via the shared `claude_cli.resolve_claude_bin`, or `RecitalError`."""
    resolved = resolve_claude_bin()
    if resolved is None:
        raise RecitalError("claude CLI not on PATH")
    return resolved


REVIEWER_MODEL = MEASURED_MODEL


class ReviewerReply(_t.NamedTuple):
    """One reviewer's answer and what it cost, as the CLI reported it. A runner may return a bare
    `str` instead (test fakes do): its row then carries no token counts."""
    text: str
    tokens: dict[str, int]
    cost_usd: float | None
    models: tuple[str, ...] = ()


def _default_runner(prompt: str, timeout: float = 180.0, *, cwd: str | None = None,
                    extra_args: _t.Sequence[str] = (), env: dict[str, str] | None = None) -> ReviewerReply:
    """Spawn `claude -p --output-format json`, prompt on stdin, and return its answer with the token
    counts and cost it reports. Raises `RecitalError` on failure.

    The prompt embeds the whole staged diff, so it goes over stdin, not argv: one argv string
    is capped at 128 KB on Linux (`MAX_ARG_STRLEN`) and the whole command line at 32 KB on
    Windows. A 2026-09-30 inventory backfill (long markdown lines) died with `E2BIG` here.
    """
    claude_bin = _resolve_claude_bin()
    # Reviewer turns load the user's MCP servers; `CECELIA_OBSERVER_NO_PAIR` (read by
    # `mcp/cecelia_mcp/client.py`, inherited by MCP children) stops a throwaway turn re-pairing
    # the user's project. Siblings: `scripts/judge/verify.py`, `scripts/agent_eval/run_overnight.py`.
    # `env` replaces the whole environment (a sandboxed agent's `agent_env`, credentials stripped).
    env = {**os.environ, "CECELIA_OBSERVER_NO_PAIR": "1"} if env is None else env
    try:
        result = subprocess.run(
            [claude_bin, "-p", "--model", REVIEWER_MODEL, "--output-format", "json", *extra_args],
            input=prompt,
            cwd=cwd,
            env=env,
            capture_output=True,
            text=True,
            timeout=timeout,
            check=False,
            encoding="utf-8",  # CLAUDE.md → Windows compatibility: default is cp1252 on Windows.
        )
    except subprocess.TimeoutExpired as e:
        raise RecitalError(f"claude timed out after {timeout}s") from e
    except OSError as e:
        # Spawn failure (E2BIG, ENOENT, permissions) → an errored `_run` row, not a traceback
        # that leaves the log with no trace of the attempt.
        raise RecitalError(f"claude could not be spawned: {e}") from e

    stderr, stdout = (result.stderr or "").strip(), (result.stdout or "").strip()
    try:
        out = read_result(result)
        said = None if result.returncode == 0 and not out.get("is_error") and isinstance(out.get("result"), str) \
            else failure_text(result, out) if out else stdout[-400:]
    except RateLimited as e:   # still a reviewer error here: the run row logs it, `auto_fix` reads the limit
        said = str(e)
    if said is not None:
        # A usage limit or API error is reported on stdout, often with stderr empty: show both, so a
        # warning on stderr can't hide the limit on stdout from `rate_limit` downstream.
        detail = "\n".join([*([f"stderr:\n{stderr}"] if stderr else []),
                            *([f"stdout:\n{said}"] if said else [])]) or "no output"
        raise RecitalError(f"claude exited {result.returncode}; {detail}")
    cost = out.get("total_cost_usd")
    return ReviewerReply(out["result"].strip(), _tokens(out),
                         float(cost) if isinstance(cost, (int, float)) else None, tuple(_models(out)))


def _reviewer_prompt(doc_path: str, diff: str) -> str:
    """Build the prompt handed to `claude -p`. Delegates the reviewer instructions to the doc
    (claude reads it), so the prompt stays a single source of truth. Explicit directive: emit
    ONLY findings + evidence — no tail line, no mechanism prose — because recital wraps its
    own tail line and evidence-fold headers around what the subagent returns."""
    return (
        f"Read `{doc_path}` and follow the 'Reviewer prompt' section verbatim against the "
        f"staged diff below. Output ONLY the findings + evidence body — do NOT emit the "
        f"`_..._` tail line or any mechanism prose. The caller wraps its own tail and "
        f"evidence-fold headers around your output.\n\n---\n\n{diff}"
    )


#: Reviewer might still emit its own tail line despite the directive (model non-determinism).
#: Strip trailing patterns like `_Convention check: run_` / `_Fanout audit: run_` /
#: `_Sibling-call audit: run_` so the recital's own tail isn't a duplicate.
_STRIP_TRAILING_TAIL = re.compile(
    r"\n?\s*_(?:Fanout audit|Sibling-call audit|Convention check|Result review):\s*[^\n_]+_\s*$"
)


def _run_reviewer(
    *,
    event_name: str,
    finding_event_name: str,
    advisory_event_name: str,
    mechanism: str,
    title: str,
    tail_none: str,
    doc_path: str,
    diff: str,
    claude_runner: _t.Callable[[str], str | ReviewerReply],
    pr: str | None,
    commit: str | None,
    branch: str | None,
    prompt: str | None = None,
    run_extra: dict | None = None,
) -> str:
    """Spawn one reviewer, emit its `_run` + per-finding events, format its recital section.
    `prompt` replaces the doc-plus-diff default; `run_extra` joins the `_run` row's payload."""
    prompt = _reviewer_prompt(doc_path, diff) if prompt is None else prompt
    start = time.monotonic()
    error: str | None = None
    output: str = ""
    reply: str | ReviewerReply = ""
    try:
        reply = claude_runner(prompt)
    except RecitalError as e:
        error = str(e)

    duration = time.monotonic() - start
    payload: dict = {"duration_s": round(duration, 2), **(run_extra or {})}
    if isinstance(reply, ReviewerReply):
        output = reply.text
        payload["tokens"] = reply.tokens
        if reply.cost_usd is not None:
            payload["cost_usd"] = round(reply.cost_usd, 4)
        if reply.models:
            payload["models"] = list(reply.models)
    else:
        output = reply
    if error is not None:
        payload["error"] = error
    append_event(event_name, payload, pr=pr, commit=commit, branch=branch)

    if error is not None:
        # Not `tail_none`: "no audit needed" under an error would say the reviewer ran and cleared it.
        return (
            f"_{title} (evidence):_\n\n> **{ERROR_MARKER}** — {error}\n\n"
            f"_{title}: not run — the reviewer errored_"
        )

    stripped = output.strip()

    # Short-circuit detection — reviewer replied with the "no X needed" tail directly.
    # Two shapes both count: wrapped `_no fanout audit needed_` (passes through verbatim as the
    # tail — the reply IS the tail line) and bare `no fanout audit needed` (wrapped with the
    # standard `_<title>: <verdict>_` frame). The bare match also accepts the pre-rename
    # "no sibling-call audit needed" wording — the reviewer may still emit it from muscle memory
    # or an older cached prompt. Single-line reply only.
    if "\n" not in stripped:
        wrapped = stripped.startswith("_") and stripped.endswith("_")
        bare = stripped.strip("_").strip().lower()
        short_circuit_wordings = {tail_none.lower(), "no sibling-call audit needed"}
        if bare in short_circuit_wordings:
            if wrapped:
                return stripped
            return f"_{title}: {tail_none}_"

    # Parse outcome-tag-requiring findings and emit one `_finding` row per (Decision 2 of
    # FINDINGS_EMISSION_PLAN.md — written pre-commit as pending, i.e. no `outcome` field;
    # log.py's UnknownOutcomeError check only fires when outcome is *present*-and-unknown).
    findings = _parse_findings(stripped, mechanism)
    for f in findings:
        append_event(
            finding_event_name,
            {"file": f.file, "line": f.line, "desc": f.desc, "slug": f.slug, "marker": f.marker},
            pr=pr,
            commit=commit,
            branch=branch,
        )
    advisories = _parse_findings(stripped, mechanism, ADVISORY_MARKERS[mechanism])
    for f in advisories:
        append_event(
            advisory_event_name,
            {"file": f.file, "line": f.line, "desc": f.desc, "marker": f.marker,
             **({"slug": f.slug} if f.marker in TAGGABLE_ADVISORY[mechanism] else {})},
            pr=pr,
            commit=commit,
            branch=branch,
        )

    # Inject slugs so the author can copy each into a `[slug: outcome]` pair in the commit
    # message: the outcome-tag-requiring findings, and the optional-tag `plausible` notes.
    slugged = _inject_slugs(stripped, mechanism)

    # Defensive strip: even with the "no tail line" directive in the prompt, the subagent
    # sometimes still emits one. Remove trailing `_<title>: <verdict>_` so the recital's
    # wrapper tail isn't a duplicate.
    cleaned = _STRIP_TRAILING_TAIL.sub("", slugged).rstrip()

    unparsed = _unparsed_marker_count(stripped, findings, MARKERS[mechanism])
    if unparsed > 0:
        cleaned += (
            f"\n\n> **RECITAL PARSE WARNING** — {unparsed} finding marker(s) are not in a "
            "`- …[**marker**]` bullet the parser knows, so they got no slug and no log row. Tag each with the legacy "
            "bare form in the commit message, and report the reviewer output shape."
        )
    unparsed_adv = _unparsed_marker_count(stripped, advisories, ADVISORY_MARKERS[mechanism])
    if unparsed_adv > 0:
        cleaned += (
            f"\n\n> **RECITAL PARSE WARNING** — {unparsed_adv} advisory marker(s) are not in a "
            "`- …[**marker**]` bullet the parser knows, so they got no log row and won't show on "
            "the console. No tag needed; report the reviewer output shape."
        )

    if any(f.marker in TAGGABLE_ADVISORY[mechanism] for f in advisories):
        cleaned += "\n\n" + _OPTIONAL_TAG_NOTE.replace("{Marker}", TAGGABLE_ADVISORY[mechanism][0].capitalize())
    return f"_{title} (evidence):_\n\n{cleaned}\n\n_{title}: run_"


#: The fenced block in `RESULT_REVIEW.md` that lists the result paths, one glob per line.
_RESULT_PATHS_FENCE = re.compile(r"^```result-paths\n(.*?)^```", re.M | re.S)
#: Never a result path, even under a listed directory.
_NOT_RESULT_PATHS = ("*/test/*", "*/tests/*", "*.test.ts", "*.md")
#: Where the staged tree is copied for the reviewer: beside the sandbox's other agent checkouts, outside
#: `~` and `.git/` (both write-denied). One folder per worktree (its git dir's hash), the latest only.
RESULT_TREE_ROOT = WORKTREE_ROOT / "result-review"
#: The reviewer reads, writes repros in its copy, and runs them; nothing else.
RESULT_REVIEW_TOOLS = "Read,Grep,Glob,Write,Edit,Bash"
#: An Opus review that writes and runs repros takes minutes, not the other reviewers' seconds.
RESULT_REVIEW_TIMEOUT = 1200.0


def result_paths(doc_text: str) -> list[str]:
    """The globs in `RESULT_REVIEW.md`'s `result-paths` block; [] when the block is missing."""
    m = _RESULT_PATHS_FENCE.search(doc_text)
    return [ln.strip() for ln in m.group(1).splitlines() if ln.strip()] if m else []


def is_result_path(path: str, globs: _t.Sequence[str]) -> bool:
    """`path` matches a result glob and none of `_NOT_RESULT_PATHS`."""
    return any(fnmatch.fnmatch(path, g) for g in globs) and not any(fnmatch.fnmatch(path, x) for x in _NOT_RESULT_PATHS)


def repo_result_paths(root: pathlib.Path | None = None) -> list[str]:
    """The result globs `RESULT_REVIEW.md` lists in the checkout at `root` (default: this repo)."""
    doc = (root or _repo_root()) / _RESULT_DOC
    return result_paths(doc.read_text(encoding="utf-8")) if doc.is_file() else []


def touched_result_paths(diff: str, globs: _t.Sequence[str]) -> list[str]:
    """The diff's files that are result paths (`is_result_path`)."""
    return [f.path for f in _parse_diff(diff) if is_result_path(f.path, globs)]


def _export_staged_tree(root: pathlib.Path, dest: pathlib.Path) -> None:
    """The index (what is about to be committed) as plain files under `dest`, plus the worktree's
    `frontend/node_modules` linked in so a vitest repro runs. The reviewer writes only here."""
    if dest.exists():
        shutil.rmtree(dest)
    dest.mkdir(parents=True)
    subprocess.run(["git", "checkout-index", "-a", "-f", f"--prefix={dest}{os.sep}"], cwd=root,
                   check=True, capture_output=True, encoding="utf-8")
    modules = root / "frontend" / "node_modules"
    if modules.is_dir() and (dest / "frontend").is_dir():
        try:
            (dest / "frontend" / "node_modules").symlink_to(modules, target_is_directory=True)
        except OSError:  # no symlink rights (Windows without developer mode): vitest repros can't run
            pass


def _result_prompt(doc_path: str, diff: str, claim: str | None, tree: pathlib.Path, python: str) -> str:
    """The doc's reviewer prompt plus what this run needs: the claim (unverified), and how to run code."""
    said = (f'The author\'s claim, UNVERIFIED: "{claim}"' if claim
            else "The author gave no claim; work out what the change is for from the diff.")
    return (
        f"Read `{doc_path}` and follow the 'Reviewer prompt' section verbatim against the staged diff "
        f"below. Output ONLY the finding bullets (or the no-findings line).\n\n{said}\n\n"
        f"Your working directory `{tree}` is a copy of the repository with the change applied; write "
        f"only inside it. Python: `{python} <script>` (PYTHONPATH already points at `python/`). "
        f"Frontend: `cd frontend && npx vitest run <file>`.\n\n---\n\n{diff}"
    )


def run_result_review(
    diff: str,
    *,
    claim: str | None,
    claude_runner: _t.Callable[[str], str | ReviewerReply] | None,
    pr: str | None,
    commit: str | None,
    branch: str | None,
) -> str:
    """The third reviewer, gated on result paths and reused for an unchanged staged diff.
    `claude_runner` (tests) replaces the spawn and skips the tree export."""
    title = "Result review"
    root = _repo_root()
    touched = touched_result_paths(diff, repo_result_paths(root))
    if not touched:
        append_event("result_review_run", {"duration_s": 0.0, "escape_valve": "no_result_paths"},
                     pr=pr, commit=commit, branch=branch)
        return f"_{title}: skipped — no result paths_"

    key = hashlib.sha1(f"{claim or ''}\n{diff}".encode("utf-8")).hexdigest()[:12]
    gd = _git_dir() if claude_runner is None else None  # a test's fake runner caches nothing
    home = (gd / "result-review") if gd else None
    saved = home / key / "section.md" if home else None
    if saved is not None and saved.is_file():
        append_event("result_review_run", {"duration_s": 0.0, "escape_valve": "unchanged_diff"},
                     pr=pr, commit=commit, branch=branch)
        return saved.read_text(encoding="utf-8").replace(f"_{title}: run_", f"_{title}: run — reused, staged diff unchanged_")

    runner = claude_runner
    trees = RESULT_TREE_ROOT / hashlib.sha1(str(gd).encode("utf-8")).hexdigest()[:10] if gd else None
    tree = (trees / key) if trees else root
    if runner is None:
        if home is None or trees is None:
            return f"_{title}: not run — no git dir to key the review on_"
        for old in [*(home.glob("*") if home.is_dir() else ()), *(trees.glob("*") if trees.is_dir() else ())]:
            if old.name != key:  # keep one review: the latest
                shutil.rmtree(old, ignore_errors=True)
        _export_staged_tree(root, tree)
        def runner(prompt: str) -> ReviewerReply:
            # The judge's verify agent's fence: OS sandbox (no network, `~` write-denied), credentials
            # stripped from the env. The copy lives under the temp dir, outside both denies.
            return _default_runner(prompt, RESULT_REVIEW_TIMEOUT, cwd=str(tree),
                                   extra_args=["--dangerously-skip-permissions",
                                               "--settings", json.dumps(SANDBOX_SETTINGS),
                                               "--tools", RESULT_REVIEW_TOOLS, "--strict-mcp-config",
                                               "--no-session-persistence"],
                                   env=agent_env(PYTHONPATH=str(tree / "python")))

    section = _run_reviewer(
        event_name="result_review_run",
        finding_event_name="result_review_finding",
        advisory_event_name="result_review_advisory",
        mechanism="result",
        title=title,
        tail_none="no result review needed",
        doc_path=_RESULT_DOC,
        diff=diff,
        claude_runner=runner,
        pr=pr,
        commit=commit,
        branch=branch,
        prompt=_result_prompt(_RESULT_DOC, diff, claim, tree, sys.executable),
        run_extra={"paths": touched[:20], "repro_dir": str(tree / "_review")},
    )
    if saved is not None and ERROR_MARKER not in section:
        saved.parent.mkdir(parents=True, exist_ok=True)
        saved.write_text(section, encoding="utf-8")
    return section


#: The stamp recital ends its body with — which change (branch @ parent SHA) the review is OF.
#: The commit-msg hook rejects a message carrying a stamp for a different change: a recital body
#: written to a file another session also writes (a shared scratchpad) gets spliced — one run's
#: body over the head of the other's — and the reader commits review evidence for code it never
#: changed. The log rows already carry branch + SHA; the text did not, so nothing could tell.
STAMP_RE = re.compile(r"_Recital stamp: (\S+)@([0-9a-f]{12}|\?)_")


def recital_stamp(branch: str | None, commit: str | None) -> str:
    """`_Recital stamp: <branch>@<sha12>_` — `?` for a part git could not report."""
    return f"_Recital stamp: {branch or '?'}@{commit[:12] if commit else '?'}_"


#: Where `scripts/recital.py` also saves the body, inside the worktree's own git dir. Each
#: worktree has its own, so no other session's run can overwrite it. Agents append this file to
#: the commit message instead of capturing stdout to a temp file and finding it again later.
RECITAL_BODY_FILE = "recital-body.md"


def recital_body_path(cwd: str | None = None) -> pathlib.Path | None:
    """`<git dir>/recital-body.md` for the worktree at `cwd`, or None outside a git checkout."""
    d = _git_dir(cwd)
    return d / RECITAL_BODY_FILE if d else None


def run_recital(
    diff: str,
    *,
    claude_runner: _t.Callable[[str], str | ReviewerReply] | None = None,
    claim: str | None = None,
) -> str:
    """Spawn the reviewers, emit `_run` events, return the formatted recital body. `claim` is what
    the result review is told the change is for (marked unverified).

    - `diff`: the staged diff string (from `git diff --staged`).
    - `claude_runner`: optional; injected in tests to bypass real subprocess. Signature:
      `callable(prompt: str) -> str | ReviewerReply` (the answer on success, raises `RecitalError`
      on failure). A `ReviewerReply` puts its `tokens` and `cost_usd` on the `_run` row.

    Return value is markdown ready to append to the commit-message body. Includes both
    reviewers' evidence folds and tail lines, and ends with `recital_stamp(branch, commit)`. `_run` events are appended to
    `~/.cecelia-effectiveness/events.jsonl` regardless of reviewer success — a failure
    payload gets `error` in it, so the log has both signals. Each row records the HEAD SHA
    at recital time in the row-level `commit` field so the pre-commit hook can enforce that
    a findings-carrying commit was reviewed against the tree it is being made on top of.
    Each row also records the current branch name so the rollup can join to a PR via
    `gh pr list --head <branch>` — findings land pre-commit and usually have `pr: null`,
    branch resolves that at render time.
    """
    runner = claude_runner or _default_runner
    pr = _current_pr()
    commit = _current_head_sha()
    branch = _current_branch()

    fanout_section = _run_reviewer(
        event_name="fanout_audit_run",
        finding_event_name="fanout_audit_finding",
        advisory_event_name="fanout_audit_advisory",
        mechanism="fanout",
        title="Fanout audit",
        tail_none="no fanout audit needed",
        doc_path=_FANOUT_DOC,
        diff=diff,
        claude_runner=runner,
        pr=pr,
        commit=commit,
        branch=branch,
    )
    convention_section = _run_reviewer(
        event_name="convention_check_run",
        finding_event_name="convention_check_finding",
        advisory_event_name="convention_check_advisory",
        mechanism="convention",
        title="Convention check",
        tail_none="no convention check needed",
        doc_path=_CONVENTION_DOC,
        diff=diff,
        claude_runner=runner,
        pr=pr,
        commit=commit,
        branch=branch,
    )
    result_section = run_result_review(diff, claim=claim, claude_runner=claude_runner,
                                       pr=pr, commit=commit, branch=branch)
    inventory_section = run_inventory_check(diff, pr=pr, commit=commit, branch=branch)
    lint_section = run_maintainability_lint(diff, pr=pr, commit=commit, branch=branch)

    return (f"{fanout_section}\n\n{convention_section}\n\n{result_section}\n\n{inventory_section}\n\n{lint_section}"
            f"\n\n{recital_stamp(branch, commit)}")
