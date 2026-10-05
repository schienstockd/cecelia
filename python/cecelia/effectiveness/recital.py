"""Recital orchestrator — the single entry point for pre-commit reviewer discipline.

Spawns both reviewers (fanout audit + convention check) via `claude -p` subprocess, runs the two
mechanical checks (inventory coverage, maintainability lint), emits log
events atomically with the runs, and produces formatted recital text ready to paste into the
commit message. Replaces the manual "agent spawns subagents, agent formats, agent emits"
protocol previously described in `CLAUDE.md` — by centralising the whole thing in code, none
of the steps can be silently skipped in autonomous mode. Documented failure that motivated
this: four consecutive reviewer runs in one session where the parent agent skipped the log
emission step every time, leaving zero rows in `~/.cecelia-effectiveness/events.jsonl`.

Usage (from an agent, via the CLI wrapper):

    pixi run recital

which reads `git diff --staged`, spawns both reviewers, emits `_run` events and one
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

import hashlib
import os
import re
import subprocess
import time
import typing as _t

from .inventory_coverage import run_inventory_check
from .maintainability_lint import run_maintainability_lint
from .log import append_event

#: Path to each reviewer's spec doc. `claude -p` reads it itself — the reviewer prompt is
#: the single source of truth.
_FANOUT_DOC = "docs/ai-assist/FANOUT_AUDIT.md"
_CONVENTION_DOC = "docs/ai-assist/CONVENTION_CHECK.md"


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
}

#: Advisory markers — no outcome tag, so no slug and no `_finding` row, but still a finding the
#: reader should see: each becomes an `_advisory` row the console lists.
ADVISORY_MARKERS: dict[str, tuple[str, ...]] = {
    "fanout": ("plausible",),
    "convention": ("potential duplicate",),
}

#: Bullet-line grammar the reviewer prompts ask for:
#:   `- **file:LINE** — <prose> [**marker**]`
#: Reviewers drift from it in practice — line ranges (`:245-246`), line lists (`:11, :309`,
#: `:44,49`), backtick-wrapped paths, `**:` instead of `** —`. A strict regex silently dropped
#: every finding from 2026-09-28 on (11 of 29 across session history), so the parser is
#: tolerant: ANY bullet line carrying the marker is a finding. The location is best-effort —
#: first `file:LINE` wins, so `foo.jl:42` slugs the same with or without a trailing range.
_BULLET_RE = re.compile(r"^[ \t]*[-*][ \t]+(?!\[(?:fanout|conv)-[0-9a-f]{8}\] )(.+)$", re.MULTILINE)
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

    mechanism: str  # "fanout" | "convention"
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
    prefix = "fanout" if mechanism == "fanout" else "conv"
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
    """Prefix each finding bullet with its slug, so the author can copy it verbatim into a
    `[slug: outcome]` pair. The reviewer's own text is kept as-is (ranges and all). Bullets
    already carrying a slug are skipped by `_BULLET_RE`."""
    def _sub(m: re.Match) -> str:
        f = _parse_bullet(m.group(1), mechanism)
        return m.group(0) if f is None else f"- [{f.slug}] {m.group(1)}"

    return _BULLET_RE.sub(_sub, output)


def _unparsed_marker_count(output: str, findings: _t.Sequence[Finding],
                           markers: _t.Sequence[str]) -> int:
    """Bold `markers` occurrences that did NOT become a finding — off a bullet line, or in a
    shape the tag pattern doesn't know. Tripwire for the next grammar drift: surfaced in the
    recital body instead of dropping silently."""
    bold = sum(len(_marker_bold_re(mk).findall(outside_code(output))) for mk in markers)
    return bold - len(findings)


from .claude_cli import resolve_claude_bin  # noqa: E402
from .git_context import current_branch as _current_branch  # noqa: E402
from .git_context import current_head_sha as _current_head_sha  # noqa: E402
from .git_context import current_pr as _current_pr  # noqa: E402  (kept near use for clarity)


def _resolve_claude_bin() -> str:
    """The `claude` CLI via the shared `claude_cli.resolve_claude_bin`, or `RecitalError`."""
    resolved = resolve_claude_bin()
    if resolved is None:
        raise RecitalError("claude CLI not on PATH")
    return resolved


def _default_runner(prompt: str, timeout: float = 180.0) -> str:
    """Spawn `claude -p`, prompt on stdin, and return stdout. Raises `RecitalError` on failure.

    The prompt embeds the whole staged diff, so it goes over stdin, not argv: one argv string
    is capped at 128 KB on Linux (`MAX_ARG_STRLEN`) and the whole command line at 32 KB on
    Windows. A 2026-09-30 inventory backfill (long markdown lines) died with `E2BIG` here.
    """
    claude_bin = _resolve_claude_bin()
    # Reviewer turns load the user's MCP servers; `CECELIA_OBSERVER_NO_PAIR` (read by
    # `mcp/cecelia_mcp/client.py`, inherited by MCP children) stops a throwaway turn re-pairing
    # the user's project. Siblings: `scripts/judge/verify.py`, `scripts/agent_eval/run_overnight.py`.
    env = {**os.environ, "CECELIA_OBSERVER_NO_PAIR": "1"}
    try:
        result = subprocess.run(
            [claude_bin, "-p"],
            input=prompt,
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

    if result.returncode != 0:
        # A usage limit or API error is reported on stdout, with stderr empty: show whichever has it.
        stderr, stdout = (result.stderr or "").strip(), (result.stdout or "").strip()
        detail = f"stderr:\n{stderr}" if stderr else f"stdout:\n{stdout[-400:]}" if stdout else "no output"
        raise RecitalError(f"claude exited {result.returncode}; {detail}")
    return (result.stdout or "").strip()


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
    r"\n?\s*_(?:Fanout audit|Sibling-call audit|Convention check):\s*[^\n_]+_\s*$"
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
    claude_runner: _t.Callable[[str], str],
    pr: str | None,
    commit: str | None,
    branch: str | None,
) -> str:
    """Spawn one reviewer, emit its `_run` + per-finding events, format its recital section."""
    prompt = _reviewer_prompt(doc_path, diff)
    start = time.monotonic()
    error: str | None = None
    output: str = ""
    try:
        output = claude_runner(prompt)
    except RecitalError as e:
        error = str(e)

    duration = time.monotonic() - start
    payload: dict = {"duration_s": round(duration, 2)}
    if error is not None:
        payload["error"] = error
    append_event(event_name, payload, pr=pr, commit=commit, branch=branch)

    if error is not None:
        # Not `tail_none`: "no audit needed" under an error would say the reviewer ran and cleared it.
        return (
            f"_{title} (evidence):_\n\n> **RECITAL SCRIPT ERROR** — {error}\n\n"
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
            {"file": f.file, "line": f.line, "desc": f.desc, "marker": f.marker},
            pr=pr,
            commit=commit,
            branch=branch,
        )

    # Inject slugs so the author can copy each into a `[slug: outcome]` pair in the commit
    # message. Only the outcome-tag-requiring findings get slugs; other bullets (plausible /
    # potential duplicate) render unchanged.
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

    return f"_{title} (evidence):_\n\n{cleaned}\n\n_{title}: run_"


#: The stamp recital ends its body with — which change (branch @ parent SHA) the review is OF.
#: The commit-msg hook rejects a message carrying a stamp for a different change: a recital body
#: written to a file another session also writes (a shared scratchpad) gets spliced — one run's
#: body over the head of the other's — and the reader commits review evidence for code it never
#: changed. The log rows already carry branch + SHA; the text did not, so nothing could tell.
STAMP_RE = re.compile(r"_Recital stamp: (\S+)@([0-9a-f]{12}|\?)_")


def recital_stamp(branch: str | None, commit: str | None) -> str:
    """`_Recital stamp: <branch>@<sha12>_` — `?` for a part git could not report."""
    return f"_Recital stamp: {branch or '?'}@{commit[:12] if commit else '?'}_"


def run_recital(
    diff: str,
    *,
    claude_runner: _t.Callable[[str], str] | None = None,
) -> str:
    """Spawn both reviewers, emit `_run` events, return the formatted recital body.

    - `diff`: the staged diff string (from `git diff --staged`).
    - `claude_runner`: optional; injected in tests to bypass real subprocess. Signature:
      `callable(prompt: str) -> str` (stdout on success, raises `RecitalError` on failure).

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
    inventory_section = run_inventory_check(diff, pr=pr, commit=commit, branch=branch)
    lint_section = run_maintainability_lint(diff, pr=pr, commit=commit, branch=branch)

    return (f"{fanout_section}\n\n{convention_section}\n\n{inventory_section}\n\n{lint_section}"
            f"\n\n{recital_stamp(branch, commit)}")
