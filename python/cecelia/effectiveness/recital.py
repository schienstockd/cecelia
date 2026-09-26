"""Recital orchestrator — the single entry point for pre-commit reviewer discipline.

Spawns both reviewers (fanout audit + convention check) via `claude -p` subprocess, emits log
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
import re
import shutil
import subprocess
import time
import typing as _t

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


#: The two outcome-tag-requiring markers per CLAUDE.md → Git & commits (only these produce
#: `_finding` rows; `plausible` / `potential duplicate` are surfaced in the recital body but
#: don't need outcome resolution, so they stay out of the log to keep the pending↔resolution
#: contract 1:1 with the hook's tag-count check).
_FANOUT_MARKER = "confirmed"
_CONVENTION_MARKER = "should reuse"

#: Bullet-line grammar shared by both reviewer prompts:
#:   `- **file:LINE** — <prose> [**marker**]`
#: `file` allows anything but `*` and `:`; `line` is digits; `desc` is non-greedy.
#: Matches the exact shape locked in SIBLING_CALL_AUDIT.md and CONVENTION_CHECK.md
#: (Decision 1 of FINDINGS_EMISSION_PLAN.md). Alternate markers per mechanism are baked in.
_FANOUT_FINDING_RE = re.compile(
    r"^- \*\*([^*:]+):(\d+)\*\*\s+[—-]\s+(.+?)\s*\[\*\*confirmed\*\*\]\s*$",
    re.MULTILINE,
)
_CONVENTION_FINDING_RE = re.compile(
    r"^- \*\*([^*:]+):(\d+)\*\*\s+[—-]\s+(.+?)\s*\[\*\*should reuse\*\*\]\s*$",
    re.MULTILINE,
)


class Finding(_t.NamedTuple):
    """A single outcome-tag-requiring finding parsed from a reviewer's output."""

    mechanism: str  # "fanout" | "convention"
    file: str
    line: int
    desc: str
    marker: str  # the literal marker text (e.g. "confirmed" / "should reuse")
    slug: str  # deterministic id — see `_slug`


def _slug(mechanism: str, file: str, line: int, marker: str) -> str:
    """Deterministic short id for a finding, per Decision 3 of FINDINGS_EMISSION_PLAN.md.

    Same (mechanism, file, line, marker) → same slug across runs, so the author can quote a
    stale slug from a re-run of the recital without breaking correlation. sha1 truncated to
    8 hex chars — collision-safe at this cardinality (findings-per-PR ~O(10)); prefixed with
    a short mechanism tag for human readability in commit messages.
    """
    key = f"{mechanism}|{file}|{line}|{marker}".encode("utf-8")
    digest = hashlib.sha1(key).hexdigest()[:8]
    prefix = "fanout" if mechanism == "fanout" else "conv"
    return f"{prefix}-{digest}"


def _parse_findings(output: str, mechanism: str) -> list[Finding]:
    """Extract outcome-tag-requiring findings from a reviewer's raw output.

    Non-matching lines (commentary bullets, plausibles, short-circuits) are silently ignored;
    the parser is strict-regex to avoid over-emitting log rows for lines the hook won't be
    asked to resolve. Order-preserving.
    """
    regex = _FANOUT_FINDING_RE if mechanism == "fanout" else _CONVENTION_FINDING_RE
    marker = _FANOUT_MARKER if mechanism == "fanout" else _CONVENTION_MARKER
    findings: list[Finding] = []
    for m in regex.finditer(output):
        file, line_s, desc = m.group(1).strip(), m.group(2), m.group(3).strip()
        try:
            line = int(line_s)
        except ValueError:  # unreachable — regex \d+ guarantees digit-only, but belt-and-braces
            continue
        findings.append(Finding(
            mechanism=mechanism,
            file=file,
            line=line,
            desc=desc,
            marker=marker,
            slug=_slug(mechanism, file, line, marker),
        ))
    return findings


def _inject_slugs(output: str, findings: _t.Sequence[Finding]) -> str:
    """Rewrite each matched finding line to lead with its slug, so the author can copy it
    verbatim into a `[slug: outcome]` pair. Non-matching lines untouched. Idempotent enough:
    if the reviewer already included a slug we'd double-tag, but that shape isn't produced by
    the reviewer prompts today."""
    if not findings:
        return output
    by_line: dict[tuple[str, int, str], Finding] = {
        (f.file, f.line, f.marker): f for f in findings
    }
    mechanism = findings[0].mechanism
    regex = _FANOUT_FINDING_RE if mechanism == "fanout" else _CONVENTION_FINDING_RE
    marker = _FANOUT_MARKER if mechanism == "fanout" else _CONVENTION_MARKER

    def _sub(m: re.Match) -> str:
        file, line_s, desc = m.group(1).strip(), m.group(2), m.group(3).strip()
        f = by_line.get((file, int(line_s), marker))
        if f is None:
            return m.group(0)
        return f"- [{f.slug}] **{file}:{line_s}** — {desc} [**{marker}**]"

    return regex.sub(_sub, output)


def _current_pr() -> str | None:
    """Best-effort PR-number capture via `gh pr view --json number`. Returns `"#N"` or None.

    Failure modes silently return None: no `gh`, not in a PR branch, offline, `gh` not
    authenticated. Recital works fine without a PR context — the log row just has `pr: null`.
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


def _resolve_claude_bin() -> str:
    """Cross-platform resolution of the `claude` CLI. Named per `CLAUDE.md → Windows
    compatibility` — a bare `subprocess.run(["claude", ...])` fails to find the npm-installed
    `claude.cmd`/`.bat` shim on Windows. `shutil.which` walks `PATH` and PATHEXT, so it
    resolves the wrapper the Python side never had a helper for (Julia has `agent_bin_path()`
    in `app/src/ai/agent_runner.jl`; this is the stdlib equivalent, no new module needed)."""
    resolved = shutil.which("claude")
    if resolved is None:
        raise RecitalError("claude CLI not on PATH (checked via shutil.which)")
    return resolved


def _default_runner(prompt: str, timeout: float = 180.0) -> str:
    """Spawn `claude -p <prompt>` and return stdout. Raises `RecitalError` on failure."""
    claude_bin = _resolve_claude_bin()
    try:
        result = subprocess.run(
            [claude_bin, "-p", prompt],
            capture_output=True,
            text=True,
            timeout=timeout,
            check=False,
            encoding="utf-8",  # CLAUDE.md → Windows compatibility: default is cp1252 on Windows.
        )
    except subprocess.TimeoutExpired as e:
        raise RecitalError(f"claude timed out after {timeout}s") from e

    if result.returncode != 0:
        raise RecitalError(
            f"claude exited {result.returncode}; stderr:\n{(result.stderr or '').strip()}"
        )
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
    mechanism: str,
    title: str,
    tail_none: str,
    doc_path: str,
    diff: str,
    claude_runner: _t.Callable[[str], str],
    pr: str | None,
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
    append_event(event_name, payload, pr=pr)

    if error is not None:
        return (
            f"_{title} (evidence):_\n\n> **RECITAL SCRIPT ERROR** — {error}\n\n"
            f"_{title}: {tail_none}_"
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
        )

    # Inject slugs so the author can copy each into a `[slug: outcome]` pair in the commit
    # message. Only the outcome-tag-requiring findings get slugs; other bullets (plausible /
    # potential duplicate) render unchanged.
    slugged = _inject_slugs(stripped, findings)

    # Defensive strip: even with the "no tail line" directive in the prompt, the subagent
    # sometimes still emits one. Remove trailing `_<title>: <verdict>_` so the recital's
    # wrapper tail isn't a duplicate.
    cleaned = _STRIP_TRAILING_TAIL.sub("", slugged).rstrip()

    return f"_{title} (evidence):_\n\n{cleaned}\n\n_{title}: run_"


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
    reviewers' evidence folds and tail lines. `_run` events are appended to
    `~/.cecelia-effectiveness/events.jsonl` regardless of reviewer success — a failure
    payload gets `error` in it, so the log has both signals.
    """
    runner = claude_runner or _default_runner
    pr = _current_pr()

    fanout_section = _run_reviewer(
        event_name="fanout_audit_run",
        finding_event_name="fanout_audit_finding",
        mechanism="fanout",
        title="Fanout audit",
        tail_none="no fanout audit needed",
        doc_path=_FANOUT_DOC,
        diff=diff,
        claude_runner=runner,
        pr=pr,
    )
    convention_section = _run_reviewer(
        event_name="convention_check_run",
        finding_event_name="convention_check_finding",
        mechanism="convention",
        title="Convention check",
        tail_none="no convention check needed",
        doc_path=_CONVENTION_DOC,
        diff=diff,
        claude_runner=runner,
        pr=pr,
    )

    return f"{fanout_section}\n\n{convention_section}"
