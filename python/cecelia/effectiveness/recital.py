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

which reads `git diff --staged`, spawns both reviewers, emits `_run` events, and prints the
recital body. Agent captures stdout and appends to the commit message body.

Test seam: `run_recital(diff, claude_runner=fake)` bypasses real claude spawning. Same pattern
as `api/test/suite/kiwi_turn.jl`'s fake engine.

Not built here (deferred):
- Escape-valve detection (docs-only / no-additions / tests-only). v1 always spawns both;
  reviewers' own short-circuit handles empty cases.
- Finding parsing + per-finding outcome-tag scaffolding. v1 passes reviewer output through
  verbatim under the evidence fold; the agent tags outcomes at commit time.
- `_finding` event emission. `_run` events (with duration + escape valve) are enough for the
  first N commits of data; findings need outcome-resolution design.
"""
from __future__ import annotations

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
    title: str,
    tail_none: str,
    doc_path: str,
    diff: str,
    claude_runner: _t.Callable[[str], str],
) -> str:
    """Spawn one reviewer, emit its `_run` event, format its section of the recital."""
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
    append_event(event_name, payload)

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

    # Defensive strip: even with the "no tail line" directive in the prompt, the subagent
    # sometimes still emits one. Remove trailing `_<title>: <verdict>_` so the recital's
    # wrapper tail isn't a duplicate.
    cleaned = _STRIP_TRAILING_TAIL.sub("", stripped).rstrip()

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

    fanout_section = _run_reviewer(
        event_name="fanout_audit_run",
        title="Fanout audit",
        tail_none="no fanout audit needed",
        doc_path=_FANOUT_DOC,
        diff=diff,
        claude_runner=runner,
    )
    convention_section = _run_reviewer(
        event_name="convention_check_run",
        title="Convention check",
        tail_none="no convention check needed",
        doc_path=_CONVENTION_DOC,
        diff=diff,
        claude_runner=runner,
    )

    return f"{fanout_section}\n\n{convention_section}"
