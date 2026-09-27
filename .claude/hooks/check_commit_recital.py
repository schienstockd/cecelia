#!/usr/bin/env python3
"""PreToolUse hook — every reviewer finding in a `git commit` recital must carry an outcome tag,
and slug-paired outcomes get written to the effectiveness log as resolution rows.

Wired via `.claude/settings.json` under `hooks.PreToolUse` matching the `Bash` tool. Fires on
every Bash call; short-circuits to allow unless the command contains `git commit`.

What it enforces
----------------
The convention-check reviewer is advisory by design (docs/todo/CONVENTION_CHECK_PLAN.md locked
decision #4) — findings land in the reservations recital but do not block a commit. That is
toothless in autonomous mode: nothing challenges a `**should reuse`` finding that the agent
silently commits over. Fanout audit is the same shape for its own findings.

This hook enforces two gates on a `git commit` whose message carries reviewer findings:

1. **Outcome-tag presence.** For every `**confirmed**` fanout / `**should reuse**` convention
   finding line, the message must carry an outcome tag from the closed vocabulary —
   `fixed_pre_commit`, `shipped_with_finding: <reason>`, or `false_positive: <reason>`.
   Deliberately mechanical text-matching, not semantic judgment: an agent could still write
   `false_positive: reasons` untruthfully; only the disclosure step is checked.
2. **SHA-anchored real-review.** A findings-carrying commit must have at least one recital
   `_run` row in the effectiveness log with `commit == HEAD-at-hook-time` — i.e. recital
   actually ran, and against the same tree the commit is being made on top of. Catches
   hand-typed recital bodies with no real `pixi run recital` invocation, and stale reviews
   from before a rebase. If HEAD SHA can't be captured (not a repo, git missing), the check
   degrades to allow rather than block on its own failure.

P3 of FINDINGS_EMISSION_PLAN.md — log writing
---------------------------------------------
When the message quotes a slug in a `[slug: outcome]` pair (e.g. `[fanout-abcd1234: fixed_pre_commit]`),
the hook ALSO writes a matching `_finding_resolved` row to `~/.cecelia-effectiveness/events.jsonl`.
That is what turns the recital's pending `_finding` rows into evidence in the rollup — see
`python/cecelia/effectiveness/rollup.py`. Log-write failure is not a commit blocker (best-effort);
the presence-check is the only gate.

Escape valve
------------
`CECELIA_SKIP_RECITAL_CHECK=1` bypasses. For real emergencies only.
"""
from __future__ import annotations

import json
import os
import pathlib
import re
import sys

#: The hook lives at `.claude/hooks/check_commit_recital.py`; the effectiveness log module
#: lives at `python/cecelia/effectiveness/log.py`. Reach the canonical vocabulary rather than
#: keeping a second literal copy — a divergent copy was the exact class of bug that
#: fanout + convention-check both caught on this hook's first draft.
_REPO_ROOT = pathlib.Path(__file__).resolve().parents[2]
sys.path.insert(0, str(_REPO_ROOT / "python"))
from cecelia.effectiveness import OUTCOME_VOCABULARY, append_event, read_events  # noqa: E402
from cecelia.effectiveness.git_context import (  # noqa: E402
    current_head_sha as _current_head_sha,
    current_pr as _current_pr,
)

#: Every finding line in the recital carries one of these bold markers.
_FINDING_MARKERS = re.compile(r"\*\*(?:confirmed|should reuse)\*\*")

#: `fixed_pre_commit` stands alone (the fix is in the diff). Every other outcome needs a
#: colon-prefixed reason, so a bare word appearing in prose or code doesn't count as a tag.
#: The standalone-vs-with-reason split is a hook convention, not a vocabulary property, so it
#: lives here rather than in `log.py`.
_STANDALONE_OUTCOMES = frozenset({"fixed_pre_commit"})
_REASON_OUTCOMES = OUTCOME_VOCABULARY - _STANDALONE_OUTCOMES

_alternation_reason = "|".join(sorted(map(re.escape, _REASON_OUTCOMES)))
_alternation_standalone = "|".join(sorted(map(re.escape, _STANDALONE_OUTCOMES)))

#: Slug-paired outcome — `[<slug>: <outcome>]` where slug is `<mechanism>-<8 hex>`. In the paired
#: form the outcome is a bare vocabulary word (no `:reason`); rationale goes in the commit body
#: prose, not in the tag. This keeps the pair-parse regex unambiguous vs the legacy bare
#: `[<outcome>: <reason>]` form the hook still accepts.
_alternation_all = "|".join(sorted(map(re.escape, OUTCOME_VOCABULARY)))
_SLUG_PAIR = re.compile(
    rf"\[((?:fanout|conv)-[0-9a-f]{{8}}):\s*({_alternation_all})\]"
)

#: Legacy bare outcome tags — accepted for counting so pre-P3 messages (or bespoke commits
#: without slugs) don't spuriously block. Slug pairs are counted separately then folded into
#: the total.
_BARE_OUTCOME_TAGS = re.compile(
    rf"\b(?:(?:{_alternation_standalone})\b|(?:{_alternation_reason}):\s*\S)"
)


def _outcome_help() -> str:
    """Build the help text listing every outcome, so a new vocabulary entry appears here for free."""
    lines = ["Slug-paired form (from `pixi run recital`):"]
    for tag in sorted(OUTCOME_VOCABULARY):
        lines.append(f"  - `[<slug>: {tag}]`  # e.g. `[fanout-abcd1234: {tag}]`")
    lines.append("Legacy bare form (no slug):")
    for tag in sorted(_STANDALONE_OUTCOMES):
        lines.append(f"  - `[{tag}]`")
    for tag in sorted(_REASON_OUTCOMES):
        lines.append(f"  - `[{tag}: <reason>]`")
    return "\n".join(lines)

#: The command has to actually be a `git commit` — subshells count (`cd path && git commit …`).
_GIT_COMMIT = re.compile(r"\bgit\s+commit\b")

#: `_run` events the SHA-anchored check considers. A recital run always emits both; findings-
#: carrying commits must have at least one row with `commit == HEAD-at-hook-time`.
_RUN_EVENTS = frozenset({"fanout_audit_run", "convention_check_run"})

_EXIT_ALLOW = 0
_EXIT_BLOCK = 2  # PreToolUse convention: non-zero = block; stderr shown to Claude.


def _load_tool_call() -> dict:
    """Read the hook's stdin JSON. Return {} on malformed input so we degrade to allow rather
    than block on our own bug."""
    try:
        return json.load(sys.stdin)
    except (json.JSONDecodeError, ValueError):
        return {}


def _parse_pairs(command: str) -> list[tuple[str, str]]:
    """Return the list of (slug, outcome) pairs the commit message quotes."""
    return [(m.group(1), m.group(2)) for m in _SLUG_PAIR.finditer(command)]


def _finding_event_for_slug(slug: str) -> str | None:
    """Map a slug prefix to its `_finding_resolved` event name."""
    if slug.startswith("fanout-"):
        return "fanout_audit_finding_resolved"
    if slug.startswith("conv-"):
        return "convention_check_finding_resolved"
    return None


def _has_matching_run(head_sha: str) -> bool:
    """True if the log has any `_run` row with `commit == head_sha`.

    Log-read failure counts as "no matching run" — a missing log means recital never
    emitted anything for this HEAD, which is exactly what the check is meant to catch.
    """
    try:
        for row in read_events():
            if row.get("event") in _RUN_EVENTS and row.get("commit") == head_sha:
                return True
    except OSError:
        pass
    return False


def check(command: str) -> str | None:
    """Return None if the commit is allowed; else a human-readable reason string.

    Two gates:
    1. **Outcome-tag presence** — every finding marker (`**confirmed**` / `**should reuse**`)
       needs a matching outcome tag (bare or slug-paired). Duplicate slugs are rejected.
    2. **SHA-anchored real-review** — a findings-carrying commit must have at least one
       recital `_run` row in the effectiveness log with `commit == HEAD-at-hook-time` (i.e.
       recital ran against the same tree the commit is being made on top of). Catches
       hand-typed recital bodies with no real `pixi run recital` invocation, and stale
       reviews from before a rebase.

    Gate 2 is skipped when HEAD SHA cannot be captured (not a repo, git missing) — degrade
    to allow rather than block on our own failure.
    """
    if not _GIT_COMMIT.search(command):
        return None

    findings = _FINDING_MARKERS.findall(command)
    bare_outcomes = _BARE_OUTCOME_TAGS.findall(command)
    pairs = _parse_pairs(command)

    # Duplicate slugs in one commit are a red flag — the same finding can't have two outcomes.
    # Block explicitly so the author de-dupes rather than silently taking whichever the log
    # happens to keep as "latest".
    slugs = [p[0] for p in pairs]
    dup_slugs = sorted({s for s in slugs if slugs.count(s) > 1})
    if dup_slugs:
        return (
            f"Duplicate slug(s) in commit message: {', '.join(dup_slugs)}. Each slug must "
            "appear at most once — the same finding cannot have two outcomes."
        )

    total_outcomes = len(bare_outcomes) + len(set(slugs))

    if len(findings) > total_outcomes:
        return (
            f"Recital carries {len(findings)} reviewer finding(s) marked "
            f"**confirmed** / **should reuse** but only {total_outcomes} outcome tag(s). "
            "Every finding needs one:\n"
            f"{_outcome_help()}\n"
            "Add the outcome tag to each finding line, then retry. "
            "Bypass in emergencies with `CECELIA_SKIP_RECITAL_CHECK=1`."
        )

    if findings:
        head_sha = _current_head_sha()
        if head_sha is not None and not _has_matching_run(head_sha):
            return (
                f"Recital carries {len(findings)} reviewer finding(s) but no matching "
                f"`_run` row is in the effectiveness log for HEAD {head_sha[:8]}. Run "
                "`pixi run recital` against the current tree, then retry — a hand-typed "
                "recital body without a real reviewer invocation is what this check exists "
                "to catch. After a rebase, re-run recital: the parent SHA changed. "
                "Bypass in emergencies with `CECELIA_SKIP_RECITAL_CHECK=1`."
            )

    return None


def write_resolutions(
    command: str, *, pr: str | None = None, commit: str | None = None,
) -> int:
    """Write one `_finding_resolved` row per slug-paired outcome in the commit message.

    Returns the number of rows written. Log-write failures are swallowed so a bad log path
    can't wedge the commit — the presence-check is the only gate. `pr` and `commit` are
    captured from `gh pr view` and `git rev-parse HEAD`; None on any failure (offline, not
    in PR, gh not installed, git not installed). `commit` here is the parent SHA of the
    commit being made (HEAD hasn't advanced yet at PreToolUse time), matching the SHA the
    recital `_run` row was written with.
    """
    written = 0
    for slug, outcome in _parse_pairs(command):
        event = _finding_event_for_slug(slug)
        if event is None:
            continue  # unknown mechanism prefix — silently skip; hook regex won't match anyway
        try:
            append_event(event, {"slug": slug, "outcome": outcome}, pr=pr, commit=commit)
            written += 1
        except Exception:  # noqa: BLE001 — best-effort emission
            pass
    return written


def main() -> int:
    if os.environ.get("CECELIA_SKIP_RECITAL_CHECK") == "1":
        return _EXIT_ALLOW

    data = _load_tool_call()
    if data.get("tool_name") != "Bash":
        return _EXIT_ALLOW

    command = data.get("tool_input", {}).get("command", "")
    reason = check(command)
    if reason is not None:
        print(f"check_commit_recital: BLOCKED — {reason}", file=sys.stderr)
        return _EXIT_BLOCK

    # Presence check passed — write any slug-paired outcomes to the log before the commit
    # runs. Best-effort: log failures don't block the commit.
    if _GIT_COMMIT.search(command):
        write_resolutions(command, pr=_current_pr(), commit=_current_head_sha())
    return _EXIT_ALLOW


if __name__ == "__main__":
    sys.exit(main())
