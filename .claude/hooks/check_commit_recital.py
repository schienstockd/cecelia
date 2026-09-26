#!/usr/bin/env python3
"""PreToolUse hook — every reviewer finding in a `git commit` recital must carry an outcome tag.

Wired via `.claude/settings.json` under `hooks.PreToolUse` matching the `Bash` tool. Fires on
every Bash call; short-circuits to allow unless the command contains `git commit`.

What it enforces
----------------
The convention-check reviewer is advisory by design (docs/todo/CONVENTION_CHECK_PLAN.md locked
decision #4) — findings land in the reservations recital but do not block a commit. That is
toothless in autonomous mode: nothing challenges a `**should reuse`` finding that the agent
silently commits over. Sibling-call is the same shape for its own findings.

This hook is the presence-check: for every `**confirmed**` sibling / `**should reuse**`
convention finding line the recital carries, the message must ALSO carry an outcome tag from
the closed vocabulary — `fixed_pre_commit`, `shipped_with_finding: <reason>`, or
`false_positive: <reason>`. Missing one blocks the commit with an explanation.

The check is deliberately mechanical text-matching, not semantic judgment: it does NOT validate
whether the outcome itself is honest (an agent could write `false_positive: reasons` untruthfully),
just that the disclosure step happened. That makes it cheap and reliable, and it is a
fundamentally different cost profile from the fuzzy-inventory-match gate previously declined.

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
#: sibling-call + convention-check both caught on this hook's first draft.
_REPO_ROOT = pathlib.Path(__file__).resolve().parents[2]
sys.path.insert(0, str(_REPO_ROOT / "python"))
from cecelia.effectiveness import OUTCOME_VOCABULARY  # noqa: E402

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
_OUTCOME_TAGS = re.compile(
    rf"\b(?:(?:{_alternation_standalone})\b|(?:{_alternation_reason}):\s*\S)"
)


def _outcome_help() -> str:
    """Build the help text listing every outcome, so a new vocabulary entry appears here for free."""
    lines = []
    for tag in sorted(_STANDALONE_OUTCOMES):
        lines.append(f"  - `{tag}`")
    for tag in sorted(_REASON_OUTCOMES):
        lines.append(f"  - `{tag}: <reason>`")
    return "\n".join(lines)

#: The command has to actually be a `git commit` — subshells count (`cd path && git commit …`).
_GIT_COMMIT = re.compile(r"\bgit\s+commit\b")

_EXIT_ALLOW = 0
_EXIT_BLOCK = 2  # PreToolUse convention: non-zero = block; stderr shown to Claude.


def _load_tool_call() -> dict:
    """Read the hook's stdin JSON. Return {} on malformed input so we degrade to allow rather
    than block on our own bug."""
    try:
        return json.load(sys.stdin)
    except (json.JSONDecodeError, ValueError):
        return {}


def check(command: str) -> str | None:
    """Return None if the commit is allowed; else a human-readable reason string."""
    if not _GIT_COMMIT.search(command):
        return None

    findings = _FINDING_MARKERS.findall(command)
    outcomes = _OUTCOME_TAGS.findall(command)

    if len(findings) <= len(outcomes):
        return None

    return (
        f"Recital carries {len(findings)} reviewer finding(s) marked "
        f"**confirmed** / **should reuse** but only {len(outcomes)} outcome tag(s). "
        "Every finding needs one:\n"
        f"{_outcome_help()}\n"
        "Add the outcome tag to each finding line, then retry. "
        "Bypass in emergencies with `CECELIA_SKIP_RECITAL_CHECK=1`."
    )


def main() -> int:
    if os.environ.get("CECELIA_SKIP_RECITAL_CHECK") == "1":
        return _EXIT_ALLOW

    data = _load_tool_call()
    if data.get("tool_name") != "Bash":
        return _EXIT_ALLOW

    command = data.get("tool_input", {}).get("command", "")
    reason = check(command)
    if reason is None:
        return _EXIT_ALLOW

    print(f"check_commit_recital: BLOCKED — {reason}", file=sys.stderr)
    return _EXIT_BLOCK


if __name__ == "__main__":
    sys.exit(main())
