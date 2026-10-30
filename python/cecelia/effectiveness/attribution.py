"""How a commit / PR names the Claude that wrote it — the repo-history twin of `author_stamp()`.

A stamp is `{model, version, session}` (the model id, the Claude Code version, the session id; any
unknown field left out, never guessed). Two producers: the commit/PR hook
(`.claude/hooks/claude_attribution.py`, from the calling agent's transcript) and the unattended
judge (`scripts/judge/auto_fix.py`, from its `claude -p` session's transcript). Both format here so
the lines match. Convention: docs/DEV.md → *Commits* / *Pull requests*.
"""
from __future__ import annotations

import re

FOOTER = "🤖 Generated with [Claude Code](https://claude.com/claude-code)"
_DATE_SUFFIX = re.compile(r"-\d{8}$")


def display_name(model: str) -> str:
    """`claude-opus-5-5` → `Claude Opus 5.5`; `claude-sonnet-4-5-20250929` → `Claude Sonnet 4.5`.
    Anything else is returned as given."""
    parts = _DATE_SUFFIX.sub("", model).split("-")
    if len(parts) < 2 or parts[0] != "claude":
        return model
    words = [p for p in parts[1:] if not p.isdigit()]
    nums = [p for p in parts[1:] if p.isdigit()]
    return " ".join(["Claude", *(w.capitalize() for w in words), *([".".join(nums)] if nums else [])])


def coauthor_line(stamp: dict) -> str:
    """The `Co-Authored-By` trailer, naming the model when the stamp knows it."""
    name = display_name(stamp["model"]) if stamp.get("model") else "Claude"
    return f"Co-Authored-By: {name} <noreply@anthropic.com>"


def trailer_lines(stamp: dict) -> list[str]:
    """`Claude-Model` / `Claude-Code` / `Claude-Session` trailers for the stamp's known fields."""
    return [f"{key}: {stamp[field]}" for key, field in
            (("Claude-Model", "model"), ("Claude-Code", "version"), ("Claude-Session", "session"))
            if stamp.get(field)]


def run_lines(models: list[str]) -> list[str]:
    """Commit lines for work several `claude -p` calls did with no one session to name (the weekly
    judge record): a co-author and a `Claude-Model` trailer per model id; none known → bare `Claude`."""
    if not models:
        return [coauthor_line({})]
    return [*(coauthor_line({"model": m}) for m in models), *(f"Claude-Model: {m}" for m in models)]


def footer(stamp: dict) -> str:
    """A PR body's last line: `FOOTER · <model id> · Claude Code <version>` (known parts only).
    A run on several models passes them joined: `{"model": ", ".join(models)}`."""
    parts = [FOOTER, stamp.get("model", "")]
    if stamp.get("version"):
        parts.append(f"Claude Code {stamp['version']}")
    return " · ".join(p for p in parts if p)
