"""What kind of thing each MCP tool does — read from `tools.json`, the one list of every tool.

A server's `_tool` registrar calls `annotations_for(name)`: a tool missing from `tools.json` fails at
import, so a new tool cannot ship unclassified, and every other list that has to know about tools (the
user-facing table in frontend/src/lib/claudeTools.ts, the ✦ pages, Kiwi's allow-list) is checked
against the same file instead of pattern-matching the server's source.
"""
from __future__ import annotations

import json
from pathlib import Path

_DATA = json.loads((Path(__file__).with_name("tools.json")).read_text(encoding="utf-8"))
KINDS: dict[str, str] = _DATA["kinds"]
OBSERVER: dict[str, str] = _DATA["observer"]
AUTONOMOUS: dict[str, str] = _DATA["autonomous"]   # its OWN tools; it also registers the observer's gating + figure sets
TOOL_KINDS: dict[str, str] = {**OBSERVER, **AUTONOMOUS}

# Kinds whose change is not purely additive: an `edit` replaces or removes something, a `run` writes
# task output. MCP defaults destructiveHint to true for any non-read-only tool, so the additive kinds
# must say false explicitly.
_DESTRUCTIVE = {"edit", "run"}


def kind_of(name: str) -> str:
    try:
        return TOOL_KINDS[name]
    except KeyError:
        raise KeyError(f"MCP tool {name!r} is not in mcp/cecelia_mcp/tools.json — add it with its kind "
                       f"({', '.join(KINDS)})") from None


def annotations_for(name: str) -> ToolAnnotations:
    """MCP's standard hints for a tool, from its kind. Everything here talks only to the local app."""
    from mcp.types import ToolAnnotations   # the SDK only where a server registers; the lists need none
    kind = kind_of(name)
    if kind == "read":
        return ToolAnnotations(readOnlyHint=True, openWorldHint=False)
    return ToolAnnotations(readOnlyHint=False, destructiveHint=kind in _DESTRUCTIVE, openWorldHint=False)
