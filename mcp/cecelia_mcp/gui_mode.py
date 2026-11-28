"""GUI mode: the autonomous server inside a GUI user's own Claude Code session — the Cecelia plugin
starts it (`mcp/claude-plugin/cecelia/.claude-plugin/plugin.json`) beside the observer, so Claude can START
a task or chain it designed with the user instead of handing it over for the Run button
(docs/todo/AGENT_GUI_RUNS_PLAN.md).

`gui_mode_enabled()` is THE reader of `CECELIA_MCP_GUI` (`1` on, unset or `0` off), as `no_gui_enabled()`
is of its switch. In GUI mode the autonomous server:

- locks each call to the project OPEN in Cecelia (the most recently opened one), checked per call, so a
  switch in the app moves the lock with it;
- refuses a run that writes an image version or a label set (segmentation, measuring, tracking, image
  processing): those change what the user's images open on;
- refuses a run that would replace an existing output, naming a free name instead
  (`POST /api/tasks/write-check`);
- offers only the run and wait tools — gating and figures stay the observer's.

Approval is Claude Code's own per-call prompt: nothing here pre-approves a tool.
"""
from __future__ import annotations

import os

from cecelia_mcp.no_gui import no_gui_enabled

GUI_ENV = "CECELIA_MCP_GUI"


def gui_mode_enabled() -> bool:
    """True when `CECELIA_MCP_GUI=1`. Any value other than `1` / `0` / unset raises, and so does GUI mode
    together with no-GUI mode: a typo or a double switch must fail at server start, not run as the
    wrong mode."""
    v = os.environ.get(GUI_ENV, "").strip()
    if v in ("", "0"):
        return False
    if v != "1":
        raise ValueError(f"{GUI_ENV}={v!r}: expected '1' or '0'")
    if no_gui_enabled():
        raise ValueError(f"{GUI_ENV}=1 and CECELIA_MCP_NO_GUI=1 together: a session is one or the other")
    return True


def write_refusals(fun_name: str, check: dict) -> list[str]:
    """Why a GUI-mode run is refused, from `/api/tasks/write-check`'s answer. Empty = allowed."""
    out = check.get("output") or {}
    name = str(out.get("name") or "")
    if not name:
        return []
    ns, into = str(out.get("ns") or ""), bool(out.get("into"))
    bad = []
    # a fixed `default` that is not written into is a NEW image (bin, crop, projections) — allowed
    if ns == "labels" or (ns == "" and (into or name != "default")):
        what = "label set" if ns == "labels" else "image version"
        bad.append(f"{fun_name} writes {'into ' if into else ''}the {what} {name!r}, which changes what "
                   "the user's images open on — design it (create_chain) and let the user run it")
    reps = check.get("replaces") or {}
    if reps:
        free, key = check.get("free"), out.get("key")
        hint = f"; use {key}={free!r} instead" if free and key else ""
        bad.append(f"{fun_name} would replace the existing {name!r} on {len(reps)} image(s) "
                   f"({', '.join(sorted(reps))}){hint}")
    return bad
