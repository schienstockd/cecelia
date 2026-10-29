"""Headless mode — Cecelia with no GUI, Claude in the terminal as the front end
(docs/todo/HEADLESS_MODE_PLAN.md).

`no_gui_enabled()` is THE reader of `CECELIA_MCP_NO_GUI` (`1` on, unset or `0` off). The headless
launcher sets it on both MCP servers. Every surface the mode changes asks this one function — the
autonomous server's project lock and its project/set/image creation tools here, the observer's
GUI-only tools — so the two servers can never disagree about which mode they are in.

Not called "headless" in the code: there, headless already means the app's own `claude -p` turns
(`observer_mcp_config(; headless)` in app/src/ai/agent_runner.jl). This mode is an interactive
session with the user at the keyboard.
"""
from __future__ import annotations

import os

NO_GUI_ENV = "CECELIA_MCP_NO_GUI"


def no_gui_enabled() -> bool:
    """True when `CECELIA_MCP_NO_GUI=1`. Unset or `0` is the default (a GUI is there).

    Any other value raises: a typo must fail loudly at server start rather than run silently as the
    other mode — with the GUI tools on and creation off, or the reverse."""
    v = os.environ.get(NO_GUI_ENV, "").strip()
    if v in ("", "0"):
        return False
    if v == "1":
        return True
    raise ValueError(f"{NO_GUI_ENV}={v!r}: expected '1' or '0'")
