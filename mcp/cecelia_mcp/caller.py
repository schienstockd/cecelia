"""Which model + Claude Code session is making the current MCP call — the server half of `caller_hook.py`.

The hook adds `cecelia_caller_model` / `cecelia_caller_session` to a tool call's arguments.
`CallerMCPServer.call_tool` pops them BEFORE the tool's argument validation (so no tool declares them,
and a model can't fill them in itself — they are not in any tool schema), holds them in a ContextVar for
the length of the call, and `client.http_json` sends them as `X-Cecelia-Model` / `X-Cecelia-Session`.
The API puts them in `author_stamp()`.

Values are checked here AND on the API side: they come from a file on disk, travel in a header, and
end up in project JSON. Anything off-shape is dropped — an unknown model is recorded as absent.
"""
from __future__ import annotations

import contextvars
import re
from typing import Any

from mcp.server.mcpserver import MCPServer

from cecelia_mcp.caller_hook import MODEL_FIELD, SESSION_FIELD

MODEL_HEADER = "X-Cecelia-Model"
SESSION_HEADER = "X-Cecelia-Session"

_MODEL_RE = re.compile(r"^[A-Za-z0-9._:\[\]-]{1,100}$")
_SESSION_RE = re.compile(r"^[A-Za-z0-9-]{1,64}$")

_CALLER: contextvars.ContextVar[dict[str, str]] = contextvars.ContextVar("cecelia_caller", default={})


def pop_caller(arguments: dict[str, Any]) -> tuple[dict[str, Any], dict[str, str]]:
    """`(arguments without the caller fields, {model?, session?})` — the valid values only."""
    args = dict(arguments)
    model = args.pop(MODEL_FIELD, None)
    session = args.pop(SESSION_FIELD, None)
    caller = {}
    if isinstance(model, str) and _MODEL_RE.match(model):
        caller["model"] = model
    if isinstance(session, str) and _SESSION_RE.match(session):
        caller["session"] = session
    return args, caller


def caller_headers() -> dict[str, str]:
    """The headers for the call in progress — empty outside a call, or when the hook isn't installed."""
    c = _CALLER.get()
    out = {}
    if "model" in c:
        out[MODEL_HEADER] = c["model"]
    if "session" in c:
        out[SESSION_HEADER] = c["session"]
    return out


class CallerMCPServer(MCPServer):
    """`MCPServer` that takes the caller fields off every tool call. Sync tools run in a worker thread
    (`anyio.to_thread.run_sync`), which copies the context — so the ContextVar set here reaches them."""

    async def call_tool(self, name, arguments, context=None):
        args, caller = pop_caller(arguments or {})
        token = _CALLER.set(caller)
        try:
            return await super().call_tool(name, args, context)
        finally:
            _CALLER.reset(token)
