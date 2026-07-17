"""WebSocket listener that feeds the SessionMonitor from the Julia API's event stream.

Deliberately thin: all it does is connect to ``ws://…/ws``, JSON-decode each frame, run it through
``normalize_frame``, and hand the result to the monitor. The signal logic lives in ``monitor.py``
(pure, tested there); the parse step is testable via ``feed_raw`` without opening a socket.

The MCP server (stdio) runs FastMCP's own loop; this runs on a background daemon thread with its own
asyncio loop (see ``start_listener``). ``websockets`` is already a repo dependency (the napari bridge),
so no new dep. If the backend is down or drops, ``websockets.connect``'s async-iterator form
reconnects automatically — the observer is best-effort and never blocks the read tools.
"""
from __future__ import annotations

import asyncio
import json
import threading

import websockets

from cecelia_mcp.monitor import SessionMonitor, normalize_frame


def feed_raw(monitor: SessionMonitor, raw: str) -> dict | None:
    """Decode one raw WS message and fold it into the monitor. Returns the normalized event (or None
    if not tracked / not decodable). Pulled out so it can be unit-tested without a live socket."""
    try:
        frame = json.loads(raw)
    except (ValueError, TypeError):
        return None
    if not isinstance(frame, dict):
        return None
    event = normalize_frame(frame)
    if event:
        monitor.record(event)
    return event


def api_url_to_ws(api_url: str) -> str:
    """http://host:port → ws://host:port/ws  (https → wss)."""
    base = api_url.rstrip("/")
    if base.startswith("https://"):
        base = "wss://" + base[len("https://"):]
    elif base.startswith("http://"):
        base = "ws://" + base[len("http://"):]
    return base + "/ws"


async def observe(monitor: SessionMonitor, ws_url: str, *, stop: asyncio.Event | None = None) -> None:
    """Connect and stream frames into the monitor forever, reconnecting on drop. Best-effort."""
    async for ws in websockets.connect(ws_url, ping_interval=20, open_timeout=5):
        try:
            async for raw in ws:
                feed_raw(monitor, raw)
                if stop is not None and stop.is_set():
                    return
        except websockets.ConnectionClosed:
            continue  # reconnect
        if stop is not None and stop.is_set():
            return


def start_listener(monitor: SessionMonitor, ws_url: str) -> threading.Thread:
    """Run ``observe`` on a background daemon thread with its own event loop. Returns the thread.

    Daemon so it never keeps the process alive past FastMCP's stdio loop. Failures are swallowed —
    the observer is an add-on; the read tools must work even if the event stream never connects."""
    def _run():
        try:
            asyncio.run(observe(monitor, ws_url))
        except Exception:  # noqa: BLE001 — best-effort background listener, never crash the server
            pass

    thread = threading.Thread(target=_run, name="cecelia-observer-ws", daemon=True)
    thread.start()
    return thread
