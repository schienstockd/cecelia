"""Render named Analysis boards of a project to PNGs with the app's OWN frontend, headless.

A fresh Chromium (throwaway profile — never the user's browser, its localStorage or its open project)
loads the frontend build from `frontend/dist`, served straight from disk through the DevTools Fetch
domain, so nothing is started and the rendering code is this checkout's. API calls go to the running
app. The page is `modules/BoardRenderView.vue`: it opens the project VIEW-ONLY and returns each slot's
plot-only light-theme PNG through the board's PDF export path (`LayoutCanvas.capturePage`).

The session must not write. Every non-GET API call is refused unless it is one of `READ_POSTS` (reads
that happen to be POSTs, and the card routes, which only fill the project's own render cache), and
refused calls are reported, so a frontend that starts writing from a board shows up in the result.

    pixi run python scripts/agent_eval/board_render.py RkJd6s "Board name" [--out DIR]
"""
from __future__ import annotations

import argparse
import base64
import json
import mimetypes
import os
import pathlib
import re
import shutil
import subprocess
import sys
import tempfile
import threading
import time
import urllib.parse

from websockets.sync.client import connect

REPO = pathlib.Path(__file__).resolve().parents[2]
sys.path.insert(0, str(REPO / "mcp"))
from cecelia_mcp.auth import auth_headers  # noqa: E402 — the API only answers its token
DIST = REPO / "frontend" / "dist"
# POSTs a board's plots make that read (or only fill the copy's own card cache)
READ_POSTS = ("/api/plot_data", "/api/labels/by_category", "/api/cell_cards", "/api/motif_cards",
              "/api/hmm_state_cards")
# big enough that an A4-locked board (width = height × page aspect) gives each slot a figure-sized box
VIEWPORT = {"width": 2200, "height": 2400, "deviceScaleFactor": 1, "mobile": False}


class RenderError(RuntimeError):
    pass


def chromium_bin() -> str:
    for c in (os.environ.get("CECELIA_CHROMIUM"), "chromium", "chromium-browser", "google-chrome", "chrome"):
        if c and shutil.which(c):
            return shutil.which(c)
    raise RenderError("no Chromium found (set CECELIA_CHROMIUM)")


def ensure_dist(dist: pathlib.Path = DIST) -> pathlib.Path:
    """`frontend/dist`, (re)built when missing or older than the frontend sources."""
    index = dist / "index.html"
    src = REPO / "frontend" / "src"
    newest = max((p.stat().st_mtime for p in src.rglob("*") if p.is_file()), default=0)
    if index.exists() and index.stat().st_mtime >= newest:
        return dist
    npx = shutil.which("npx")
    if not npx:
        raise RenderError("no npx to build the frontend")
    r = subprocess.run([npx, "vite", "build", "--outDir", str(dist), "--emptyOutDir"], cwd=str(REPO / "frontend"),
                       capture_output=True, text=True, encoding="utf-8", timeout=900)
    if r.returncode != 0 or not index.exists():
        raise RenderError("frontend build failed: " + (r.stderr or r.stdout)[-400:])
    return dist


class _Cdp:
    """A minimal DevTools client: one websocket, one flat page session, Fetch events served inline."""

    def __init__(self, ws, api: str, dist: pathlib.Path):
        self.ws = ws
        self.api = urllib.parse.urlsplit(api)
        self.dist = dist
        self.n = 0
        self.session = None
        self.blocked: list[str] = []
        self.errors: list[str] = []      # the page's own errors, for a render that comes back empty

    def call(self, method: str, params: dict | None = None, timeout: float = 60, session=True) -> dict:
        self.n += 1
        mine = self.n          # serving a paused request below sends (and numbers) messages of its own
        msg = {"id": mine, "method": method, "params": params or {}}
        if session and self.session:
            msg["sessionId"] = self.session
        self.ws.send(json.dumps(msg))
        deadline = time.monotonic() + timeout
        while True:
            left = deadline - time.monotonic()
            if left <= 0:
                raise RenderError(f"{method} timed out after {timeout:.0f} s")
            m = json.loads(self.ws.recv(timeout=left))
            if m.get("id") == mine:
                if "error" in m:
                    raise RenderError(f"{method}: {m['error'].get('message')}")
                return m.get("result") or {}
            if m.get("method") == "Fetch.requestPaused":
                self._serve(m["params"])
            elif m.get("method") == "Runtime.exceptionThrown":
                self.errors.append(json.dumps(m["params"].get("exceptionDetails", {}))[:300])
            elif m.get("method") == "Runtime.consoleAPICalled" and m["params"].get("type") == "error":
                self.errors.append(" ".join(str(a.get("value", a.get("description", "")))
                                            for a in m["params"].get("args", []))[:300])

    def _serve(self, p: dict):
        req, rid = p["request"], p["requestId"]
        u = urllib.parse.urlsplit(req["url"])
        if u.path.startswith("/api/"):
            if req["method"] in ("GET", "HEAD", "OPTIONS") or u.path in READ_POSTS:
                return self._send("Fetch.continueRequest", {"requestId": rid})
            self.blocked.append(f"{req['method']} {u.path}")
            return self._send("Fetch.fulfillRequest", {"requestId": rid, "responseCode": 403, "body": base64.b64encode(
                b'{"error":"read-only render session"}').decode()})
        f = (self.dist / u.path.lstrip("/")).resolve()
        if u.path in ("", "/") or not f.is_file() or self.dist.resolve() not in f.parents:
            f = self.dist / "index.html"
        ctype = mimetypes.guess_type(f.name)[0] or "application/octet-stream"
        self._send("Fetch.fulfillRequest", {"requestId": rid, "responseCode": 200,
                                            "responseHeaders": [{"name": "Content-Type", "value": ctype}],
                                            "body": base64.b64encode(f.read_bytes()).decode()})

    def _send(self, method: str, params: dict):
        self.n += 1
        self.ws.send(json.dumps({"id": self.n, "method": method, "params": params, "sessionId": self.session}))

    def evaluate(self, expr: str, timeout: float = 60):
        r = self.call("Runtime.evaluate", {"expression": expr, "awaitPromise": True, "returnByValue": True},
                      timeout=timeout)
        if "exceptionDetails" in r:
            raise RenderError("page error: " + json.dumps(r["exceptionDetails"])[:300])
        return (r.get("result") or {}).get("value")


def _launch(profile: str):
    proc = subprocess.Popen([chromium_bin(), "--headless=new", "--remote-debugging-port=0",
                             f"--user-data-dir={profile}", "--no-first-run", "--no-default-browser-check",
                             "--disable-extensions", "--use-angle=swiftshader", "--enable-unsafe-swiftshader",
                             "about:blank"], stdout=subprocess.DEVNULL, stderr=subprocess.PIPE, text=True,
                            encoding="utf-8", errors="replace")
    found: list[str] = []
    ready = threading.Event()

    def drain():                     # keep reading, or a chatty Chromium blocks on a full pipe
        for line in proc.stderr:
            m = re.search(r"DevTools listening on (ws://\S+)", line)
            if m and not found:
                found.append(m.group(1))
                ready.set()
    threading.Thread(target=drain, daemon=True).start()
    if not ready.wait(60):
        proc.kill()
        raise RenderError("Chromium did not start")
    return proc, found[0]


def _session(cdp: _Cdp, api: str, project_uid: str, boards: list[str], image_uids: list[str],
             timeout_s: float) -> dict:
    target = cdp.call("Target.createTarget", {"url": "about:blank"}, session=False)["targetId"]
    cdp.session = cdp.call("Target.attachToTarget", {"targetId": target, "flatten": True},
                           session=False)["sessionId"]
    cdp.call("Network.enable")
    cdp.call("Network.setBlockedURLs", {"urls": ["ws://*", "wss://*"]})   # no live session: no pairing, no pushes
    cdp.call("Fetch.enable", {"patterns": [{"urlPattern": f"{api.rstrip('/')}/*"}]})
    # The API only answers its own user (app/src/api_token.jl): give this browser the sign-in cookie a
    # launch link would have set, so the page's own /api fetches pass the gate.
    token = auth_headers().get("Authorization", "").removeprefix("Bearer ")
    cdp.call("Network.setCookie", {"name": f"cecelia_auth_{urllib.parse.urlsplit(api).port or 80}",
                                   "value": token, "url": api.rstrip("/") + "/", "httpOnly": True})
    cdp.call("Emulation.setDeviceMetricsOverride", VIEWPORT)
    q = urllib.parse.urlencode({"project": project_uid, "images": ",".join(image_uids)})
    cdp.call("Page.enable")
    cdp.call("Runtime.enable")
    cdp.call("Page.navigate", {"url": f"{api.rstrip('/')}/#/board-render?{q}"})
    deadline = time.monotonic() + 120
    while not cdp.evaluate("!!(window.__ccBoardRender && window.__ccBoardRender.ready)"):
        if time.monotonic() > deadline:
            where = cdp.evaluate("location.href + ' — ' + (document.body ? document.body.innerText.slice(0, 200) : '')")
            raise RenderError(f"the render page never became ready (is the app up?) at {where}; "
                              f"page errors: {cdp.errors[:3]}")
        time.sleep(0.2)
    err = cdp.evaluate("window.__ccBoardRender.error || ''")
    if err:
        raise RenderError(err)
    out: dict = {"boards": {}}
    for name in boards:
        try:
            # through JSON: a by-value object holding several MB of data URLs came back empty
            got = json.loads(cdp.evaluate(
                f"window.__ccBoardRender.render({json.dumps(name)}).then(r => JSON.stringify(r))",
                timeout=timeout_s) or "null") or {"ok": False, "error": "the page returned nothing", "slots": []}
        except RenderError as e:
            got = {"ok": False, "error": str(e), "slots": []}
        if not got.get("ok") and cdp.errors:
            got["error"] = f"{got.get('error')}; page errors: {cdp.errors[-3:]}"
        for s in got.get("slots") or []:
            png = s.get("png") or ""
            s["png"] = png.split(",", 1)[1] if png.startswith("data:image/png;base64,") else None
        out["boards"][name] = got
    return out


def render_boards(api: str, project_uid: str, boards: list[str], image_uids: list[str] | None = None,
                  dist: pathlib.Path | None = None, timeout_s: float = 300) -> dict:
    """{"boards": {name: {"ok", "error"?, "slots": [{index, name, title?, png (base64)}]}}, "blocked": [...]}.
    Raises RenderError when nothing could be rendered at all (no browser, no build, app down)."""
    dist = ensure_dist(dist or DIST)
    profile = tempfile.mkdtemp(prefix="cc-board-render-")
    proc, ws_url = _launch(profile)
    try:
        with connect(ws_url, max_size=None, open_timeout=30) as ws:
            cdp = _Cdp(ws, api, dist)
            out = _session(cdp, api, project_uid, boards, image_uids or [], timeout_s)
            out["blocked"] = sorted(set(cdp.blocked))
            return out
    finally:
        proc.terminate()
        try:
            proc.wait(10)
        except subprocess.TimeoutExpired:
            proc.kill()
        shutil.rmtree(profile, ignore_errors=True)


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("project")
    ap.add_argument("boards", nargs="+")
    ap.add_argument("--api-url", default=os.environ.get("CECELIA_API_URL", "http://127.0.0.1:8080"))
    ap.add_argument("--images", default="")
    ap.add_argument("--out", default=None, help="write <board>-<slot>.png here")
    a = ap.parse_args(argv)
    r = render_boards(a.api_url, a.project, a.boards, [x for x in a.images.split(",") if x])
    if a.out:
        out = pathlib.Path(a.out).expanduser()
        out.mkdir(parents=True, exist_ok=True)
        for name, b in r["boards"].items():
            for s in b.get("slots") or []:
                if s["png"]:
                    stem = re.sub(r"[^\w.-]+", "_", f"{name}-{s['index'] + 1}-{s['name']}")
                    (out / f"{stem}.png").write_bytes(base64.b64decode(s["png"]))
    print(json.dumps({n: {"ok": b.get("ok"), "error": b.get("error"), "slots": [
        (s["name"], bool(s["png"])) for s in b.get("slots") or []]} for n, b in r["boards"].items()}
        | {"blocked": r["blocked"]}, indent=1))
    return 0


if __name__ == "__main__":
    sys.exit(main())
