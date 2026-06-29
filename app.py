#!/usr/bin/env python3
"""Cecelia launcher.

Starts the Julia API server, waits until it answers `/api/health`, then opens the user's default
browser at http://localhost:8080. The Julia server serves the built Vue frontend at that same
origin, so the whole app is one URL — no separate frontend process in production.

This is the entrypoint behind both `pixi run app` and the desktop shortcut created by the
constructor installer (menuinst). It runs inside the Pixi/conda env, so `julia` and the Python
analysis stack the server spawns all resolve to that env. See docs/SHIPPING.md.

Close this window (or Ctrl-C) to stop the server.
"""
import os
import sys
import time
import subprocess
import urllib.request
import webbrowser

ROOT = os.path.dirname(os.path.abspath(__file__))
PORT = os.environ.get("CECELIA_PORT", "8080")
URL = f"http://localhost:{PORT}"
HEALTH = f"{URL}/api/health"


def _server_ready(timeout: float = 180.0) -> bool:
    deadline = time.time() + timeout
    while time.time() < deadline:
        try:
            with urllib.request.urlopen(HEALTH, timeout=2) as resp:
                if resp.status == 200:
                    return True
        except Exception:
            time.sleep(0.5)
    return False


def main() -> int:
    # Production mode: plain include, no Revise. Inherits PATH from the activated env so the
    # server's Python subprocesses use the same env.
    proc = subprocess.Popen(
        ["julia", "--project", "src/server.jl"],
        cwd=os.path.join(ROOT, "api"),
    )
    try:
        print(f"Starting Cecelia… (waiting for {HEALTH})")
        if _server_ready():
            webbrowser.open(URL)
            print(f"Cecelia is running at {URL} — close this window to stop.")
        else:
            print("Cecelia server did not become ready in time.", file=sys.stderr)
            proc.terminate()
            return 1
        proc.wait()
    except KeyboardInterrupt:
        pass
    finally:
        if proc.poll() is None:
            proc.terminate()
            try:
                proc.wait(timeout=10)
            except subprocess.TimeoutExpired:
                proc.kill()
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
