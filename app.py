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
import shutil
import sys
import time
import subprocess
import urllib.request
import webbrowser

ROOT = os.path.dirname(os.path.abspath(__file__))


def _find_julia() -> str:
    """Resolve the Julia binary. A GUI-launched desktop shortcut may not have juliaup on PATH,
    so fall back to its default install location."""
    found = shutil.which("julia")
    if found:
        return found
    candidate = os.path.expanduser("~/.juliaup/bin/julia")
    return candidate if os.path.exists(candidate) else "julia"
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


def _apply_pending_update() -> None:
    """Apply an update staged by a previous run (the `.pending-update` marker + `.update-staging/
    payload`), before the server starts — when nothing is using the files. Best-effort: logs and
    continues with the current version on any error."""
    pending = os.path.join(ROOT, ".pending-update")
    if not os.path.exists(pending):
        return
    payload = os.path.join(ROOT, ".update-staging", "payload")
    try:
        tag = open(pending).read().strip()
        if os.path.isdir(payload):
            print(f"Applying staged update {tag}...")
            for item in os.listdir(payload):
                src, dst = os.path.join(payload, item), os.path.join(ROOT, item)
                if os.path.isdir(dst) and not os.path.islink(dst):
                    shutil.rmtree(dst, ignore_errors=True)
                elif os.path.exists(dst) or os.path.islink(dst):
                    os.remove(dst)
                shutil.move(src, dst)
        shutil.rmtree(os.path.join(ROOT, ".update-staging"), ignore_errors=True)
        os.remove(pending)
        # Deps may have changed (pixi.lock / Manifest) — re-provision before launch.
        pixi = shutil.which("pixi") or os.path.expanduser("~/.pixi/bin/pixi")
        print("Updating environment...")
        subprocess.run([pixi, "install"], cwd=ROOT, check=False)
        subprocess.run([_find_julia(), "--project=api", "-e", "using Pkg; Pkg.instantiate()"],
                       cwd=ROOT, check=False)
        print(f"Update {tag} applied.")
    except Exception as e:  # noqa: BLE001 — never block launch on a failed update
        print(f"Update could not be applied ({e}); continuing with the current version.",
              file=sys.stderr)


def main() -> int:
    _apply_pending_update()
    # Production mode: plain include, no Revise. Inherits PATH from the activated env so the
    # server's Python subprocesses use the same env.
    proc = subprocess.Popen(
        [_find_julia(), "--project", "src/server.jl"],
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
