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


def _stop_gracefully(proc, timeout: float = 20.0) -> bool:
    """Ask the server to stop ITS OWN children, then exit. True if it did.

    `proc.terminate()` kills the Julia server and nothing else. The server is the parent of three
    resident processes — the napari bridge (:7655), the task-preview worker (:7656) and the Pluto
    notebooks server (:7660) — and they are grandchildren in their own process groups, so they survive
    it. That left them running with no backend able to reach them: the preview worker in particular
    holds a warm cellpose model's VRAM, and an orphan is then silently ADOPTED by the next launch, which
    is how a worker running stale code outlived several restarts.

    `POST /api/app/shutdown` already stops all three and then exits, and it is the path the in-app Quit
    button uses — so this REUSES it rather than adding a third copy of platform-specific port-killing
    (Julia has one in `_kill_listeners_on_port`, the dev supervisor another in `api/dev.jl::_free_port`).
    Failure just falls through to terminate/kill, which is where this always ended up.
    """
    try:
        req = urllib.request.Request(
            f"{URL}/api/app/shutdown", data=b"{}",
            headers={"Content-Type": "application/json"}, method="POST")
        with urllib.request.urlopen(req, timeout=5) as resp:
            if resp.status != 200:
                return False
    except Exception:
        return False          # hung, already gone, or too early to have a server — not worth reporting
    try:
        proc.wait(timeout=timeout)
        return True
    except subprocess.TimeoutExpired:
        return False          # it accepted the request but did not exit; caller escalates


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


# The server exits with this code to ask its supervisor (us) to relaunch it — Settings → System →
# Restart (POST /api/app/restart). Mirrors the dev.jl supervisor loop. Any other exit → we stop.
RESTART_EXIT_CODE = 42


def main() -> int:
    # Production mode: plain include, no Revise. Inherits PATH from the activated env so the
    # server's Python subprocesses use the same env. CECELIA_SUPERVISED tells the server that
    # backend restart is available (we relaunch it on RESTART_EXIT_CODE).
    env = {**os.environ, "CECELIA_SUPERVISED": "1"}
    first = True
    while True:
        # Apply staged updates every iteration, not just at first launch — Settings → System Restart
        # re-enters this loop with the Julia backend down, which is the one moment we can swap
        # api/src/*.jl without a running process locking them.
        _apply_pending_update()
        proc = subprocess.Popen(
            [_find_julia(), "--project", "src/server.jl"],
            cwd=os.path.join(ROOT, "api"),
            env=env,
        )
        try:
            print(f"Starting Cecelia… (waiting for {HEALTH})")
            if _server_ready():
                if first:
                    webbrowser.open(URL)   # only pop a browser on the initial launch, not each restart
                    first = False
                print(f"Cecelia is running at {URL} — close this window to stop.")
            else:
                print("Cecelia server did not become ready in time.", file=sys.stderr)
                proc.terminate()
                return 1
            rc = proc.wait()
            if rc == RESTART_EXIT_CODE:
                print("Restarting Cecelia…")
                continue
            return 0
        except KeyboardInterrupt:
            return 0
        finally:
            # Ctrl-C, a crash, or the window being closed all land here. Ask the server to take its
            # children down with it first (see `_stop_gracefully`); terminate/kill only if that fails,
            # which is what this did unconditionally before — and which orphaned all three.
            if proc.poll() is None and not _stop_gracefully(proc):
                proc.terminate()
                try:
                    proc.wait(timeout=10)
                except subprocess.TimeoutExpired:
                    proc.kill()


if __name__ == "__main__":
    raise SystemExit(main())
