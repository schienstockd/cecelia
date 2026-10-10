#!/usr/bin/env python3
"""Cecelia launcher.

Starts the Julia API server, waits until it answers `/api/health`, then opens the user's default
browser at http://localhost:8080 (or the next free port slot — see app/src/ports.jl). The Julia server serves the built Vue frontend at that same
origin, so the whole app is one URL — no separate frontend process in production.

This is the entrypoint behind both `pixi run app` and the desktop shortcut created by the
constructor installer (menuinst). It runs inside the Pixi/conda env, so `julia` and the Python
analysis stack the server spawns all resolve to that env. See docs/SHIPPING.md.

Close this window (or Ctrl-C) to stop the server.
"""
import hashlib
import json
import os
import shutil
import ssl
import sys
import time
import subprocess
import urllib.request
import webbrowser

ROOT = os.path.dirname(os.path.abspath(__file__))


def _exe(name: str) -> str:
    """A binary's file name on this platform: `julia` -> `julia.exe` on Windows. Only for paths we
    build ourselves — `shutil.which` already applies PATHEXT, but `os.path.exists` on a hand-built
    `~/.juliaup/bin/julia` is false on Windows, where install.ps1 puts `julia.exe`."""
    return name + ".exe" if sys.platform == "win32" else name


def _find_julia() -> str:
    """Resolve the Julia binary. A GUI-launched desktop shortcut may not have juliaup on PATH,
    so fall back to its default install location.

    A juliaup inside the install (`<root>/juliaup`) wins. install.sh puts one there for system scope,
    and on Apple Silicon when the Julia it found was an Intel build. That juliaup keeps its own state,
    so point JULIAUP_DEPOT_PATH at it, and put it first on PATH for anything that runs bare `julia`.
    Every `pixi run` already gets this from scripts/activate_juliaup.sh; this covers a launch that bypasses pixi.

    A system-scope install also has a shared package depot (`<root>/juliaup/depot`), read-only to every
    account but the owner. A per-user writable depot goes in front of it, and the trailing empty entry
    keeps Julia's bundled stdlib depot — without both, Julia precompiles into the read-only depot and dies."""
    private = os.path.join(ROOT, "juliaup")
    bin_dir = os.path.join(private, "bin")
    if os.path.exists(os.path.join(bin_dir, _exe("julia"))):
        os.environ["JULIAUP_DEPOT_PATH"] = private
        shared = os.path.join(private, "depot")
        if os.path.isdir(shared):
            user_depot = os.path.join(os.path.expanduser("~"), ".cecelia", "julia-depot")
            os.environ["JULIA_DEPOT_PATH"] = os.pathsep.join([user_depot, shared, ""])
        if not os.environ.get("PATH", "").startswith(bin_dir + os.pathsep):
            os.environ["PATH"] = bin_dir + os.pathsep + os.environ.get("PATH", "")
        return os.path.join(bin_dir, _exe("julia"))
    found = shutil.which("julia")
    if found:
        return found
    candidate = os.path.join(os.path.expanduser("~"), ".juliaup", "bin", _exe("julia"))
    return candidate if os.path.exists(candidate) else "julia"


def _find_pixi() -> str:
    """Resolve the Pixi binary — same order as `_find_pixi` in api/src/pixi_bin.jl: `PIXI_EXE`
    (exported by `pixi run`), PATH, then install.sh's locations (system scope `<root>/pixi/bin`,
    `$PIXI_HOME/bin`, `~/.pixi/bin`). Nothing found → the `~/.pixi` path, so the caller's error names it."""
    from_run = os.environ.get("PIXI_EXE", "").strip()
    if from_run and os.path.isfile(from_run):
        return from_run
    found = shutil.which("pixi")
    if found:
        return found
    pixi_home = os.environ.get("PIXI_HOME", "")
    user = os.path.join(os.path.expanduser("~"), ".pixi", "bin", _exe("pixi"))
    for cand in (os.path.join(ROOT, "pixi", "bin", _exe("pixi")),
                 os.path.join(pixi_home, "bin", _exe("pixi")) if pixi_home else "",
                 user):
        if cand and os.path.isfile(cand):
            return cand
    return user


def _config_dir() -> str:
    """The per-user dir holding `custom.toml`: the Python twin of `config_dir` in app/src/config.jl,
    same order — `CECELIA_DEV_DIR` env, then `CECELIA_DEV_DIR` in `<root>/.env`, then `~/.cecelia`
    (the installed app has neither, so it lands there)."""
    val = os.environ.get("CECELIA_DEV_DIR")
    if not val:
        try:
            with open(os.path.join(ROOT, ".env"), encoding="utf-8") as f:
                for line in f:
                    key, sep, rest = line.strip().partition("=")
                    if sep and key == "CECELIA_DEV_DIR":
                        val = rest.strip()
        except OSError:
            pass
    # normpath: `~/x` expands to `C:\Users\u/x` on Windows; Julia's `expand_user` canonicalises it.
    return os.path.normpath(os.path.expanduser(val or os.path.join("~", ".cecelia")))


def _multithreaded_setting() -> bool:
    """`[server] multithreaded` in custom.toml — Settings → System → "Use all CPU cores". Default ON.
    Julia owns the write (`set_api_multithreaded!`, app/src/config/server_threads.jl); this is the
    launch-time read, because a Julia process cannot change its thread count once started. An
    unreadable file never blocks a launch: it reads as the default."""
    try:
        import tomllib
        with open(os.path.join(_config_dir(), "custom.toml"), "rb") as f:
            val = tomllib.load(f).get("server", {}).get("multithreaded", True)
        return val if isinstance(val, bool) else True
    except Exception:  # noqa: BLE001 — missing file, bad TOML, no tomllib: all mean "default"
        return True


def _thread_args() -> tuple:
    """The `-t` flag for the server, and what was applied (`auto` / `1` / `env`).

    The applied value is handed to the server as `CECELIA_LAUNCH_THREADS`, so Settings can tell
    "the setting says X but this process started with Y — restart to apply" without guessing from
    `Threads.nthreads()` (a 1-core box running `-t auto` also has one thread). An explicit
    `JULIA_NUM_THREADS` wins over the setting: no `-t`, so Julia reads the env var itself."""
    if os.environ.get("JULIA_NUM_THREADS"):
        return [], "env"
    return (["-t", "auto"], "auto") if _multithreaded_setting() else (["-t", "1"], "1")


def _file_digest(path: str) -> bytes:
    try:
        with open(path, "rb") as f:
            return hashlib.sha256(f.read()).digest()
    except OSError:
        return b""


def _reexec_if_launcher_changed(before: bytes, browser_opened: bool) -> None:
    """An update or revert can replace THIS file. Without a re-exec the old launcher keeps running
    until the next full relaunch, so a launcher fix (e.g. the thread flag) would not land on the
    in-app Restart that finishes an update. POSIX only: `os.execv` keeps the PID, so `pixi run`
    and the desktop shortcut still own us; on Windows it spawns a detached copy instead, so there
    the new launcher waits for the next launch."""
    if sys.platform == "win32" or _file_digest(os.path.abspath(__file__)) == before:
        return
    print("Launcher updated — reloading it…")
    if browser_opened:
        os.environ["CECELIA_LAUNCHER_NO_BROWSER"] = "1"
    sys.stdout.flush(); sys.stderr.flush()
    os.execv(sys.executable, [sys.executable, os.path.abspath(__file__), *sys.argv[1:]])


# The port the server took. Normally 8080, but the SERVER picks it: several users can each run Cecelia
# on one machine, and a later one gets the next port slot (app/src/ports.jl). `_launched_port` reads
# the choice back each launch; this initial value only covers the time before that.
PORT = os.environ.get("CECELIA_PORT", "8080")
# The server decides HTTP vs HTTPS from `[tls].enabled` / `CECELIA_TLS` (see app/src/config/tls.jl);
# prod defaults to HTTPS + HTTP/2, dev to HTTP/1.1, and a missing openssl silently falls back to HTTP.
# The launcher can't reproduce that resolution without shelling into Julia, so it probes both on the
# same port and remembers whichever answered. `URL` is set by `_server_ready`; the browser open and
# the shutdown POST both read it back so they use the same scheme the health check succeeded on.
URL = f"http://localhost:{PORT}"
# Self-signed loopback cert — verification would always fail. urllib's default HTTPSHandler enforces
# it, so we pass an unverified context explicitly for the probe.
_NOVERIFY = ssl._create_unverified_context()


def _probe(url: str) -> bool:
    try:
        opener = (urllib.request.build_opener(urllib.request.HTTPSHandler(context=_NOVERIFY))
                  if url.startswith("https:") else urllib.request.build_opener())
        with opener.open(url + "/api/health", timeout=2) as resp:
            return resp.status == 200
    except Exception:
        return False


def _launched_port(proc, launched_at: float, timeout: float = 180.0) -> str | None:
    """The API port the server we just started took, read from the single-instance lock it writes
    (`acquire_single_instance!`, app/src/single_instance.jl) before it binds. Only a lock written
    since this launch counts — a stale one from a crashed run names a port nobody is on. Not keyed on
    the PID: on Windows `julia` is the juliaup launcher, whose PID is not the server's. `None` when
    the server exits first (e.g. it refused because Cecelia is already running for this user)."""
    path = os.path.join(_config_dir(), "cecelia.lock")
    deadline = time.time() + timeout
    while time.time() < deadline and proc.poll() is None:
        try:
            if os.path.getmtime(path) >= launched_at - 1:
                with open(path, encoding="utf-8") as f:
                    port = json.load(f).get("api_port")
                if port:
                    return str(port)
        except (OSError, ValueError):
            pass                       # not written yet, or caught mid-write — look again
        time.sleep(0.5)
    return None


def _server_ready(timeout: float = 180.0) -> bool:
    """Probe HTTPS first, then HTTP, on the same port. Sets the module-level `URL` to whichever
    answered so downstream (browser open, shutdown POST) speaks the same scheme as the server."""
    global URL
    https_url = f"https://localhost:{PORT}"
    http_url  = f"http://localhost:{PORT}"
    deadline = time.time() + timeout
    while time.time() < deadline:
        for candidate in (https_url, http_url):
            if _probe(candidate):
                URL = candidate
                return True
        time.sleep(0.5)
    return False


def _stop_gracefully(proc, timeout: float = 20.0) -> bool:
    """Ask the server to stop ITS OWN children, then exit. True if it did.

    `proc.terminate()` kills the Julia server and nothing else. The server is the parent of two
    resident processes — the task-preview worker (:7656) and the Pluto notebooks server (:7660) —
    and they are grandchildren in their own process groups, so they survive it. That left them
    running with no backend able to reach them: the preview worker in particular holds a warm
    cellpose model's VRAM, and an orphan is then silently ADOPTED by the next launch, which is how
    a worker running stale code outlived several restarts.

    `POST /api/app/shutdown` already stops both and then exits, and it is the path the in-app Quit
    button uses — so this REUSES it rather than adding a third copy of platform-specific port-killing
    (Julia has one in `_kill_listeners_on_port`, the dev supervisor another in `api/dev.jl::_free_port`).
    Failure just falls through to terminate/kill, which is where this always ended up.
    """
    try:
        req = urllib.request.Request(
            f"{URL}/api/app/shutdown", data=b"{}",
            headers={"Content-Type": "application/json"}, method="POST")
        ctx = _NOVERIFY if URL.startswith("https:") else None
        with urllib.request.urlopen(req, timeout=5, context=ctx) as resp:
            if resp.status != 200:
                return False
    except Exception:
        return False          # hung, already gone, or too early to have a server — not worth reporting
    try:
        proc.wait(timeout=timeout)
        return True
    except subprocess.TimeoutExpired:
        return False          # it accepted the request but did not exit; caller escalates


def _reprovision_env() -> None:
    """Re-provision Pixi + Julia deps after either an apply or a revert — pixi.lock and Manifest may
    have moved in either direction. Both paths call this."""
    pixi = _find_pixi()
    print("Updating environment...")
    subprocess.run([pixi, "install"], cwd=ROOT, check=False)
    subprocess.run([_find_julia(), "--project=api", "-e", "using Pkg; Pkg.instantiate()"],
                   cwd=ROOT, check=False)


def _apply_pending_update() -> None:
    """Apply an update staged by a previous run (the `.pending-update` marker + `.update-staging/
    payload`), before the server starts — when nothing is using the files. Best-effort: logs and
    continues with the current version on any error.

    Snapshots the files it's about to overwrite into `.previous-release/payload/<item>`, so the
    Settings → Software → Revert button has something to roll back to. Only ONE step of history is
    kept (previous snapshot is discarded each apply)."""
    pending = os.path.join(ROOT, ".pending-update")
    if not os.path.exists(pending):
        return
    payload = os.path.join(ROOT, ".update-staging", "payload")
    prev_root = os.path.join(ROOT, ".previous-release")
    prev_payload = os.path.join(prev_root, "payload")
    try:
        tag = open(pending).read().strip()
        if os.path.isdir(payload):
            print(f"Applying staged update {tag}...")
            shutil.rmtree(prev_root, ignore_errors=True)
            os.makedirs(prev_payload)
            # Human-readable marker of what the user is reverting FROM. .cecelia-version is written
            # by install.sh (stable tag) and by the dev-channel apply (dev @ branch sha); absent on a
            # source checkout, which is fine — the revert message just says "previous release".
            cv = os.path.join(ROOT, ".cecelia-version")
            prev_tag = ""
            if os.path.isfile(cv):
                try:
                    with open(cv, "r", encoding="utf-8") as f:
                        prev_tag = f.read().strip()
                except Exception:
                    prev_tag = ""
            with open(os.path.join(prev_root, "marker"), "w", encoding="utf-8") as f:
                f.write(prev_tag)
            for item in os.listdir(payload):
                src, dst = os.path.join(payload, item), os.path.join(ROOT, item)
                # Move (not delete) the current version aside, so revert can restore it. `shutil.move`
                # handles files, directories AND symlinks; we only need to guard against a leftover
                # in the snapshot dir from a mid-apply crash on a previous run.
                snap = os.path.join(prev_payload, item)
                if os.path.exists(snap) or os.path.islink(snap):
                    if os.path.isdir(snap) and not os.path.islink(snap):
                        shutil.rmtree(snap, ignore_errors=True)
                    else:
                        os.remove(snap)
                if os.path.exists(dst) or os.path.islink(dst):
                    shutil.move(dst, snap)
                shutil.move(src, dst)
        shutil.rmtree(os.path.join(ROOT, ".update-staging"), ignore_errors=True)
        os.remove(pending)
        _reprovision_env()
        print(f"Update {tag} applied.")
    except Exception as e:  # noqa: BLE001 — never block launch on a failed update
        print(f"Update could not be applied ({e}); continuing with the current version.",
              file=sys.stderr)
        # Drop the marker so the app re-offers the update instead of "Restart to update" forever
        # (the server reads a present marker as "staged, waiting for restart").
        try:
            os.remove(pending)
        except OSError:
            pass


def _apply_pending_revert() -> None:
    """Roll back to the snapshot the previous `_apply_pending_update` left in `.previous-release/`.
    Same lifecycle as apply — nothing overwrites files while the server is running. Best-effort."""
    pending = os.path.join(ROOT, ".pending-revert")
    if not os.path.exists(pending):
        return
    prev_root = os.path.join(ROOT, ".previous-release")
    prev_payload = os.path.join(prev_root, "payload")
    try:
        tag = open(pending).read().strip()
        if not os.path.isdir(prev_payload):
            print(f"Revert requested to {tag or 'previous release'} but no snapshot on disk; skipping.",
                  file=sys.stderr)
            os.remove(pending)
            return
        print(f"Reverting to {tag or 'previous release'}...")
        for item in os.listdir(prev_payload):
            src, dst = os.path.join(prev_payload, item), os.path.join(ROOT, item)
            if os.path.isdir(dst) and not os.path.islink(dst):
                shutil.rmtree(dst, ignore_errors=True)
            elif os.path.exists(dst) or os.path.islink(dst):
                os.remove(dst)
            shutil.move(src, dst)
        shutil.rmtree(prev_root, ignore_errors=True)
        os.remove(pending)
        _reprovision_env()
        print(f"Revert to {tag or 'previous release'} applied.")
    except Exception as e:  # noqa: BLE001
        print(f"Revert could not be applied ({e}); continuing with the current version.",
              file=sys.stderr)
        try:
            os.remove(pending)   # same reason as the apply: a stuck marker reads as "restart to finish"
        except OSError:
            pass


# The server exits with this code to ask its supervisor (us) to relaunch it — Settings → System →
# Restart (POST /api/app/restart). Mirrors the dev.jl supervisor loop.
RESTART_EXIT_CODE = 42

# …and a CRASH is relaunched too, on the same rule dev.jl uses (`_crash_death` there — keep the two in
# step). Exit 0 is the in-app Quit and a negative rc is a signal we or the OS sent to stop it
# (SIGTERM/SIGINT/SIGKILL, i.e. the window closing or a `kill`), so neither comes back. Anything else
# — a nonzero exit, or a FAULT signal — is the server falling over, and a user whose two-hour
# segmentation is mid-flight would rather have the app come back than find a closed window. Bounded by
# CRASH_LIMIT inside CRASH_WINDOW so a server that cannot boot reports that instead of looping.
_FAULT_SIGNALS = (4, 6, 7, 8, 10, 11)   # ILL, ABRT, BUS (7 Linux / 10 macOS), FPE, SEGV
CRASH_LIMIT = 3
CRASH_WINDOW = 60.0


def _crashed(rc: int) -> bool:
    """Did the server FALL OVER (relaunch), rather than being asked to stop (don't)?

    `Popen.wait` returns `-N` for a process killed by signal N, so a fault is `-11`, not `11`.
    """
    return (-rc in _FAULT_SIGNALS) if rc < 0 else rc not in (0, RESTART_EXIT_CODE)


def main() -> int:
    global PORT
    # Production mode: plain include, no Revise. Inherits PATH from the activated env so the
    # server's Python subprocesses use the same env. CECELIA_SUPERVISED tells the server that
    # backend restart is available (we relaunch it on RESTART_EXIT_CODE). Resolve Julia first: it can
    # set JULIAUP_DEPOT_PATH + PATH, and the server's env is copied from os.environ here.
    julia = _find_julia()
    # A re-exec after an update (`_reexec_if_launcher_changed`) arrives with the browser already open.
    first = os.environ.pop("CECELIA_LAUNCHER_NO_BROWSER", "") != "1"
    crashes: list[float] = []          # fault timestamps inside CRASH_WINDOW — the loop breaker
    while True:
        # Apply staged updates every iteration, not just at first launch — Settings → System Restart
        # re-enters this loop with the Julia backend down, which is the one moment we can swap
        # api/src/*.jl without a running process locking them. Revert runs FIRST so a user who
        # somehow stacked a revert on top of an unrelated apply gets what they asked for.
        launcher = _file_digest(os.path.abspath(__file__))
        _apply_pending_revert()
        _apply_pending_update()
        _reexec_if_launcher_changed(launcher, browser_opened=not first)
        # Read per iteration, so Settings → "Use all CPU cores" lands on the in-app Restart.
        targs, applied = _thread_args()
        env = {**os.environ, "CECELIA_SUPERVISED": "1", "CECELIA_LAUNCH_THREADS": applied}
        launched_at = time.time()
        proc = subprocess.Popen(
            [julia, "--project", *targs, "src/server.jl"],
            cwd=os.path.join(ROOT, "api"),
            env=env,
        )
        try:
            print("Starting Cecelia…")
            port = _launched_port(proc, launched_at)
            if port is None:
                # Exited first (its own message says why, e.g. already running for this user) or
                # never got as far as the lock. Never fall back to probing 8080: that may be
                # ANOTHER user's Cecelia, and we would open the browser on it.
                print("Cecelia server did not start.", file=sys.stderr)
                if proc.poll() is None:
                    proc.terminate()
                return 1
            PORT = port
            print(f"Waiting for /api/health on port {PORT}")
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
            if _crashed(rc):
                now = time.time()
                crashes = [t for t in crashes if now - t < CRASH_WINDOW] + [now]
                if len(crashes) < CRASH_LIMIT:
                    print(f"Cecelia stopped unexpectedly ({rc}) — restarting…", file=sys.stderr)
                    continue
                print(f"Cecelia crashed {len(crashes)} times in {int(CRASH_WINDOW)}s — giving up.",
                      file=sys.stderr)
                return 1
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
