#!/usr/bin/env python3
"""Cecelia with no GUI — Claude in the terminal is the front end (docs/todo/HEADLESS_MODE_PLAN.md).

    pixi run headless [<projectUid>]

Uses the Cecelia server already running for this user, or starts one with no browser and its output
going to a log file (the terminal belongs to Claude). With no project given, it lists the projects to
pick from, newest-opened first, with "new project" at the top. Then the server builds the session —
the MCP config, the caller hook, the `claude` path and the profile env (`GET /api/observer/headless`)
— and this starts it. When Claude exits, a server this launcher started is shut down; one it found
running is left alone.

The server half is `app.py`'s own (find Julia, thread args, lifeline, graceful stop) — imported, not
copied. `pixi run console` in a second terminal shows the tasks live.
"""
from __future__ import annotations

import json
import os
import signal
import subprocess
import sys
import urllib.error
import urllib.parse

import app
from cecelia.utils import loopback


def _call(path: str, body: dict | None = None, timeout: float = 30.0) -> dict:
    """One JSON request to this user's server (`app.URL`), with the API token."""
    headers = {"Authorization": f"Bearer {app._api_token()}"}
    data = None
    if body is not None:
        data = json.dumps(body).encode("utf-8")
        headers["Content-Type"] = "application/json"
    try:
        with loopback.open_url(app.URL, path, data=data, headers=headers, timeout=timeout,
                               method="POST" if body is not None else "GET") as resp:
            return json.loads(resp.read().decode("utf-8"))
    except urllib.error.HTTPError as e:            # the API's own `{error}` says why
        try:
            msg = json.loads(e.read().decode("utf-8")).get("error") or str(e)
        except (ValueError, OSError):
            msg = str(e)
        raise SystemExit(f"Cecelia: {msg}") from None


def _attach() -> bool:
    """Is Cecelia already up for this user? Asked where every Python client looks for THIS user's
    backend (`loopback.default_base_url` — `CECELIA_API_URL`, else this user's port slot)."""
    port = urllib.parse.urlparse(loopback.default_base_url()).port
    if not port:
        return False
    app.PORT = str(port)
    return app._server_ready(timeout=3)


def _start(log_path: str):
    """Start the server like `app.py` does, minus the browser and the supervisor loop; its output goes
    to `log_path`. Returns the process once /api/health answers, or None."""
    log = open(log_path, "a", encoding="utf-8")
    # Its own process group: Ctrl-C in the terminal is Claude's "stop this turn", and must not reach
    # the server. The lifeline (stdin pipe) still stops it if this launcher dies.
    group = ({"creationflags": subprocess.CREATE_NEW_PROCESS_GROUP} if sys.platform == "win32"
             else {"start_new_session": True})
    proc, launched_at = app._spawn_server(app._find_julia(), stdout=log, stderr=subprocess.STDOUT, **group)
    print(f"Starting Cecelia (log: {log_path})…")
    port = app._launched_port(proc, launched_at)
    if port is None:
        return None
    app.PORT = port
    return proc if app._server_ready() else None


def pick_project(projects: list[dict], ask=input, say=print) -> str | None:
    """Ask which project to open. `""` = a new project (Claude creates it), None = quit. Enter alone
    takes the most recently opened one — the list is already in that order (server-side)."""
    say("Which project?")
    say("  0) new project")
    for i, p in enumerate(projects, 1):
        say(f"  {i}) {p.get('name')} ({p.get('uid')})")
    default = "1" if projects else "0"
    while True:
        choice = ask(f"Number [{default}], q to quit: ").strip().lower() or default
        if choice == "q":
            return None
        if choice.isdigit() and int(choice) <= len(projects):
            return "" if choice == "0" else str(projects[int(choice) - 1].get("uid"))
        say(f"  pick 0–{len(projects)}")


def claude_env(base: dict, env: dict) -> dict:
    """The env `claude` runs under: ours, with the server's unset list removed and its set applied."""
    out = {k: v for k, v in base.items() if k not in set(env.get("unset") or [])}
    out.update({k: str(v) for k, v in (env.get("set") or {}).items()})
    return out


def run_claude(argv: list[str], env: dict) -> int:
    """Run the interactive session in this terminal. Ctrl-C belongs to Claude (it stops a turn), so
    the launcher swallows it meanwhile — `subprocess.call` would otherwise kill Claude on the first one.
    A no-op handler, not SIG_IGN: an ignored signal stays ignored across exec, a handler resets."""
    previous = signal.signal(signal.SIGINT, lambda *_: None)
    try:
        return subprocess.Popen(argv, env=env).wait()
    finally:
        signal.signal(signal.SIGINT, previous)


def main(argv: list[str]) -> int:
    project = argv[1].strip() if len(argv) > 1 else ""
    proc = None
    if _attach():
        print(f"Using the Cecelia server already running at {app.URL}.")
    else:
        proc = _start(os.path.join(app._config_dir(), "headless-server.log"))
        if proc is None:
            print("Cecelia server did not start — see the log above for why.", file=sys.stderr)
            return 1
    try:
        if not project:
            picked = pick_project(_call("/api/projects").get("projects") or [])
            if picked is None:
                return 0
            project = picked
        q = f"?projectUid={urllib.parse.quote(project)}" if project else ""
        session = _call(f"/api/observer/headless{q}")
        if not session.get("available"):
            print(session.get("error") or "Claude Code is not available.", file=sys.stderr)
            return 1
        if project:
            _call("/api/projects/load", {"uid": project})   # what opening it in the GUI stamps
        what = f"project {session.get('projectName')} ({project})" if project else "a new project"
        print(f"Cecelia, no GUI — {what}. Live task progress: `pixi run console` in another terminal.")
        return run_claude(session["argv"], claude_env(dict(os.environ), session.get("env") or {}))
    except KeyboardInterrupt:
        return 0
    finally:
        if proc is not None and proc.poll() is None and not app._stop_gracefully(proc):
            proc.terminate()
            try:
                proc.wait(timeout=10)
            except subprocess.TimeoutExpired:
                proc.kill()


if __name__ == "__main__":
    raise SystemExit(main(sys.argv))
