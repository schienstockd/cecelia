"""`pixi run guide-run <guide>`: an agent follows one in-app guide on that guide's test project
(docs/todo/GUIDE_RUNS_PLAN.md P3). A thin wrapper over `run_app.py`, which does the run itself.

    pixi run guide-run intravital-timelapse                 # one run now, $5 cap, knowledge off
    pixi run guide-run intravital-timelapse --runs 3        # three, one after another
    pixi run guide-run intravital-timelapse --at 02:00      # once, at 02:00 (Linux: a systemd user timer)

What it adds to `run_app.py`:
- the guide's test project, from `guide_projects.json` (source project, set, images), and the brief
  "Use the <guide> guide to process the images in this project.";
- a refusal when the app is not running. It never starts or stops the app;
- each run's directory under `~/.cecelia-effectiveness/app-runs/<stamp>/`;
- one lock, so runs never overlap: `--runs N` runs them in turn, and stops at the first that fails;
- one line per run in `~/.cecelia-effectiveness/guide-runs.jsonl` (stamp, guide, commit, knowledge,
  cost: the run, the after-run why turn and their total; wall time, exit, record id), written even
  when the run aborts.
"""
from __future__ import annotations

import argparse
import datetime as _dt
import json
import os
import pathlib
import shutil
import signal
import subprocess
import sys
import time
import urllib.error
import urllib.request

HERE = pathlib.Path(__file__).resolve().parent
REPO = HERE.parents[1]
sys.path.insert(0, str(HERE))

import trace_view  # noqa: E402
from cecelia.effectiveness.git_context import git_output  # noqa: E402
from cecelia.effectiveness.state import state_dir, try_lock  # noqa: E402

PROJECTS = HERE / "guide_projects.json"
GUIDES = REPO / "mcp" / "cecelia_mcp" / "guides.json"
BRIEF = "Use the {name} guide to process the images in this project."
DEFAULT_API = os.environ.get("CECELIA_API_URL", "http://127.0.0.1:8080")


class GuideRunError(Exception):
    """A run refused before it started; the message says why."""


# ── where things go ──────────────────────────────────────────────────────────────────────────────

def runs_dir() -> pathlib.Path:
    return state_dir() / "app-runs"


def log_path() -> pathlib.Path:
    return state_dir() / "guide-runs.jsonl"


# ── guide → test project + brief ─────────────────────────────────────────────────────────────────

def load_projects(path: pathlib.Path = PROJECTS) -> dict:
    return json.loads(path.read_text(encoding="utf-8"))


def guide_titles(path: pathlib.Path = GUIDES) -> dict[str, str]:
    """{guide id: title} of the guides the app ships (the agent reads them with `get_guide`)."""
    return {g["id"]: g["title"] for g in json.loads(path.read_text(encoding="utf-8"))}


def test_project(guide: str, projects: dict | None = None, titles: dict | None = None) -> dict:
    """The guide's test project `{sourceProject, sourceSet, images, checklist, title}`; refuses a guide that has
    none or that the app does not ship."""
    projects = load_projects() if projects is None else projects
    titles = guide_titles() if titles is None else titles
    if guide not in projects:
        raise GuideRunError(f"no test project for guide {guide!r}. Known: {', '.join(sorted(projects))}")
    if guide not in titles:
        raise GuideRunError(f"{guide!r} is not a guide in {GUIDES.relative_to(REPO)}")
    return {**projects[guide], "title": titles[guide]}


def guide_name(title: str) -> str:
    """The title as it reads mid-sentence: "Intravital timelapse" → "intravital timelapse". An
    acronym ("AF correction") keeps its capitals."""
    return title if len(title) > 1 and title[1].isupper() else title[:1].lower() + title[1:]


def brief_for(title: str) -> str:
    return BRIEF.format(name=guide_name(title))


def run_app_argv(guide: str, entry: dict, *, root: pathlib.Path, projects_dir: str, api_url: str,
                 budget_usd: float, knowledge: bool, timeout_s: int, ask_why: bool) -> list[str]:
    """`run_app.py`'s arguments for one run of `guide` on its test project."""
    argv = ["--projects-dir", projects_dir, "--source-project", entry["sourceProject"],
            "--source-set", entry.get("sourceSet", "")]
    for im in entry["images"]:
        argv += ["--image", im]
    for c in entry.get("checklist") or []:
        argv += ["--check", c]
    argv += ["--guide", guide, "--brief", brief_for(entry["title"]), "--root", str(root),
             "--budget-usd", str(budget_usd), "--timeout-s", str(timeout_s), "--api-url", api_url]
    return argv + (["--knowledge"] if knowledge else []) + ([] if ask_why else ["--no-ask-why"])


# ── the app, the code, the outcome ───────────────────────────────────────────────────────────────

def app_status(api_url: str, timeout: float = 5) -> dict | None:
    """The running app's `/api/diagnostics` (its projects dir, its commit), or None when it is down."""
    try:
        with urllib.request.urlopen(f"{api_url.rstrip('/')}/api/diagnostics", timeout=timeout) as r:
            return json.loads(r.read().decode("utf-8"))
    except (urllib.error.URLError, OSError, ValueError):
        return None


def code_state() -> tuple[str | None, bool]:
    """(HEAD sha of this checkout, whether it has uncommitted changes) — the code the agent's MCP runs."""
    sha = git_output("rev-parse", "HEAD", cwd=str(REPO))
    return sha, bool(git_output("status", "--porcelain", "--untracked-files=no", cwd=str(REPO)))


def run_outcome(root: pathlib.Path) -> dict:
    """The cost (from the trace's final line, so an aborted run run_app never recorded still has it if
    the agent finished) and the Blackboard record id, when they exist."""
    cost = None
    trace = root / "trace.jsonl"
    if trace.exists():
        final = [e for e in trace_view.events(str(trace)) if e["kind"] == "final"]
        cost = final[-1]["cost"] if final else None
    record_id = None
    try:
        rec = json.loads((root / "record.json").read_text(encoding="utf-8"))
        record_id = (rec.get("blackboard") or {}).get("entryId")
    except (OSError, ValueError):
        pass
    try:   # the after-run why turn (`run_record.ask_why`), under its own cap and not in the trace
        why_cost = json.loads((root / "why.json").read_text(encoding="utf-8")).get("costUsd")
    except (OSError, ValueError, AttributeError):
        why_cost = None
    return {"costUsd": cost, "whyCostUsd": why_cost, "recordId": record_id}


def log_line(*, stamp: str, guide: str, commit: str | None, knowledge: bool, cost_usd: float | None,
             wall_s: float, exit_code: int, record_id: str | None, why_cost_usd: float | None = None,
             **extra) -> dict:
    """`costUsd` is the agent's run, `whyCostUsd` the after-run why turn, `totalCostUsd` both."""
    total = None if cost_usd is None and why_cost_usd is None else round((cost_usd or 0) + (why_cost_usd or 0), 4)
    return {"stamp": stamp, "guide": guide, "commit": commit, "knowledge": knowledge, "costUsd": cost_usd,
            "whyCostUsd": why_cost_usd, "totalCostUsd": total,
            "wallS": round(wall_s), "exit": exit_code, "recordId": record_id, **extra}


def append_log(line: dict, path: pathlib.Path | None = None) -> None:
    """Append-only: one JSON line per run, never rewritten."""
    path = path or log_path()
    path.parent.mkdir(parents=True, exist_ok=True)
    with open(path, "a", encoding="utf-8") as f:
        f.write(json.dumps(line) + "\n")


# ── one run ──────────────────────────────────────────────────────────────────────────────────────

def _raise_on_sigterm(signum, frame):
    raise SystemExit(128 + signum)


def _end(proc: subprocess.Popen | None, grace_s: float = 0) -> None:
    """Stop `run_app.py` if it is still running: wait `grace_s`, then SIGTERM (which it turns into
    stopping the agent), then kill."""
    if proc is None or proc.poll() is not None:
        return
    if grace_s:
        try:
            proc.wait(timeout=grace_s)
            return
        except subprocess.TimeoutExpired:
            pass
    proc.terminate()
    try:
        proc.wait(timeout=90)
    except subprocess.TimeoutExpired:
        proc.kill()
        proc.wait()


def new_stamp() -> str:
    stamp = time.strftime("%Y%m%dT%H%M%S")
    while (runs_dir() / stamp).exists():         # a run that failed at once, then the next one
        time.sleep(1)
        stamp = time.strftime("%Y%m%dT%H%M%S")
    return stamp


def run_one(guide: str, entry: dict, a, projects_dir: str, commit: str | None, extra: dict) -> int:
    """One run in a fresh `app-runs/<stamp>/`. The log line is written whatever happens; a stopped
    wrapper stops `run_app.py`, which stops the agent."""
    stamp = new_stamp()
    root = runs_dir() / stamp
    argv = run_app_argv(guide, entry, root=root, projects_dir=projects_dir, api_url=a.api_url,
                        budget_usd=a.budget_usd, knowledge=a.knowledge, timeout_s=a.timeout_s,
                        ask_why=a.ask_why)
    print(f"guide-run: {guide} → {root}", flush=True)
    t0, rc, proc = time.time(), None, None
    try:
        proc = subprocess.Popen([sys.executable, str(HERE / "run_app.py"), *argv], cwd=str(REPO))
        rc = proc.wait()
    except KeyboardInterrupt:
        # Ctrl-C reached run_app too (same process group): let it stop the agent itself first
        _end(proc, grace_s=60)
        raise
    finally:
        _end(proc)
        rc = proc.returncode if proc is not None and rc is None else rc
        got = run_outcome(root)
        line = log_line(stamp=stamp, guide=guide, commit=commit, knowledge=a.knowledge,
                        cost_usd=got["costUsd"], why_cost_usd=got["whyCostUsd"], wall_s=time.time() - t0,
                        exit_code=rc if rc is not None else -1, record_id=got["recordId"], **extra)
        append_log(line)
        print(f"guide-run: logged {json.dumps(line)}", flush=True)
    return rc


# ── --at: a transient systemd user timer ─────────────────────────────────────────────────────────

def next_at(at: str, now: _dt.datetime) -> _dt.datetime:
    """`HH:MM` = the next time the clock shows it (today or tomorrow); `YYYY-MM-DD HH:MM` = then."""
    try:
        if " " in at.strip():
            when = _dt.datetime.strptime(at.strip(), "%Y-%m-%d %H:%M")
        else:
            t = _dt.datetime.strptime(at.strip(), "%H:%M").time()
            when = _dt.datetime.combine(now.date(), t)
            if when <= now:
                when += _dt.timedelta(days=1)
    except ValueError:
        raise GuideRunError(f"--at {at!r}: use HH:MM or 'YYYY-MM-DD HH:MM'") from None
    if when <= now:
        raise GuideRunError(f"--at {at!r} is in the past")
    return when


def forward_args(guide: str, a) -> list[str]:
    """The same command without `--at`, for the timer to run."""
    out = [guide, "--runs", str(a.runs), "--budget-usd", str(a.budget_usd), "--timeout-s", str(a.timeout_s),
           "--api-url", a.api_url]
    if a.projects_dir:
        out += ["--projects-dir", a.projects_dir]
    return out + (["--knowledge"] if a.knowledge else []) + ([] if a.ask_why else ["--no-ask-why"])


def schedule_argv(when: _dt.datetime, unit: str, command: list[str], path_env: str,
                  effectiveness_log: str | None = None) -> list[str]:
    """A one-shot transient user timer: the calendar names the full date, so it fires once."""
    env = [f"--setenv=PATH={path_env}"] + \
        ([f"--setenv=CECELIA_EFFECTIVENESS_LOG={effectiveness_log}"] if effectiveness_log else [])
    return ["systemd-run", "--user", f"--unit={unit}", f"--description=Cecelia guide run ({unit})",
            f"--on-calendar={when:%Y-%m-%d %H:%M:00}", "--timer-property=AccuracySec=1s",
            f"--working-directory={REPO}", *env, "--", *command]


def schedule(guide: str, a) -> int:
    if not sys.platform.startswith("linux") or not shutil.which("systemd-run"):
        raise GuideRunError("--at needs Linux with systemd (systemd-run); run without --at instead")
    pixi = shutil.which("pixi")
    if not pixi:
        raise GuideRunError("--at: pixi is not on PATH")
    when = next_at(a.at, _dt.datetime.now())
    unit = f"cecelia-guide-run-{when:%Y%m%dT%H%M}"
    command = [pixi, "run", "--manifest-path", str(REPO / "pixi.toml"), "guide-run", *forward_args(guide, a)]
    r = subprocess.run(schedule_argv(when, unit, command, os.environ.get("PATH", ""),
                                     os.environ.get("CECELIA_EFFECTIVENESS_LOG")),
                       capture_output=True, text=True, encoding="utf-8")
    if r.returncode != 0:
        raise GuideRunError(f"systemd-run failed: {r.stderr.strip()}")
    print(f"guide-run: scheduled {guide} for {when:%Y-%m-%d %H:%M} ({a.runs} run(s), ${a.budget_usd} cap each)\n"
          f"  runs: {' '.join(command)}\n"
          f"  watch:  journalctl --user -u {unit}.service -f\n"
          f"  cancel: systemctl --user stop {unit}.timer\n"
          "  The app must be running then; if it is not, the run is skipped (and logged).")
    return 0


# ── main ─────────────────────────────────────────────────────────────────────────────────────────

def _lock():
    """The one-run-at-a-time lock (`app-runs/.lock`); None when another run holds it. Not guarded on
    Windows, where this dev tool isn't run."""
    return try_lock(runs_dir() / ".lock")


def run_batch(guide: str, entry: dict, a) -> int:
    commit, dirty = code_state()
    status = app_status(a.api_url)
    skip = None
    if status is None:
        skip = f"the app is not running at {a.api_url}; start it first (guide-run never starts it)"
    lock = _lock() if skip is None else None
    if skip is None and lock is None:
        skip = "another guide run holds the lock"
    if skip:
        append_log(log_line(stamp=time.strftime("%Y%m%dT%H%M%S"), guide=guide, commit=commit,
                            knowledge=a.knowledge, cost_usd=0, wall_s=0, exit_code=2, record_id=None,
                            skipped=skip))
        print(f"guide-run: {skip}", file=sys.stderr)
        return 2
    projects_dir = a.projects_dir or status.get("projectsDir") or ""
    if not (pathlib.Path(projects_dir).expanduser() / entry["sourceProject"]).is_dir():
        print(f"guide-run: source project {entry['sourceProject']} not found in {projects_dir}", file=sys.stderr)
        return 2
    extra = {"dirty": dirty, "appCommit": status.get("commit"), "budgetUsd": a.budget_usd}
    if status.get("stale"):
        print(f"guide-run: note — the app runs {status.get('commit')}, behind its checkout; restart it "
              "to test the current code", file=sys.stderr)
    with lock:
        for i in range(a.runs):
            rc = run_one(guide, entry, a, projects_dir, commit, extra)
            if rc != 0:
                print(f"guide-run: run {i + 1}/{a.runs} exited {rc}; not starting the rest", file=sys.stderr)
                return rc
    return 0


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(
        prog="pixi run guide-run",
        description="An agent follows one in-app guide on that guide's test project, against the RUNNING "
                    "app (docs/todo/GUIDE_RUNS_PLAN.md). Spends real money: up to --budget-usd per run.")
    ap.add_argument("guide", help=f"a guide id with a test project: {', '.join(sorted(load_projects()))}")
    ap.add_argument("--runs", type=int, default=1, help="how many, one after another (default 1)")
    ap.add_argument("--budget-usd", type=float, default=5.0, help="cost cap per run (default 5)")
    ap.add_argument("--knowledge", action="store_true",
                    help="carry the source project's lab-knowledge entries in: a knowledge run, reported apart")
    ap.add_argument("--at", default=None, metavar="HH:MM",
                    help="run once at that time ('YYYY-MM-DD HH:MM' for another day) instead of now. "
                         "Linux only: a transient systemd user timer runs this same command then")
    ap.add_argument("--no-ask-why", dest="ask_why", action="store_false",
                    help="skip the post-run why turn")
    ap.add_argument("--projects-dir", default=None, help="default: the running app's projects dir")
    ap.add_argument("--timeout-s", type=int, default=3 * 3600)
    ap.add_argument("--api-url", default=DEFAULT_API)
    a = ap.parse_args(argv)
    try:
        if a.runs < 1:
            raise GuideRunError("--runs must be at least 1")
        entry = test_project(a.guide)
        if a.at:
            return schedule(a.guide, a)
        signal.signal(signal.SIGTERM, _raise_on_sigterm)   # a stopped timer still logs and tears down
        return run_batch(a.guide, entry, a)
    except GuideRunError as e:
        print(f"guide-run: {e}", file=sys.stderr)
        return 2
    except KeyboardInterrupt:
        print("guide-run: interrupted; the run was stopped and logged", file=sys.stderr)
        return 130


if __name__ == "__main__":
    raise SystemExit(main())
