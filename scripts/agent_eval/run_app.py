"""One unattended agent run against the RUNNING app (docs/todo/AGENT_OVERNIGHT_PLAN.md, app tier).

The agent gets what any user of the app would hand it — the observer + the autonomous MCP server,
NO built-in tools (no shell, no files, no python) — a disposable copy of the run images, raw, in one
set (`app_project.py`) and a one-line brief. Everything it does lands in the copy.

    pixi run python scripts/agent_eval/run_app.py --projects-dir ~/cecelia-feijoa/projects \\
        --source-project tSJpBI --image yDfwP7 --image UJS0Hz --root ~/.cecelia-effectiveness/app-runs/<stamp>

`pixi run guide-run <guide>` (`guide_run.py`) is the usual way in: it picks the guide's test project,
writes the brief and the root, and logs the cost.

Writes into `--root`: run.json (the copy), mcp.json, trace.jsonl (stream-json, written live — tail it
to supervise), stderr.log, record.json, decisions.json. The source project is canaried: a file
changed or removed under it during the run is a failure (bar the bookkeeping the app rewrites when
the user opens it — reported, not failed). After the canary, the run's record goes onto the source
project's blackboard (`run_record.py`, AGENT_RUN_REVIEW_PLAN P1) — the one write into the source.

A run the account's usage limit stopped carries `rateLimited` = {message, resetAt} in record.json and
on its blackboard entry, and the runner exits 75 (`EX_TEMPFAIL`): what the copy holds is where the
limit cut it off, not the agent's result.
"""
from __future__ import annotations

import argparse
import datetime
import json
import os
import pathlib
import signal
import subprocess
import sys
import time

HERE = pathlib.Path(__file__).resolve().parent
REPO = HERE.parents[1]
sys.path.insert(0, str(HERE))

import app_project  # noqa: E402
import run_findings  # noqa: E402
import run_record  # noqa: E402
import score  # noqa: E402
import trace_view  # noqa: E402
from cecelia.effectiveness import claude_cli  # noqa: E402
from cecelia.utils import vn_versioning  # noqa: E402
from cecelia.utils.atomic_io import write_json_atomic  # noqa: E402

DEFAULT_BRIEF = "Hey. can you track the cells in these images and analyse their behaviour?"
# what the app rewrites when the USER opens a project (lastOpenedAt, runlog normalisation, lab-log
# context) — not analysis; the canary reports them separately instead of failing on them
APP_BOOKKEEPING = ("project.json", "runlog.json", "settings/", "logs/", "tasks/")
# what the app's open project tells the assistant in a real session; without it the observer's
# list_projects would point the agent at the user's most recently opened project, not the copy
CONTEXT = "[Cecelia — open project: {name} ({projectUid}); set: images ({setUid}), {n} images]\n\n"
# the tasks that bank a cohort QC metric (get_cohort_qc) — read on both sides after the run
COHORT_FUNS = ("segment.cellpose", "segment.measureLabels", "tracking.bayesian_tracking",
               "tracking.track_measures", "behaviour.hmm_states", "behaviour.hmm_transitions")


# The MCP servers' one switch for the task-discovery information (`discovery_enabled()`,
# mcp/cecelia_mcp/discovery.py): `off` is the arm that runs without it (TASK_DISCOVERY_PLAN Decision 9).
DISCOVERY_ENV = "CECELIA_MCP_DISCOVERY"


def mcp_config(api_url: str, project_uid: str, prefix: str, discovery: str = "on") -> dict:
    py = str(REPO / ".pixi" / "envs" / "default" / "bin" / "python3")
    env = {"PYTHONPATH": str(REPO / "mcp"), "CECELIA_API_URL": api_url, DISCOVERY_ENV: discovery}
    return {"mcpServers": {
        "cecelia-observer": {"command": py, "args": ["-m", "cecelia_mcp.server"],
                             # headless: never re-pair the user's open project
                             "env": {**env, "CECELIA_OBSERVER_NO_PAIR": "1"}},
        "cecelia-autonomous": {"command": py, "args": ["-m", "cecelia_mcp.autonomous_server"],
                               "env": {**env, "CECELIA_MCP_PROJECT": project_uid,
                                       "CECELIA_MCP_PREFIX": prefix}},
    }}


def build_command(claude: str, mcp_path: pathlib.Path, budget_usd: float, model: str | None) -> list[str]:
    cmd = [claude, "-p", "--tools", "",                  # no built-ins: the MCP surface is all it has
           "--mcp-config", str(mcp_path), "--strict-mcp-config",
           "--dangerously-skip-permissions",
           "--max-budget-usd", str(budget_usd),
           "--output-format", "stream-json", "--verbose"]
    return cmd + (["--model", model] if model else [])


# ── collect ──────────────────────────────────────────────────────────────────────────────────────

def summarise_trace(path: pathlib.Path, source_project: str) -> dict:
    calls: dict[str, int] = {}
    leaks, errors, model, final = 0, 0, None, {}
    for e in trace_view.events(str(path)):
        if e["kind"] == "init":
            model = e["model"]
        elif e["kind"] == "call":
            calls[e["name"]] = calls.get(e["name"], 0) + 1
            # reading the user's own analysis would let the agent copy it instead of doing it
            if source_project in json.dumps(e["input"]):
                leaks += 1
        elif e["kind"] == "result":
            errors += e["isError"]
        elif e["kind"] == "final":
            final = e
    return {"model": model, "costUsd": final.get("cost"), "turns": final.get("turns"), "toolCalls": calls,
            "toolCallsTotal": sum(calls.values()), "toolErrors": errors,
            "sourceProjectReads": leaks, "finalMessage": final.get("text", ""),
            "rateLimited": final.get("rateLimited")}


def _get(api_url: str, path: str, params: dict):
    return run_record._get(api_url, path, params, timeout=30)


def _pop_paths(node: dict, prefix: str = "") -> list[str]:
    """Population paths of a gating/{vn}.json: top level `populations`, nested `children`."""
    out = []
    for child in node.get("populations") or node.get("children") or []:
        p = f"{prefix}/{child.get('name')}"
        out += [p, *_pop_paths(child, p)]
    return out


def _track_summary(props_dir: pathlib.Path, vn: str) -> dict:
    """Cells, tracks and median per-track measures of one label set — the numbers to hold an agent's
    tracking against the reference's."""
    from cecelia.utils.label_props_utils import LabelPropsView
    out = {}
    try:
        cells = LabelPropsView(str(props_dir / f"{vn}.h5ad")).view_cols(["track_id"]).as_df()
        out["cells"] = len(cells)
        if "track_id" in cells:
            out["tracks"] = int(cells["track_id"].dropna().nunique())
        tracks_file = props_dir / f"{vn}__tracks.h5ad"
        if tracks_file.exists():
            t = LabelPropsView(str(tracks_file)).as_df()
            out["trackMedians"] = {c.removeprefix("live.track."): round(float(t[c].median()), 3)
                                   for c in ("live.track.speed", "live.track.duration",
                                             "live.track.straightness") if c in t}
    except Exception as e:  # noqa: BLE001 — a record, not a gate
        out["error"] = str(e)
    return out


def analysis_state(api_url: str, projects_dir: pathlib.Path, project: str, image: str) -> dict:
    """What an image holds now: label sets, their gated populations with counts, versions, chains."""
    root = projects_dir / project
    ccid = json.loads((root / "1" / image / "ccid.json").read_text(encoding="utf-8"))
    sets = {}
    for vn in sorted(vn_versioning.versioned_keys(ccid.get("label_props") or {})):
        gating = root / "1" / image / "gating" / f"{vn}.json"
        paths = _pop_paths(json.loads(gating.read_text(encoding="utf-8"))) if gating.exists() else []
        pops = {}
        for p in paths:
            try:
                pops[p] = _get(api_url, "/api/gating/stats", {"projectUid": project, "imageUid": image,
                                                              "valueName": vn, "pop": p})
            except Exception as e:  # noqa: BLE001 — a record, not a gate
                pops[p] = {"error": str(e)}
        sets[vn] = {"populations": pops, **_track_summary(root / "1" / image / "labelProps", vn)}
    chains_dir = root / "settings" / "chains"
    return {"labelSets": sets,
            "imageVersions": sorted(vn_versioning.versioned_keys(ccid.get("filepath") or {})),
            "chains": sorted(p.stem for p in chains_dir.glob("*.json")) if chains_dir.exists() else [],
            "chainRuns": len(list((chains_dir / "runs").glob("*"))) if (chains_dir / "runs").exists() else 0}


def cohort_qc(api_url: str, project: str, set_uid: str, images: list[str]) -> dict:
    """Per banked task, the cohort QC doc of `set_uid` — the numbers the agent could have read. On the
    reference side the source set may hold more images than the run (held-out ones); `images` says
    which are the run's."""
    out = {"setUid": set_uid, "images": images, "byFun": {}}
    for fun in COHORT_FUNS:
        try:
            out["byFun"][fun] = _get(api_url, "/api/qc/cohort", {"projectUid": project, "setUid": set_uid,
                                                               "funName": fun})
        except Exception as e:  # noqa: BLE001 — a record, not a gate; a fun that never ran has none
            out["byFun"][fun] = {"error": str(e)}
    return out


def _stop(proc: subprocess.Popen) -> int:
    """End the agent: terminate, then kill after 30 s. Its MCP servers exit when their stdin closes."""
    proc.terminate()
    try:
        return proc.wait(timeout=30)
    except subprocess.TimeoutExpired:
        proc.kill()
        return proc.wait()


def _raise_on_sigterm(signum, frame):
    raise SystemExit(128 + signum)


def run(a) -> dict:
    root = pathlib.Path(a.root).expanduser().resolve()
    root.mkdir(parents=True, exist_ok=True)
    projects_dir = pathlib.Path(a.projects_dir).expanduser()
    src_root = projects_dir / a.source_project
    before = score.snapshot(src_root)

    stamp = time.strftime("%Y-%m-%d %H:%M")
    started_utc = datetime.datetime.now(datetime.timezone.utc).isoformat()
    code_sha = subprocess.run(["git", "rev-parse", "HEAD"], cwd=str(REPO), capture_output=True,
                              text=True).stdout.strip() or None
    name = f"Agent run {stamp}"
    info = {**app_project.build(projects_dir, a.source_project, a.image, name, a.knowledge), "projectName": name}
    write_json_atomic(root / "run.json", info, indent=2)
    mcp_path = root / "mcp.json"
    write_json_atomic(mcp_path, mcp_config(a.api_url, info["projectUid"], a.prefix, a.discovery), indent=2)
    prompt = CONTEXT.format(name=name, n=len(info["images"]), **info) + a.brief
    workdir = root / "cwd"                               # empty: no CLAUDE.md, no repo to read
    workdir.mkdir(exist_ok=True)

    cmd = build_command(a.claude, mcp_path, a.budget_usd, a.model)
    t0 = time.time()
    with open(root / "trace.jsonl", "w", encoding="utf-8") as out, \
            open(root / "stderr.log", "w", encoding="utf-8") as err:
        proc = subprocess.Popen(cmd, stdin=subprocess.PIPE, stdout=out, stderr=err, cwd=str(workdir),
                                text=True, encoding="utf-8")
        (root / "pid").write_text(str(proc.pid), encoding="utf-8")
        try:
            proc.stdin.write(prompt)
            proc.stdin.close()
            rc = proc.wait(timeout=a.timeout_s)
            timed_out = False
        except subprocess.TimeoutExpired:
            rc, timed_out = _stop(proc), True
        except BaseException:
            # Ctrl-C / SIGTERM: never leave the agent running; a second SIGTERM must not cut this short
            signal.signal(signal.SIGTERM, signal.SIG_IGN)
            _stop(proc)
            raise
    wall = round(time.time() - t0)

    after = score.snapshot(src_root)
    canary = score.check_canary(before, after, ignore=APP_BOOKKEEPING)
    canary["added"] = sorted(set(after["files"]) - set(before["files"]))[:20]
    canary["appBookkeeping"] = [f for f, h in before["files"].items()
                                if any(x in f for x in APP_BOOKKEEPING) and after["files"].get(f) != h]
    trace = summarise_trace(root / "trace.jsonl", a.source_project)
    rec = {"startedAt": stamp, "startedAtUtc": started_utc, "codeSha": code_sha, "wallS": wall, "exitCode": rc, "timedOut": timed_out,
           "guide": a.guide or None, "checklist": a.check or [], "knowledgeOn": bool(a.knowledge), "discovery": a.discovery, "brief": a.brief, "prompt": prompt, "budgetUsd": a.budget_usd, "copy": info,
           "trace": trace, "canary": canary,
           "rateLimited": claude_cli.rate_limit_note(trace["rateLimited"]) if trace["rateLimited"] else None}
    rec["agent"], rec["reference"] = {}, {}
    for im in info["images"]:
        for key, (proj, img) in (("agent", (info["projectUid"], im["imageUid"])),
                                 ("reference", (a.source_project, im["sourceImageUid"]))):
            try:
                rec[key][im["sourceImageUid"]] = analysis_state(a.api_url, projects_dir, proj, img)
            except Exception as e:  # noqa: BLE001
                rec[key][im["sourceImageUid"]] = {"error": str(e)}
    run_images = [im["sourceImageUid"] for im in info["images"]]
    rec["cohortQc"] = {"agent": cohort_qc(a.api_url, info["projectUid"], info["setUid"],
                                          [im["imageUid"] for im in info["images"]])}
    if a.source_set:
        rec["cohortQc"]["reference"] = cohort_qc(a.api_url, a.source_project, a.source_set, run_images)
    write_json_atomic(root / "record.json", json.loads(json.dumps(rec, default=str)), indent=2)
    # after the canary: the record is the one thing the harness writes into the source project
    try:
        rec["blackboard"] = run_record.write(root, a.api_url, projects_dir, None, why=a.ask_why,
                                             claude=a.claude)
    except (Exception, SystemExit) as e:  # noqa: BLE001 — the run is done; re-run run_record.py
        rec["blackboard"] = {"error": str(e)}
    # its platform errors, for the weekly judge (P1b) — after record.json, which they read
    write_json_atomic(root / "record.json", json.loads(json.dumps(rec, default=str)), indent=2)
    try:
        rec["findings"] = len(run_findings.emit(root, a.api_url))
    except Exception as e:  # noqa: BLE001 — re-run run_findings.py
        rec["findings"] = {"error": str(e)}
    write_json_atomic(root / "record.json", json.loads(json.dumps(rec, default=str)), indent=2)
    return rec


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("--projects-dir", required=True)
    ap.add_argument("--source-project", required=True)
    ap.add_argument("--image", required=True, action="append", help="repeat for each run image")
    ap.add_argument("--source-set", default="", help="the source set, for its cohort QC")
    ap.add_argument("--ask-why", action="store_true",
                    help="after the run, resume the whole session to ask why: ~$6 a run, since its first turn "
                         "re-reads the session and the budget cap only applies after it")
    ap.add_argument("--knowledge", action="store_true",
                    help="carry the source project's lab-knowledge entries into the copy (P4)")
    ap.add_argument("--discovery", choices=("on", "off"), default="on",
                    help="off: the MCP hides what each task is for (an arm of its own, never pooled)")
    ap.add_argument("--guide", default="", help="the guide id the brief names; goes on the run record")
    ap.add_argument("--check", action="append", help="a reviewer checklist item, shown atop the record (repeat)")
    ap.add_argument("--root", required=True)
    ap.add_argument("--brief", default=DEFAULT_BRIEF)
    ap.add_argument("--budget-usd", type=float, default=15.0)
    ap.add_argument("--timeout-s", type=int, default=3 * 3600)
    ap.add_argument("--model", default=None)
    ap.add_argument("--prefix", default="")
    ap.add_argument("--api-url", default=os.environ.get("CECELIA_API_URL", "http://127.0.0.1:8080"))
    ap.add_argument("--claude", default="claude")
    signal.signal(signal.SIGTERM, _raise_on_sigterm)   # so a stopped run still tears down the agent
    rec = run(ap.parse_args(argv))
    t = rec["trace"]
    print(json.dumps({"copy": rec["copy"]["projectUid"], "wallS": rec["wallS"], "costUsd": t["costUsd"],
                      "toolCalls": t["toolCallsTotal"], "toolErrors": t["toolErrors"],
                      "canaryOk": rec["canary"]["intact"], "blackboard": rec.get("blackboard"),
                      "rateLimited": rec.get("rateLimited")}))
    if not rec["canary"]["intact"]:
        return 1
    if rec.get("rateLimited"):
        return claude_cli.EX_TEMPFAIL
    return 0 if rec["exitCode"] == 0 else 1


if __name__ == "__main__":
    raise SystemExit(main())
