"""One unattended agent run against the RUNNING app (docs/todo/AGENT_OVERNIGHT_PLAN.md, app tier).

The agent gets what any user of the app would hand it — the observer + the autonomous MCP server,
NO built-in tools (no shell, no files, no python) — a disposable copy of one raw image
(`app_project.py`) and a one-line brief. Everything it does lands in the copy, which is left in the
projects dir as the record: open it in the app to see its chains, gates, tracks and behaviour.

    pixi run python scripts/agent_eval/run_app.py --projects-dir ~/cecelia-feijoa/projects \\
        --source-project tSJpBI --image yDfwP7 --root /tmp/cecelia-agent-app/<stamp> --budget-usd 15

Writes into `--root`: run.json (the copy), mcp.json, trace.jsonl (stream-json, written live — tail it
to supervise), stderr.log, record.json. The source project is canaried: any file added, removed or
changed under it during the run is a failure (bar the bookkeeping the app rewrites when the user
opens it — reported, not failed).
"""
from __future__ import annotations

import argparse
import json
import os
import pathlib
import subprocess
import sys
import time
import urllib.parse
import urllib.request

HERE = pathlib.Path(__file__).resolve().parent
REPO = HERE.parents[1]
sys.path.insert(0, str(HERE))

import app_project  # noqa: E402
import score  # noqa: E402
from cecelia.utils import vn_versioning  # noqa: E402
from cecelia.utils.atomic_io import write_json_atomic  # noqa: E402
from run_overnight import tool_errors  # noqa: E402

DEFAULT_BRIEF = "Hey. can you track the cells in that image and analyse their behaviour?"
# what the app rewrites when the USER opens a project (lastOpenedAt, runlog normalisation, lab-log
# context) — not analysis; the canary reports them separately instead of failing on them
APP_BOOKKEEPING = ("project.json", "runlog.json", "settings/", "logs/", "tasks/")
# what the app's open project tells the assistant in a real session; without it the observer's
# list_projects would point the agent at the user's most recently opened project, not the copy
CONTEXT = "[Cecelia — open project: {name} ({projectUid}); image: {imageName} ({imageUid})]\n\n"


def mcp_config(api_url: str, project_uid: str, prefix: str) -> dict:
    py = str(REPO / ".pixi" / "envs" / "default" / "bin" / "python3")
    env = {"PYTHONPATH": str(REPO / "mcp"), "CECELIA_API_URL": api_url}
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
    leaks, final, cost, turns, model = 0, "", None, None, None
    text = path.read_text(encoding="utf-8", errors="replace")
    for line in text.splitlines():
        try:
            ev = json.loads(line)
        except json.JSONDecodeError:
            continue
        if ev.get("type") == "system" and ev.get("subtype") == "init":
            model = ev.get("model")
        for block in (ev.get("message") or {}).get("content") or []:
            if not isinstance(block, dict):
                continue
            if block.get("type") == "tool_use":
                calls[block.get("name", "?")] = calls.get(block.get("name", "?"), 0) + 1
                # reading the user's own analysis would let the agent copy it instead of doing it
                if source_project in json.dumps(block.get("input") or {}):
                    leaks += 1
        if ev.get("type") == "result":
            final, cost, turns = ev.get("result") or "", ev.get("total_cost_usd"), ev.get("num_turns")
    return {"model": model, "costUsd": cost, "turns": turns, "toolCalls": calls,
            "toolCallsTotal": sum(calls.values()), "toolErrors": tool_errors(text),
            "sourceProjectReads": leaks, "finalMessage": final}


def _get(api_url: str, path: str, params: dict):
    url = api_url.rstrip("/") + path + "?" + urllib.parse.urlencode(params)
    with urllib.request.urlopen(url, timeout=30) as r:
        return json.loads(r.read().decode("utf-8"))


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


def run(a) -> dict:
    root = pathlib.Path(a.root).expanduser().resolve()
    root.mkdir(parents=True, exist_ok=True)
    projects_dir = pathlib.Path(a.projects_dir).expanduser()
    src_root = projects_dir / a.source_project
    before = score.snapshot(src_root)

    stamp = time.strftime("%Y-%m-%d %H:%M")
    info = app_project.build(projects_dir, a.source_project, a.image, f"Agent run {stamp}")
    write_json_atomic(root / "run.json", info, indent=2)
    mcp_path = root / "mcp.json"
    write_json_atomic(mcp_path, mcp_config(a.api_url, info["projectUid"], a.prefix), indent=2)
    prompt = CONTEXT.format(name=f"Agent run {stamp}", **info) + a.brief
    workdir = root / "cwd"                               # empty: no CLAUDE.md, no repo to read
    workdir.mkdir(exist_ok=True)

    cmd = build_command(a.claude, mcp_path, a.budget_usd, a.model)
    t0 = time.time()
    with open(root / "trace.jsonl", "w", encoding="utf-8") as out, \
            open(root / "stderr.log", "w", encoding="utf-8") as err:
        proc = subprocess.Popen(cmd, stdin=subprocess.PIPE, stdout=out, stderr=err, cwd=str(workdir),
                                text=True, encoding="utf-8")
        (root / "pid").write_text(str(proc.pid), encoding="utf-8")
        proc.stdin.write(prompt)
        proc.stdin.close()
        try:
            rc = proc.wait(timeout=a.timeout_s)
            timed_out = False
        except subprocess.TimeoutExpired:
            proc.terminate()
            try:
                rc = proc.wait(timeout=30)
            except subprocess.TimeoutExpired:
                proc.kill()
                rc = proc.wait()
            timed_out = True
    wall = round(time.time() - t0)

    after = score.snapshot(src_root)
    canary = score.check_canary(before, after, ignore=APP_BOOKKEEPING)
    canary["added"] = sorted(set(after["files"]) - set(before["files"]))[:20]
    canary["appBookkeeping"] = [f for f, h in before["files"].items()
                                if any(x in f for x in APP_BOOKKEEPING) and after["files"].get(f) != h]
    rec = {"startedAt": stamp, "wallS": wall, "exitCode": rc, "timedOut": timed_out,
           "brief": a.brief, "prompt": prompt, "budgetUsd": a.budget_usd, "copy": info,
           "trace": summarise_trace(root / "trace.jsonl", a.source_project),
           "canary": canary}
    for key, (proj, img) in (("agent", (info["projectUid"], info["imageUid"])),
                             ("reference", (a.source_project, a.image))):
        try:
            rec[key] = analysis_state(a.api_url, projects_dir, proj, img)
        except Exception as e:  # noqa: BLE001
            rec[key] = {"error": str(e)}
    write_json_atomic(root / "record.json", json.loads(json.dumps(rec, default=str)), indent=2)
    return rec


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("--projects-dir", required=True)
    ap.add_argument("--source-project", required=True)
    ap.add_argument("--image", required=True)
    ap.add_argument("--root", required=True)
    ap.add_argument("--brief", default=DEFAULT_BRIEF)
    ap.add_argument("--budget-usd", type=float, default=15.0)
    ap.add_argument("--timeout-s", type=int, default=3 * 3600)
    ap.add_argument("--model", default=None)
    ap.add_argument("--prefix", default="")
    ap.add_argument("--api-url", default=os.environ.get("CECELIA_API_URL", "http://127.0.0.1:8080"))
    ap.add_argument("--claude", default="claude")
    rec = run(ap.parse_args(argv))
    t = rec["trace"]
    print(json.dumps({"copy": rec["copy"]["projectUid"], "wallS": rec["wallS"], "costUsd": t["costUsd"],
                      "toolCalls": t["toolCallsTotal"], "toolErrors": t["toolErrors"],
                      "canaryOk": rec["canary"]["intact"]}))
    return 0 if rec["canary"]["intact"] and rec["exitCode"] == 0 else 1


if __name__ == "__main__":
    raise SystemExit(main())
