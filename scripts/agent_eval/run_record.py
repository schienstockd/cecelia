"""An agent run's record on the blackboard of its SOURCE project (docs/todo/AGENT_RUN_REVIEW_PLAN.md P1):
one entry per run, one section per decision, written from the trace by the harness — never by the
agent — so a run cannot leave out the decision it got wrong.

A decision is an action (a chain node run, a task run, a gate added / moved / deleted, a board or a
lab-log line) tagged with its image(s) and one step of `STEPS`. Each section holds what was done,
the reads since the previous decision (*looked at*), the agent's own words since then (*said before
acting*), calls that failed on the way (*tried first*), what came out, and — when the harness asked
after the run — *explained after the run*. Tool errors are listed apart, unscored (Decision 10).

The pictures it looked at are uploaded as `agent_run` captures into the source project and attached,
so the entry reads completely after the copy is deleted. A gate decision with no picture in the trace
gets one re-rendered from the copy (its gates as they are at record time — said in the caption). Every
stage that ran (segmentation, gating, tracking, HMM, clustering) also gets its RESULTS — the Analysis
board plots for it, added to the copy and rendered headless by the app's own frontend
(`stage_boards.py`); a stage whose board fails says so in its section, and the record is written anyway.

    pixi run python scripts/agent_eval/run_record.py ~/.cecelia-effectiveness/app-runs/<stamp> [--dry-run DIR]
                                                     [--no-stage-boards]

`--dry-run DIR` writes entry.md + the pictures to DIR and posts nothing. `--ask-why` resumes the
finished session once for the why (costs an agent turn; off for a back-fill).

A run the account's usage limit stopped (the trace's `result` event is a 429 / limit refusal) is said
in the title, at the top of the entry and in the `agentRun` marker (`rateLimited`): its decisions are
where the limit cut it off, not a finished run, and the CLI's refusal is not shown as the agent's last
message. A why turn the limit refuses (or that fails) is said in the entry, not left blank.
"""
from __future__ import annotations

import argparse
import base64
import datetime as _dt
import json
import os
import pathlib
import subprocess
import sys
import urllib.error
import urllib.parse
import urllib.request

HERE = pathlib.Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))

import app_project  # noqa: E402
import stage_boards  # noqa: E402
import trace_view  # noqa: E402
from cecelia.effectiveness import claude_cli  # noqa: E402
from cecelia.utils.atomic_io import write_atomic, write_json_atomic  # noqa: E402

STEPS = ("cleanup", "segment", "measure", "gate", "track", "behaviour", "report")
GATE_TOOLS = {"add_gate", "set_gate", "delete_gate"}
REPORT_TOOLS = {"add_analysis_board", "append_lab_log", "create_blackboard_entry", "revise_blackboard_entry",
                "create_notebook", "revise_notebook"}
ACTIONS = {"run_chain", "run_task"} | GATE_TOOLS | REPORT_TOOLS
# calls that only wait on an action already recorded — not something the agent looked at
_WAITS = {"wait_for_chain", "wait_for_tasks", "get_lock"}
SURFACE = "agent_run"


def step_of(fun_name: str) -> str:
    """The fixed step a task belongs to."""
    if fun_name == "segment.measureLabels":
        return "measure"
    head = fun_name.split(".")[0]
    return {"cleanupImages": "cleanup", "segment": "segment", "tracking": "track", "behaviour": "behaviour",
            "clustPops": "behaviour", "clustTracks": "behaviour"}.get(head, "measure")


def _json(text: str):
    try:
        return json.loads(text)
    except (json.JSONDecodeError, TypeError):
        return None


def _short(v, n: int = 60) -> str:
    s = v if isinstance(v, str) else json.dumps(v, separators=(",", ":"))
    return s if len(s) <= n else s[: n - 1] + "…"


# ── trace → decisions ────────────────────────────────────────────────────────────────────────────

def pair_calls(evs: list[dict]) -> list[dict]:
    """The trace as an ordered list of `text` items and `call` items, each call with its result."""
    results = {e["id"]: e for e in evs if e["kind"] == "result"}
    out = []
    for e in evs:
        if e["kind"] == "text" and e["text"].strip():
            out.append({"kind": "text", "text": e["text"].strip()})
        elif e["kind"] == "call":
            r = results.get(e["id"]) or {"text": "", "isError": False, "images": []}
            out.append({"kind": "call", "name": e["name"], "input": e["input"], "result": r["text"],
                        "isError": r["isError"], "images": r["images"]})
    return out


def _chain_states(items: list[dict], run_id: str) -> dict:
    """{imageUid: {nodeId: state}} from the last wait_for_chain on `run_id`."""
    states = {}
    for it in items:
        if it["kind"] == "call" and it["name"] == "wait_for_chain" and it["input"].get("run_id") == run_id:
            states = (_json(it["result"]) or {}).get("imageStates") or states
    return states


def _task_states(items: list[dict], start: int, task_ids: list[str]) -> dict:
    """{taskId: state} from wait_for_tasks calls after position `start`."""
    out = {}
    for it in items[start:]:
        if it["kind"] == "call" and it["name"] == "wait_for_tasks":
            for k, v in ((_json(it["result"]) or {}).get("states") or {}).items():
                if k in task_ids:
                    out[k] = v
    return out


def units_of(items: list[dict], i: int, chains: dict, all_images: list[str]) -> list[dict]:
    """What the action at `items[i]` did, as units {step, fn, target, images, did, outcome}."""
    it = items[i]
    name, inp = it["name"], it["input"]
    if name == "run_chain":
        run_id = (_json(it["result"]) or {}).get("runId", "")
        images = inp.get("image_uids") or all_images
        states = _chain_states(items, run_id)
        out = []
        for node in chains.get(inp.get("chain_name"), []):
            fn, params = node.get("fn", "?"), node.get("params") or {}
            got = {img: (states.get(img) or {}).get(node.get("id"), "?") for img in images}
            out.append({"step": step_of(fn), "fn": fn, "target": stage_boards.output_value_name(params),
                        "images": images,
                        "did": f"`{fn}` (chain {inp.get('chain_name')!r}, node {node.get('id')}) {_short(params, 400)}",
                        "params": params, "outcome": got})
        return out
    if name == "run_task":
        fn, params = inp.get("fun_name", "?"), inp.get("params") or {}
        images = inp.get("image_uids") or all_images
        tasks = (_json(it["result"]) or {}).get("tasks") or []
        states = _task_states(items, i + 1, [t.get("taskId") for t in tasks])
        got = {}
        for t in tasks:
            # not waited on: the last log line the start call returned says how far it got
            last = next((ln for ln in reversed(t.get("log") or []) if ln.strip()), "")
            state = states.get(t.get("taskId")) or \
                f"{t.get('status', '?')}, not waited on" + (f" (last log: {_short(last, 120)})" if last else "")
            for img in ([t["imageUid"]] if t.get("imageUid") else images):
                got[img] = state
        return [{"step": step_of(fn), "fn": fn, "target": stage_boards.output_value_name(params), "images": images,
                 "did": f"`{fn}` {_short(params, 400)}", "params": params, "outcome": got}]
    if name in GATE_TOOLS:
        img, vn = inp.get("image_uid", ""), inp.get("value_name", "")
        pop = inp.get("path") or f"{(inp.get('parent') or 'root').rstrip('/').removeprefix('root')}/{inp.get('name')}"
        gate = inp.get("gate") or {}
        did = {"add_gate": f"add `{pop}` on {vn}", "set_gate": f"move `{pop}` on {vn}",
               "delete_gate": f"delete `{pop}` on {vn}"}[name]
        if gate:
            did += f": {_gate_text(gate)}"
        return [{"step": "gate", "fn": name, "target": vn, "images": [img], "did": did, "pop": pop,
                 "gate": gate, "parent": inp.get("parent") or "root", "outcome": {}}]
    summary = {"add_analysis_board": lambda: f"board {inp.get('name')!r} with {len(inp.get('plots') or [])} plots",
               "append_lab_log": lambda: "lab log: " + " / ".join(inp.get("lines") or [])}.get(
        name, lambda: f"{name} {_short(inp.get('title') or inp.get('name') or '', 80)}")()
    return [{"step": "report", "fn": name, "target": "", "images": [], "did": summary, "outcome": {}}]


def _gate_text(g: dict) -> str:
    def axis(a):
        t = (g.get(f"{a}_transform") or {}).get("kind", "linear")
        return f"{g.get(f'{a}_channel')} ({t})"
    if g.get("kind") == "rectangle":
        return (f"rectangle {axis('x')} {g.get('x_min')}–{g.get('x_max')} × "
                f"{axis('y')} {g.get('y_min')}–{g.get('y_max')}")
    return f"{g.get('kind')} on {axis('x')} × {axis('y')}, {len(g.get('vertices') or [])} vertices"


def _read_text(it: dict, src_of) -> str:
    args = {k: v for k, v in it["input"].items() if k not in ("project_uid",)}
    if "image_uid" in args:
        args["image_uid"] = src_of(args["image_uid"])
    return f"{it['name']}({', '.join(f'{k}={_short(v, 40)}' for k, v in args.items())})"


def decisions(trace_path: str, images: list[dict]) -> dict:
    """The run as sections. `images` = run.json's copy↔source map (app_project.copy_images)."""
    evs = trace_view.events(trace_path)
    items = pair_calls(evs)
    copy_uids = [im["imageUid"] for im in images]
    src = {im["imageUid"]: im["sourceImageUid"] for im in images}
    src_of = lambda u: src.get(u, u)          # noqa: E731
    chains, sections, errors = {}, [], []
    looked, said, tried, pics = [], [], [], []
    for i, it in enumerate(items):
        if it["kind"] == "text":
            said.append(it["text"])
            continue
        name = it["name"]
        if it["isError"]:
            errors.append({"tool": name, "error": it["result"], "input": it["input"]})
            tried.append(f"{name} → {_short(it['result'], 240)}")
            continue
        if name == "create_chain":
            chains[it["input"].get("name")] = it["input"].get("nodes") or []
            continue
        if name not in ACTIONS:
            if name not in _WAITS:
                looked.append(_read_text(it, src_of))
                pics += [{"png": p, "caption": _read_text(it, src_of), "call": it} for p in it["images"]]
            continue
        units = units_of(items, i, chains, copy_uids)
        fresh = bool(looked or said or tried)
        for k, u in enumerate(units):
            u["images"] = [src_of(x) for x in u["images"]]
            u["outcome"] = {src_of(x): v for x, v in u["outcome"].items()}
            key = (u["step"], u["fn"], tuple(u["images"]), u["target"] if u["step"] == "gate" else "")
            prev = sections[-1] if sections else None
            # one decision: same kind of action on the same images, with nothing read or said between
            if prev and prev["key"] == key and (k > 0 or not fresh):
                prev["units"].append(u)
                continue
            first = k == 0
            sections.append({"key": key, "step": u["step"], "fn": u["fn"], "images": u["images"],
                             "units": [u], "lookedAt": looked if first else [], "said": said if first else [],
                             "triedFirst": tried if first else [], "pictures": pics if first else []})
        looked, said, tried, pics = [], [], [], []
    final = next((e for e in reversed(evs) if e["kind"] == "final"), None)
    limited = final and final.get("rateLimited")
    if final and final["text"].strip() and not limited:     # a limit refusal is the CLI's, not the agent's
        sections.append({"key": ("report", "final", (), ""), "step": "report", "fn": "final message",
                         "images": [], "units": [{"did": "the run's last message", "outcome": {}}],
                         "lookedAt": looked, "said": [final["text"].strip()], "triedFirst": tried,
                         "pictures": pics})
    for n, s in enumerate(sections, 1):
        s["id"] = f"d{n:02d}"
        del s["key"]
    init = next((e for e in evs if e["kind"] == "init"), {})
    return {"sessionId": init.get("sessionId"), "model": init.get("model"), "sections": sections,
            "toolErrors": errors, "cost": final and final["cost"], "turns": final and final["turns"],
            "rateLimited": limited or None}


# ── the entry ────────────────────────────────────────────────────────────────────────────────────

def _images_tag(imgs: list[str]) -> str:
    if not imgs:
        return "all"
    return ", ".join(imgs) if len(imgs) <= 3 else f"{len(imgs)} images"


def heading(s: dict) -> str:
    targets = sorted({u.get("target") for u in s["units"] if u.get("target")})
    what = s["fn"] + (f" on {', '.join(targets)}" if targets else "")
    return f"### {s['id']} · {s['step']} · {_images_tag(s['images'])} · {what}"


def _outcome_text(u: dict) -> str:
    o = u.get("outcome") or {}
    if u.get("stats"):
        return u["stats"]
    if not o:
        return ""
    vals = set(o.values())
    return next(iter(vals)) if len(vals) == 1 else ", ".join(f"{k}: {v}" for k, v in o.items())


def _quote(text: str) -> str:
    return "\n".join("> " + ln for ln in text.splitlines())


def knowledge_on(run: dict, rec: dict) -> bool:
    """Whether the run was a knowledge run. Records from before `knowledgeOn` say it by what they carried."""
    return bool(rec.get("knowledgeOn", bool(run.get("knowledge"))))


def title_of(run: dict, rec: dict, root_name: str, set_name: str) -> str:
    """The entry's title: the guide (when the brief named one), the start, the source set, and knowledge."""
    head = f"Guide run {rec['guide']} · " if rec.get("guide") else "Agent run "
    return f"{head}{rec.get('startedAt') or root_name} — {set_name}" + \
        (" · with lab knowledge" if knowledge_on(run, rec) else "")


def render(run: dict, rec: dict, dec: dict, why: dict | None, caps: dict, notes: dict | None = None,
           why_failed: str | None = None) -> str:
    """entry.md. `caps` = {sectionId: [captureId or local file name]}; `notes` = {sectionId: [what the
    stage boards could not show]}, `""` for the whole record; `why_failed` = why the after-run
    question has no answers."""
    notes = notes or {}
    images = app_project.copy_images(run)
    t = rec.get("trace") or {}
    checklist = ["**Review checklist:**", *(f"- {c}" for c in rec["checklist"]), ""] \
        if rec.get("checklist") else []
    lines = []
    if dec.get("rateLimited"):
        lines += [f"**Stopped by the usage limit:** {_short(dec['rateLimited'], 300)} — the decisions below "
                  "are where the limit cut the run off, not a finished run. Leave it out of any comparison.",
                  ""]
    lines += [*checklist,
             f"**Guide:** `{rec['guide']}`" if rec.get("guide") else "**Guide:** none — a free brief",
             f"**Brief:** {rec.get('brief', '')}",
             f"**Code:** `{(rec.get('codeSha') or '?')[:10]}`",
             f"**Copy:** {run.get('projectName') or ''} `{run['projectUid']}` — disposable; this entry is the record.",
             "**Images:** " + "; ".join(f"`{im['sourceImageUid']}` = copy `{im['imageUid']}` ({im['imageName']})"
                                        for im in images),
             "**Lab knowledge:** " + ("; ".join(f"[[{k['entryId']}]]" for k in run.get("knowledge") or [])
                                      or ("on, but the source has no knowledge entries" if knowledge_on(run, rec)
                                          else "none — the copy started with an empty Blackboard")),
             f"**Run:** {dec.get('model') or t.get('model')} · ${t.get('costUsd') or dec.get('cost')} · "
             f"{t.get('turns') or dec.get('turns')} turns · {rec.get('wallS', '?')} s · "
             f"{t.get('toolCallsTotal', '?')} tool calls, {len(dec['toolErrors'])} errors · "
             f"source canary {'intact' if (rec.get('canary') or {}).get('intact') else 'NOT intact'}",
             "",
             "Each section is one decision the harness read from the run's trace. *Said before acting* is "
             "the agent's own words at the time; *explained after the run* was asked once the run was over "
             "and its record frozen.", ""]
    if why_failed:
        lines += [f"**Explained after the run:** {why_failed}", ""]
    lines += [f"**Stage boards:** {n}" for n in notes.get("", [])]
    lines += ["## Decisions", ""] if not notes.get("") else ["", "## Decisions", ""]
    for s in dec["sections"]:
        lines.append(heading(s))
        for u in s["units"]:
            out = _outcome_text(u)
            lines.append(f"- **Did:** {u['did']}" + (f" → {out}" if out else ""))
        if s["lookedAt"]:
            lines.append("- **Looked at:** " + " · ".join(s["lookedAt"]))
        if s["triedFirst"]:
            lines.append("- **Tried first:** " + " · ".join(s["triedFirst"]))
        for c in caps.get(s["id"], []):
            lines.append(f"- **Picture:** {c}")
        for n in notes.get(s["id"], []):
            lines.append(f"- **Stage board:** {n}")
        if s["said"]:
            lines += ["- **Said before acting:**", _quote("\n\n".join(s["said"]))]
        if why and why.get(s["id"]):
            lines += ["- **Explained after the run:**", _quote(why[s["id"]])]
        lines.append("")
    lines += ["## Tool errors (unscored)", ""]
    lines += [f"- `{e['tool']}`: {_short(e['error'], 300)}" for e in dec["toolErrors"]] or ["None."]
    return "\n".join(lines) + "\n"


# ── pictures ─────────────────────────────────────────────────────────────────────────────────────

def _get(api: str, path: str, params: dict, timeout: int = 120):
    """A JSON GET on the app's API (no client header: the harness, not Claude, is the caller)."""
    with urllib.request.urlopen(f"{api.rstrip('/')}{path}?{urllib.parse.urlencode(params)}",
                                timeout=timeout) as r:
        return json.loads(r.read().decode("utf-8"))


def _post(api: str, path: str, body: dict):
    req = urllib.request.Request(api.rstrip("/") + path, data=json.dumps(body).encode("utf-8"),
                                 headers={"Content-Type": "application/json"}, method="POST")
    with urllib.request.urlopen(req, timeout=60) as r:
        return json.loads(r.read().decode("utf-8"))


def gate_picture(api: str, copy_uid: str, copy_img: str, u: dict) -> str | None:
    """The gate plot of a gate unit's parent with its gates, from the copy as it is now."""
    g = u.get("gate") or {}
    if not g.get("x_channel"):
        return None
    q = {"projectUid": copy_uid, "imageUid": copy_img, "valueName": u["target"], "x": g["x_channel"],
         "y": g["y_channel"], "pop": u.get("parent") or "root"}
    for a in ("x", "y"):
        t = dict(g.get(f"{a}_transform") or {})
        q[f"{a}t"] = t.pop("kind", "linear")
        q.update({f"{a}{k}": v for k, v in t.items()})
    try:
        return _get(api, "/api/gating/plot-image", q).get("png")
    except (urllib.error.URLError, OSError, ValueError):
        return None


def pop_stats(api: str, copy_uid: str, copy_img: str, vn: str, pop: str) -> str:
    try:
        s = _get(api, "/api/gating/stats", {"projectUid": copy_uid, "imageUid": copy_img, "valueName": vn,
                                            "pop": pop})
        return f"{s['count']} / {s['parentCount']} cells now"
    except (urllib.error.URLError, OSError, ValueError, KeyError):
        return ""


def pictures(api: str | None, run: dict, dec: dict) -> dict:
    """{sectionId: [(png base64, caption, address)]} — the trace's pictures, else a re-rendered gate
    plot for a gate decision (needs the copy and the app)."""
    copy_of = {im["sourceImageUid"]: im["imageUid"] for im in app_project.copy_images(run)}
    out = {}
    for s in dec["sections"]:
        got = [(p["png"], f"{p['caption']} — the picture the agent got", {}) for p in s["pictures"]]
        if s["step"] == "gate" and api:
            img = copy_of.get(s["images"][0]) if s["images"] else None
            for u in s["units"]:
                if img and u["fn"] != "delete_gate":
                    u["stats"] = pop_stats(api, run["projectUid"], img, u["target"], u["pop"])
            u = s["units"][-1]
            if not got and img and u["fn"] != "delete_gate":
                png = gate_picture(api, run["projectUid"], img, u)
                if png:
                    g = u["gate"]
                    got.append((png, f"gate plot of {u.get('parent') or 'root'} on {u['target']} "
                                     f"({s['images'][0]}), x {g['x_channel']}, y {g['y_channel']} — "
                                     "rendered after the run, gates as they ended", {}))
        if got:
            out[s["id"]] = got
    return out


def upload_capture(api: str, source_uid: str, png_b64: str, caption: str, address: dict) -> str:
    """One `agent_run` capture in the source project. Read back: an app that predates the surface
    files it as a viewer frame the user shared — removed again and refused."""
    r = _post(api, "/api/viewer/capture", {"projectUid": source_uid, "surface": SURFACE, "noPush": True,
                                           "address": address, "notes": caption,
                                           "frames": [{"png": png_b64}]})
    cid = r["captureId"]
    got = _get(api, "/api/viewer/capture", {"projectUid": source_uid, "captureId": cid})
    if (got.get("capture") or {}).get("surface") != SURFACE:
        _post(api, "/api/viewer/capture/delete", {"projectUid": source_uid, "captureId": cid})
        raise SystemExit("the running app predates the `agent_run` capture surface — restart it on "
                         "origin/main, then re-run this record")
    return cid


# ── the why, after the run ───────────────────────────────────────────────────────────────────────

WHY_PROMPT = ("Your run is over and its record is frozen: nothing you say now changes it. Below is each "
              "decision you made, as the record names it. For each, say in one to three sentences why you "
              "made it, as you remember it. Reply with ONLY a JSON object mapping the id to your answer, "
              "e.g. {\"d01\": \"...\"}.\n\n")


class WhyFailed(RuntimeError):
    """The why turn failed for a reason other than the usage limit; the message is the CLI's."""


def ask_why(root: pathlib.Path, session_id: str, dec: dict, claude: str = "claude",
            budget_usd: float = 2.0) -> dict:
    """Resume the finished session once (no tools) and ask why for every decision. Raises
    `claude_cli.RateLimited` on the usage limit and `WhyFailed` on any other CLI failure — never an
    empty answer that reads as "the agent had nothing to say"."""
    prompt = WHY_PROMPT + "\n".join(heading(s).removeprefix("### ") for s in dec["sections"])
    cmd = [claude, "-p", "--resume", session_id, "--tools", "", "--strict-mcp-config",
           "--mcp-config", json.dumps({"mcpServers": {}}), "--max-budget-usd", str(budget_usd),
           "--output-format", "json"]
    r = subprocess.run(cmd, input=prompt, capture_output=True, text=True, cwd=str(root / "cwd"),
                       timeout=900, encoding="utf-8")
    out = claude_cli.read_result(r)
    if r.returncode != 0 or out.get("is_error"):
        raise WhyFailed(f"exit {r.returncode}: {claude_cli.failure_text(r, out)}")
    reply = out.get("result") or ""
    start, end = reply.find("{"), reply.rfind("}")
    got = _json(reply[start:end + 1]) if start >= 0 else None
    return {k: str(v) for k, v in (got or {}).items() if isinstance(k, str)}


# ── write ────────────────────────────────────────────────────────────────────────────────────────

def source_set_name(projects_dir: pathlib.Path, source_uid: str, image_uids: list[str]) -> str:
    for p in (projects_dir / source_uid / "1").glob("*/ccid.json"):
        try:
            d = json.loads(p.read_text(encoding="utf-8"))
        except (OSError, json.JSONDecodeError):
            continue
        if d.get("class") == "CciaSet" and set(image_uids) <= set(d.get("image_uids") or []):
            return d.get("name") or d.get("uid", "")
    return ""


def results(api: str | None, run: dict, dec: dict, stamp: str) -> tuple[dict, dict]:
    """The stages' board pictures + notes — never raises: the record must be written without them."""
    if not api:
        return {}, {}
    try:
        return stage_boards.stage_pictures(api, run, dec, stamp)
    except Exception as e:  # noqa: BLE001 — anything here costs the pictures, never the record
        return {}, {"": [f"not rendered — {type(e).__name__}: {e}"]}


def _run_end(rec: dict) -> _dt.datetime | None:
    """When the run stopped (start + wall-clock), so a reset time is read against the run's clock,
    not a back-fill's. None when the record does not say."""
    try:
        return _dt.datetime.fromisoformat(rec["startedAtUtc"]) + _dt.timedelta(seconds=int(rec.get("wallS") or 0))
    except (KeyError, TypeError, ValueError):
        return None


def write(root: pathlib.Path, api: str | None, projects_dir: pathlib.Path | None, dry_run: pathlib.Path | None,
          why: bool = False, claude: str = "claude", boards: bool = True) -> dict:
    run = json.loads((root / "run.json").read_text(encoding="utf-8"))
    rec_path = root / "record.json"
    rec = json.loads(rec_path.read_text(encoding="utf-8")) if rec_path.exists() else {}
    run.setdefault("projectName", (rec.get("copy") or {}).get("projectName") or
                   f"Agent run {rec.get('startedAt', '')}")
    images = app_project.copy_images(run)
    source_uid = run["source"]["projectUid"]
    dec = decisions(str(root / "trace.jsonl"), images)
    answers, why_failed = None, None
    if why and dec.get("rateLimited"):
        why_failed = "not asked — the run itself was stopped by the usage limit"
    elif why and dec.get("sessionId"):
        try:
            answers = ask_why(root, dec["sessionId"], dec, claude)
        except claude_cli.RateLimited as e:
            why_failed = f"not answered — usage limit: {_short(str(e), 300)}"
        except (WhyFailed, OSError, subprocess.TimeoutExpired) as e:
            why_failed = f"not answered — {e}"
    pics = pictures(api, run, dec)
    stage_pics, notes = results(api, run, dec, root.name) if boards else ({}, {})
    for sid, got in stage_pics.items():
        pics.setdefault(sid, []).extend(got)
    set_name = source_set_name(projects_dir, source_uid, [im["sourceImageUid"] for im in images]) \
        if projects_dir else ""
    title = title_of(run, rec, root.name, set_name or source_uid) + \
        (" · stopped by usage limit" if dec.get("rateLimited") else "")
    caps: dict[str, list[str]] = {}
    attach = []
    if dry_run:
        dry_run.mkdir(parents=True, exist_ok=True)
    for sid, got in pics.items():
        for k, (png, caption, address) in enumerate(got, 1):
            if dry_run:
                name = f"{sid}-{k}.png"
                with write_atomic(dry_run / name, "wb") as f:
                    f.write(base64.b64decode(png))
                caps.setdefault(sid, []).append(f"`{name}` — {caption}")
            else:
                addr = {"projectUid": run["projectUid"], "section": sid, "run": root.name, **address}
                cid = upload_capture(api, source_uid, png, caption, addr)
                attach.append(cid)
                caps.setdefault(sid, []).append(f"`{cid}` — {caption}")
    content = render(run, rec, dec, answers, caps, notes, why_failed)
    out = {"title": title, "sections": len(dec["sections"]), "toolErrors": len(dec["toolErrors"]),
           "pictures": sum(len(v) for v in pics.values()), "why": bool(answers), "whyFailed": why_failed,
           "rateLimited": bool(dec.get("rateLimited"))}
    write_json_atomic(root / "decisions.json", json.loads(json.dumps({**dec, "why": answers}, default=str)),
                      indent=2)
    if dry_run:
        with write_atomic(dry_run / "entry.md") as f:
            f.write(f"# {title}\n\n{content}")
        return {**out, "dryRun": str(dry_run)}
    # the marker the Blackboard's "agent runs" filter and the section verdicts work from
    agent_run = {"copyProjectUid": run["projectUid"], "copyProjectName": run["projectName"],
                 "startedAt": rec.get("startedAt", ""), "run": root.name,
                 "images": [{"sourceImageUid": im["sourceImageUid"], "imageUid": im["imageUid"]}
                            for im in images],
                 "sectionIds": [s["id"] for s in dec["sections"]],
                 "knowledge": [k["entryId"] for k in run.get("knowledge") or []],
                 "knowledgeOn": knowledge_on(run, rec), "guide": rec.get("guide"), "codeSha": rec.get("codeSha"),
                 **({"rateLimited": claude_cli.rate_limit_note(dec["rateLimited"], _run_end(rec))}
                    if dec.get("rateLimited") else {})}
    r = _post(api, "/api/blackboard/create", {"projectUid": source_uid, "title": title, "content": content,
                                              "attachments": attach, "agentRun": agent_run})
    return {**out, "entryId": r.get("entryId"), "projectUid": source_uid}


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("root", help="a run's directory (run.json, trace.jsonl, record.json)")
    ap.add_argument("--projects-dir", default=None)
    ap.add_argument("--api-url", default=os.environ.get("CECELIA_API_URL", "http://127.0.0.1:8080"))
    ap.add_argument("--dry-run", default=None, help="write entry.md + pictures here; post nothing")
    ap.add_argument("--ask-why", action="store_true")
    ap.add_argument("--claude", default="claude")
    ap.add_argument("--no-stage-boards", action="store_true", help="skip the per-stage board pictures")
    a = ap.parse_args(argv)
    out = write(pathlib.Path(a.root).expanduser().resolve(), a.api_url,
                pathlib.Path(a.projects_dir).expanduser() if a.projects_dir else None,
                pathlib.Path(a.dry_run).expanduser() if a.dry_run else None, a.ask_why, a.claude,
                not a.no_stage_boards)
    print(json.dumps(out))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
