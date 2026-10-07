#!/usr/bin/env python3
"""Weekly judge — a person's causes on agent run records (docs/todo/GUIDE_RUNS_PLAN.md P2).

A run record is a Blackboard entry with meta `agentRun`, written into the project the run was copied
from (`scripts/agent_eval/run_record.py`). Its reviewer marks sections `bad` with a cause
(`api/src/blackboard_run_review.jl`, `sectionOutcomes[id].cause`):

- `guide` (the guide didn't say it) and `platform` (the agent couldn't see it) are the app's to fix.
  Each one a person set (`by.via == "app"`, not a Claude proposal) is logged once as an
  `agent_run_finding` with `kind: "review"`, keyed by project + entry + section, so the bug sweep
  (`bugs.py`) opens it and verify checks whether the fix is in. A person set the cause, so whether
  the finding is real is not the question.
- `agent` (the guide and tools were enough) stays on the record. They are listed per guide in the
  pass's record; a guide whose `agent` notes span 2+ runs is flagged for the owner to compare. The
  matching itself is a person's read: the judge has no step that compares notes.

A `bad` with no cause (marked before causes existed) is not set, and is skipped.

The projects dir: `--projects-dir`, else `CECELIA_AGENT_APP_PROJECTS`, else `~/cecelia-feijoa/projects`
(the dev config's). Not the running app's (`guide_run.app_status`): the weekly pass runs whether or
not the app is up. A missing dir reads as no records.

Usage:
    pixi run judge-run-reviews [--projects-dir D]   # print; logs nothing
"""
from __future__ import annotations

import argparse
import datetime as _dt
import hashlib
import json
import os
import pathlib
import re
import sys
import typing as _t

_REPO = pathlib.Path(__file__).resolve().parents[2]

sys.path.insert(0, str(_REPO / "python"))
from cecelia.effectiveness.log import append_event  # noqa: E402

EVENT = "agent_run_finding"
#: Causes that are the app's to fix; `agent` stays on the record.
LOGGED_CAUSES = ("guide", "platform")
CAUSE_MEANS = {"guide": "the guide didn't say it", "platform": "the agent couldn't see what it needed",
               "agent": "the guide and tools were enough"}
#: A guide's `agent` notes from this many separate runs are flagged as a possible guide gap.
GAP_RUNS = 2
DEFAULT_PROJECTS = "~/cecelia-feijoa/projects"
_HEAD = re.compile(r"^### ([dms]\d{2,3}) · (.*)$", re.M)


def projects_dir() -> pathlib.Path:
    return pathlib.Path(os.environ.get("CECELIA_AGENT_APP_PROJECTS") or DEFAULT_PROJECTS).expanduser()


def _read_json(p: pathlib.Path) -> dict | None:
    try:
        v = json.loads(p.read_text(encoding="utf-8"))
    except (OSError, ValueError):
        return None
    return v if isinstance(v, dict) else None


def review_key(project: str, entry: str, section: str) -> str:
    """One verdict, however often a pass sees it: a later edit to its note or cause is the same one."""
    return "rev-" + hashlib.sha1(f"{project}\n{entry}\n{section}".encode("utf-8")).hexdigest()[:10]


def scan(projects: pathlib.Path | None = None) -> list[dict]:
    """Every `bad` verdict with a cause that a person set on a run record, oldest entry first."""
    root = projects if projects is not None else projects_dir()
    out: list[dict] = []
    if not root.is_dir():
        return out
    for meta_path in sorted(root.glob("*/blackboard/*/meta.json")):
        meta = _read_json(meta_path)
        run = (meta or {}).get("agentRun")
        if not isinstance(run, dict):
            continue
        entry_dir = meta_path.parent
        project = entry_dir.parent.parent.name
        try:
            heads = dict(_HEAD.findall((entry_dir / "entry.md").read_text(encoding="utf-8")))
        except OSError:
            heads = {}
        for sid, o in sorted((meta.get("sectionOutcomes") or {}).items()):
            if not isinstance(o, dict) or o.get("verdict") != "bad" or o.get("cause") not in CAUSE_MEANS:
                continue
            if ((o.get("by") or {}).get("via")) != "app":   # a Claude proposal is not a review
                continue
            guide = run.get("guide") if isinstance(run.get("guide"), str) else None
            out.append({"key": review_key(project, entry_dir.name, sid), "project": project,
                        "entry": entry_dir.name, "title": meta.get("title") or "", "run": run.get("run"),
                        "guide": guide, "section": sid, "heading": heads.get(sid, ""),
                        "cause": o["cause"], "note": o.get("note") or "", "at": o.get("at"),
                        "commit": run.get("codeSha")})
    return out


def finding(v: dict) -> dict:
    """The `agent_run_finding` payload for one `guide` / `platform` verdict."""
    where = f"{v['section']} · {v['heading']}" if v["heading"] else v["section"]
    return {"key": v["key"], "kind": "review", "tool": None, "error": None, "file": None, "line": None,
            "cause": v["cause"], "note": v["note"], "guide": v["guide"], "project": v["project"],
            "entry": v["entry"], "section": v["section"], "run": v["run"],
            "desc": (f"reviewer of agent run {v['run'] or v['entry']}"
                     + (f" (guide `{v['guide']}`)" if v["guide"] else "")
                     + f" marked `{where}` bad, cause `{v['cause']}` ({CAUSE_MEANS[v['cause']]}): "
                     + v["note"][:400])}


def log_new(verdicts: _t.Iterable[dict], events: _t.Iterable[dict], *, pass_ts: str, write: bool = True,
            log_path: pathlib.Path | None = None) -> list[dict]:
    """Log each `guide` / `platform` verdict the log doesn't hold yet; return the rows. With
    `write=False` (a dry run) the rows are built, not appended.

    Rows are stamped one second before `pass_ts`, the pass's start: the sweep reads rows from the
    previous pass's start on, so a row stamped inside this pass would be read again, as new, by the
    next one (and a bug verify dismissed would re-open)."""
    ts = (_dt.datetime.fromisoformat(pass_ts.replace("Z", "+00:00")) - _dt.timedelta(seconds=1)
          ).isoformat().replace("+00:00", "Z")
    seen = {(r.get("payload") or {}).get("key") for r in events if r.get("event") == EVENT}
    rows = []
    for v in verdicts:
        if v["cause"] not in LOGGED_CAUSES or v["key"] in seen:
            continue
        seen.add(v["key"])
        payload = finding(v)
        if write:
            rows.append(append_event(EVENT, payload, session="weekly-judge", commit=v["commit"], branch=None,
                                     ts=ts, log_path=log_path))
        else:
            rows.append({"event": EVENT, "ts": ts, "session": "weekly-judge", "commit": v["commit"],
                         "branch": None, "payload": payload})
    return rows


def agent_causes(verdicts: _t.Iterable[dict]) -> list[dict]:
    """`agent` causes per guide (`null` = a record that names none), with `gap` set when they span
    `GAP_RUNS`+ separate runs: a person compares the notes for one shared guide gap."""
    by: dict[str | None, list[dict]] = {}
    for v in verdicts:
        if v["cause"] == "agent":
            by.setdefault(v["guide"], []).append(
                {k: v[k] for k in ("run", "project", "entry", "section", "heading", "note")})
    return [{"guide": g, "runs": len({n["entry"] for n in notes}),
             "gap": g is not None and len({n["entry"] for n in notes}) >= GAP_RUNS, "notes": notes}
            for g, notes in sorted(by.items(), key=lambda kv: (kv[0] is None, kv[0] or ""))]


def main(argv: list[str] | None = None) -> int:
    ap = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    ap.add_argument("--projects-dir", type=pathlib.Path, help=f"default: {projects_dir()}")
    args = ap.parse_args(argv)
    found = scan(args.projects_dir.expanduser() if args.projects_dir else None)
    print(json.dumps({"findings": [finding(v) for v in found if v["cause"] in LOGGED_CAUSES],
                      "agent_causes": agent_causes(found)}, indent=2, ensure_ascii=False))
    return 0


if __name__ == "__main__":
    sys.exit(main())
