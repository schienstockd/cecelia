"""An agent run's platform errors, logged for the weekly judge (docs/todo/AGENT_RUN_REVIEW_PLAN.md P1b).

A tool error or a backend error during an unattended run is the app's fault, not the agent's: the run
record lists them unscored, and this logs each as an `agent_run_finding` event to the effectiveness
log, where the judge (`scripts/judge/bugs.py`) sweeps and verifies it like any other bug.

Row: `commit` = the SHA the run's app was on, `branch` = null (the code is landed). Payload
`{key, tool, error, file, line, desc}` — `file`/`line` only when a backend stacktrace names repo code.
`key` = "run-" + 10 hex of sha1(tool + the error with ids, paths, numbers and quoted values stripped),
so the same error is one bug however many runs hit it. One row per key per run (the judge counts
rows as runs).

Not logged: an HTTP 4xx the API answered with a reason — the agent's own bad input (a chain name with
"/", a missing set uid). Everything else goes: a 5xx, an MCP-side exception, an error with no text at
all (a lost message is itself the bug).

    pixi run python scripts/agent_eval/run_findings.py /tmp/cecelia-agent-app/<stamp> [--commit SHA] [--dry-run]
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

HERE = pathlib.Path(__file__).resolve().parent
REPO = HERE.parents[1]
sys.path.insert(0, str(HERE))
sys.path.insert(0, str(REPO / "python"))

import app_project  # noqa: E402
import run_record  # noqa: E402
from cecelia.effectiveness.log import append_event  # noqa: E402

EVENT = "agent_run_finding"
_PREFIX = re.compile(r"^Error executing tool \S+:?\s*")
_HTTP = re.compile(r"HTTP (\d{3})")
_REPO_FRAME = re.compile(r"((?:api|app|python|mcp)/[\w./-]+\.(?:jl|py)):(\d+)")


def normalise(error: str) -> str:
    """The error with what differs between runs stripped: quoted values, paths, ids, timestamps, numbers."""
    s = _PREFIX.sub("", error or "")
    s = re.sub(r"'[^']*'|\"[^\"]*\"|`[^`]*`", "<v>", s)
    s = re.sub(r"\d{4}-\d{2}-\d{2}[T ][\d:.]+Z?", "<ts>", s)
    s = re.sub(r"(?:[A-Za-z]:)?[/\\][^\s,;)]+", "<path>", s)
    s = re.sub(r"\b(?=\w*\d)(?=\w*[A-Za-z])\w{6,}\b", "<id>", s)     # hashes, capture ids, uids with a digit
    s = re.sub(r"\b(?=[A-Za-z]*[a-z])(?=[A-Za-z]*[A-Z])[A-Za-z]{6}\b", "<id>", s)   # all-letter uids (mixed case)
    s = re.sub(r"\d+(?:\.\d+)?", "<n>", s)
    return re.sub(r"\s+", " ", s).strip().lower()


def finding_key(tool: str, error: str) -> str:
    return "run-" + hashlib.sha1(f"{tool}\n{normalise(error)}".encode("utf-8")).hexdigest()[:10]


def is_agent_misuse(error: str) -> bool:
    """An HTTP 4xx the API answered with a reason: the agent's own bad input, not a platform bug."""
    m = _HTTP.search(error or "")
    if not m or not 400 <= int(m.group(1)) < 500:
        return False
    return bool(error[m.end():].strip(" :"))


def tool_findings(dec: dict, run: str) -> list[dict]:
    """The run's tool errors as findings, one per key; misuse dropped."""
    out = {}
    for e in dec["toolErrors"]:
        err = (e.get("error") or "").strip()
        if is_agent_misuse(err):
            continue
        key = finding_key(e["tool"], err)
        text = _PREFIX.sub("", err) or "(no error text)"
        out.setdefault(key, {"key": key, "tool": e["tool"], "error": text,
                             "desc": f"agent run {run}: `{e['tool']}` failed: {text[:300]}"})
    return list(out.values())


def backend_findings(api: str, start: _dt.datetime, end: _dt.datetime, run: str) -> list[dict]:
    """Backend errors logged in the run's window (the app's recent-log ring), with the first repo
    frame of the stacktrace as file:line when there is one."""
    try:
        logs = run_record._get(api, "/api/logs/recent", {}).get("logs") or []
    except Exception:  # noqa: BLE001 — a record, not a gate
        return []
    out = {}
    for row in logs:
        if row.get("level") != "error":
            continue
        try:
            ts = _dt.datetime.fromisoformat(str(row.get("ts", "")).replace("Z", "+00:00"))
        except ValueError:
            continue
        if not start <= ts <= end:
            continue
        msg = str(row.get("message") or "")
        detail = str(row.get("detail") or "")
        key = finding_key("backend", msg)
        if key in out:
            continue
        f = {"key": key, "tool": "backend", "error": msg[:500],
             "desc": f"agent run {run}: backend error: {msg[:300]}"}
        m = _REPO_FRAME.search(detail)
        if m:
            f["file"], f["line"] = m.group(1), int(m.group(2))
        out[key] = f
    return list(out.values())


def run_commit(root: pathlib.Path, rec: dict) -> str | None:
    """The SHA the run's app was on: record.json's `codeSha`, else the cron log's `code:` line."""
    if rec.get("codeSha"):
        return rec["codeSha"]
    log = root.parent / f"cron-{root.name}.log"
    if log.exists():
        m = re.search(r"^code: ([0-9a-f]{7,40})", log.read_text(encoding="utf-8", errors="replace"), re.M)
        if m:
            return m.group(1)
    return None


def emit(root: pathlib.Path, api: str | None, commit: str | None = None, dry_run: bool = False,
         log_path: pathlib.Path | None = None) -> list[dict]:
    rec_path = root / "record.json"
    rec = json.loads(rec_path.read_text(encoding="utf-8")) if rec_path.exists() else {}
    run = json.loads((root / "run.json").read_text(encoding="utf-8"))
    dec = run_record.decisions(str(root / "trace.jsonl"), app_project.copy_images(run))
    found = tool_findings(dec, root.name)
    if api and rec.get("startedAtUtc"):
        start = _dt.datetime.fromisoformat(rec["startedAtUtc"])
        found += backend_findings(api, start, start + _dt.timedelta(seconds=int(rec.get("wallS", 0)) + 60),
                                  root.name)
    commit = commit or run_commit(root, rec)
    if not dry_run:
        for f in found:
            append_event(EVENT, f, session=dec.get("sessionId"), commit=commit, branch=None, log_path=log_path)
    return [{**f, "commit": commit} for f in found]


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("root", help="a run's directory (run.json, trace.jsonl, record.json)")
    ap.add_argument("--api-url", default=os.environ.get("CECELIA_API_URL", "http://127.0.0.1:8080"))
    ap.add_argument("--commit", default=None, help="the SHA the run's app was on (default: from the record)")
    ap.add_argument("--dry-run", action="store_true", help="print the findings; log nothing")
    a = ap.parse_args(argv)
    for f in emit(pathlib.Path(a.root).expanduser().resolve(), a.api_url, a.commit, a.dry_run):
        print(json.dumps(f))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
