"""An agent run's platform errors, logged for the weekly judge (docs/todo/AGENT_RUN_REVIEW_PLAN.md P1b).

A tool error or a backend error during an unattended run is the app's fault, not the agent's: the run
record lists them unscored, and this logs each as an `agent_run_finding` event to the effectiveness
log, where the judge (`scripts/judge/bugs.py`) sweeps and verifies it like any other bug.

Row: `commit` = the SHA the run's app was on, `branch` = null (the code is landed). Payload
`{key, tool, error, file, line, desc}` — `file`/`line` only when a backend stacktrace names repo code.
`key` = "run-" + 10 hex of sha1(tool + the error with ids, paths, numbers and quoted values stripped),
so the same error is one bug however many runs hit it. One row per key per run (the judge counts
rows as runs); `run` = the run's directory name, so re-running this on a run logs nothing twice.

An HTTP 4xx the API answered with a reason is the agent's own bad input (a chain name with "/", a
missing set uid), so one alone is not a finding. It is logged as an `agent_run_misuse` observation
(the judge never reads those), keyed by `repeat_key`, and once the same key has been hit in
`REPEAT_RUNS` separate runs each run that hits it logs an `agent_run_finding` with `kind: "repeat"`
and `runs` = the count: independent agents making the same mistake means the platform isn't
guiding them. Everything else goes as a finding straight away: a 5xx, an MCP-side exception, an
error with no text at all (a lost message is itself the bug).

    pixi run python scripts/agent_eval/run_findings.py /tmp/cecelia-agent-app/<stamp> [--commit SHA] [--dry-run]
    # several runs, oldest first: a dry run counts repeats across them as if each had been logged
    pixi run python scripts/agent_eval/run_findings.py /tmp/cecelia-agent-app/2026* --dry-run --api-url ''
"""
from __future__ import annotations

import argparse
import datetime as _dt
import hashlib
import itertools
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
from cecelia.effectiveness.git_context import git_output  # noqa: E402
from cecelia.effectiveness.log import append_event, read_events  # noqa: E402

EVENT = "agent_run_finding"
MISUSE_EVENT = "agent_run_misuse"
#: Separate runs a 4xx-with-reason must recur in before it is a finding. Runs are fresh sessions
#: on raw copies that share nothing, so a second agent making the same mistake says the tool's
#: surface invites it; one hit is one agent's slip, and the reason already let it recover.
REPEAT_RUNS = 2
#: Words of a 4xx reason's opening kept as its template: the raise site's own wording, before the
#: first interpolated value. Three, because a plain-word value (a column called `volume`) can
#: follow the template's words with nothing to tell it apart.
TEMPLATE_WORDS = 3
_CLAUSE = re.compile(r"\s+[—–]\s+|;\s+|\.\s+")
_WORD = re.compile(r"[a-z]+,?")
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


def misuse_template(error: str) -> tuple[str, str]:
    """A 4xx-with-reason as (status, template): the reason's first clause, cut to its leading plain
    words (`TEMPLATE_WORDS` at most) — or the whole normalised clause when fewer than two lead it, as
    when a value comes first. Hints a fix appends later ("— e.g. ...") don't change it."""
    m = _HTTP.search(error)
    clause = _CLAUSE.split(error[m.end():].strip(" :"), maxsplit=1)[0]
    norm = normalise(clause)
    lead = list(itertools.takewhile(_WORD.fullmatch, norm.split()))
    return m.group(1), " ".join(lead[:TEMPLATE_WORDS]) if len(lead) >= 2 else norm


def repeat_key(tool: str, error: str) -> str:
    """The same mistake however its values differ: tool + status + `misuse_template`."""
    status, template = misuse_template(error)
    return "rep-" + hashlib.sha1(f"{tool}\n{status}\n{template}".encode("utf-8")).hexdigest()[:10]


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


def misuses(dec: dict) -> list[dict]:
    """The run's 4xx-with-reason errors, one per `repeat_key` (the first one's text)."""
    out = {}
    for e in dec["toolErrors"]:
        err = (e.get("error") or "").strip()
        if is_agent_misuse(err):
            status, template = misuse_template(err)
            out.setdefault(repeat_key(e["tool"], err), {"tool": e["tool"], "status": status, "template": template,
                                                        "error": _PREFIX.sub("", err)})
    return [{"key": k, **v} for k, v in out.items()]


def repeat_finding(m: dict, runs: int, run: str) -> dict:
    """A misuse seen in `runs` separate runs, framed for the judge as a guidance question."""
    return {"key": m["key"], "kind": "repeat", "tool": m["tool"], "error": m["error"], "template": m["template"],
            "runs": runs,
            "desc": f"agents hit this error in {runs} separate runs (latest {run}): `{m['tool']}` answered "
                    f"{m['error'][:300]} — each input was the agent's, but the same mistake recurring "
                    "means the platform may not be guiding them: the tool's description, the MCP guidance "
                    "or the error message itself."}


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


def full_sha(sha: str | None) -> str | None:
    """`sha` as the full 40-hex SHA (the rest of the effectiveness log carries full ones); unchanged
    when git cannot resolve it here."""
    if not sha:
        return sha
    return git_output("rev-parse", "--verify", "--quiet", f"{sha}^{{commit}}", cwd=str(REPO)) or sha


def run_commit(root: pathlib.Path, rec: dict) -> str | None:
    """The SHA the run's app was on: record.json's `codeSha`, else the cron log's `code:` line."""
    if rec.get("codeSha"):
        return full_sha(rec["codeSha"])
    log = root.parent / f"cron-{root.name}.log"
    if log.exists():
        m = re.search(r"^code: ([0-9a-f]{7,40})", log.read_text(encoding="utf-8", errors="replace"), re.M)
        if m:
            return full_sha(m.group(1))
    return None


def _logged(rows: list[dict], event: str, key: str, run: str, session: str | None) -> bool:
    """Whether this run already logged `key` — by `run`, or by session for rows written before `run`."""
    return any(r.get("event") == event and (r.get("payload") or {}).get("key") == key
               and ((r.get("payload") or {}).get("run") == run or (session and r.get("session") == session))
               for r in rows)


def emit(root: pathlib.Path, api: str | None, commit: str | None = None, dry_run: bool = False,
         log_path: pathlib.Path | None = None, pending: list[dict] | None = None) -> list[dict]:
    """Log the run's findings and misuse observations; return the findings (with `commit`).

    Idempotent per run: a key this run already logged is not logged again. A dry run writes
    nothing; rows it would have written go to `pending`, which later calls count as logged, so
    a dry run over several runs counts their repeats."""
    rec_path = root / "record.json"
    rec = json.loads(rec_path.read_text(encoding="utf-8")) if rec_path.exists() else {}
    run = json.loads((root / "run.json").read_text(encoding="utf-8"))
    dec = run_record.decisions(str(root / "trace.jsonl"), app_project.copy_images(run))
    found = tool_findings(dec, root.name)
    if api and rec.get("startedAtUtc"):
        start = _dt.datetime.fromisoformat(rec["startedAtUtc"])
        found += backend_findings(api, start, start + _dt.timedelta(seconds=int(rec.get("wallS", 0)) + 60),
                                  root.name)
    commit = full_sha(commit) or run_commit(root, rec)
    session, run_id = dec.get("sessionId"), root.name
    pending = [] if pending is None else pending
    rows = [*read_events(log_path), *pending]

    def log(event: str, payload: dict) -> None:
        if dry_run:
            row = {"event": event, "session": session, "commit": commit, "payload": payload}
        else:
            row = append_event(event, payload, session=session, commit=commit, branch=None, log_path=log_path)
        rows.append(row)
        if dry_run:
            pending.append(row)

    for m in misuses(dec):
        if not _logged(rows, MISUSE_EVENT, m["key"], run_id, session):
            log(MISUSE_EVENT, {**m, "run": run_id})
        same = [(r.get("event"), r.get("payload") or {}) for r in rows if (r.get("payload") or {}).get("key") == m["key"]]
        runs = len({p.get("run") for e, p in same if e == MISUSE_EVENT})
        told = max((int(p.get("runs") or 0) for e, p in same if e == EVENT), default=0)
        if runs >= REPEAT_RUNS and runs > told:   # only a count the log hasn't said yet
            found.append(repeat_finding(m, runs, run_id))
    out = []
    for f in found:
        if not _logged(rows, EVENT, f["key"], run_id, session):
            log(EVENT, {**f, "run": run_id})
            out.append({**f, "run": run_id, "commit": commit})
    return out


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("root", nargs="+", help="a run's directory (run.json, trace.jsonl, record.json); "
                                             "several are emitted in the order given")
    ap.add_argument("--api-url", default=os.environ.get("CECELIA_API_URL", "http://127.0.0.1:8080"))
    ap.add_argument("--commit", default=None, help="the SHA the run's app was on (default: from the record)")
    ap.add_argument("--dry-run", action="store_true", help="print the findings; log nothing")
    a = ap.parse_args(argv)
    pending: list[dict] = []
    for root in a.root:
        for f in emit(pathlib.Path(root).expanduser().resolve(), a.api_url or None, a.commit, a.dry_run,
                      pending=pending):
            print(json.dumps(f))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
