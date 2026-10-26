"""A co-driven session's record on the Blackboard (docs/todo/CODRIVE_RECORD_PLAN.md P1): one entry
per session, one `### sNN` section per exchange that touched Cecelia, written from the Claude Code
transcript by this script — never by Claude — so a session can't leave out the step that went wrong.

An exchange is one message of yours and everything Claude did until your next one: what it read
(*looked at*), what it pointed at in the app (*pointed at*), what it wrote (*wrote*), calls that
failed (*tried first*), the pictures tools returned, and what it said. Only exchanges with a Cecelia
tool call, a capture you shared, or a `#fix` become sections; the rest are counted in the header.
`#fix` anywhere in your message marks a correction of the section before it — you still give the
verdict, as on any section.

    pixi run codrive-record [transcript.jsonl | sessionId] [--project UID] [--since ISO] [--until ISO]
                            [--dry-run DIR]

With no transcript it records the Claude Code session it runs under — so "record this co-drive" at
the end of a session is Claude running it (`.claude/skills/codrive-record/SKILL.md`).
`--dry-run DIR` writes entry.md + the pictures to DIR and posts nothing.
"""
from __future__ import annotations

import argparse
import base64
import collections
import datetime as _dt
import json
import pathlib
import re
import sys

HERE = pathlib.Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))

import run_record  # noqa: E402 — the run record's API helpers, capture upload and quoting
import trace_view  # noqa: E402
from cecelia.effectiveness.claude_sessions import calling_session, claude_home, typed_by_a_person  # noqa: E402
from cecelia.effectiveness.state import state_dir  # noqa: E402
from cecelia.utils.atomic_io import write_atomic, write_json_atomic  # noqa: E402
from cecelia.utils.loopback import default_base_url  # noqa: E402


POINT_TOOLS = {"mark_cells", "mark_tracks", "mark_tile", "mark_freeform", "mark_plot", "point_at_ui",
               "select_on_plot", "seek_viewer", "open_analysis_board_plot"}
# a run's actions (cecelia-autonomous) plus the observer's other additive writes
WRITE_TOOLS = run_record.ACTIONS | {"set_blackboard_status", "set_blackboard_outcome", "set_blackboard_section_outcome",
                                    "set_notebook_description", "create_chain", "set_labarchives_context"}
FIX_RE = re.compile(r"(?<![\w#])#fix\b", re.IGNORECASE)
SHARED_RE = re.compile(r"\[cecelia\] shared capture (cap-[0-9T]+-[0-9a-f]+) from project (\w+)")
_REMINDER_RE = re.compile(r"<system-reminder>.*?</system-reminder>", re.DOTALL)
_PASTE_RE = re.compile(r"</?pasted_content[^>]*>")    # the tags round text you pasted into a message
USER_MAX, SAID_MAX = 1500, 1200


# ── transcript → events ──────────────────────────────────────────────────────────────────────────

def _split_tool(name: str) -> tuple[str, str]:
    """`mcp__cecelia-observer__get_capture` → ("cecelia-observer", "get_capture"); built-ins → ("", name)."""
    parts = name.split("__")
    return (parts[1], parts[-1]) if len(parts) >= 3 and parts[0] == "mcp" else ("", name)


def events(path: str) -> list[dict]:
    """The transcript in order: `user` {text, ts}, `shared` {captureId, projectUid, ts}, `text` {text},
    `call` {id, server, name, input}, `result` {id, text, isError, images: [{data, mediaType}]}, and one
    `init` {sessionId, model}. Sub-agent (`isSidechain`) and meta lines are left out."""
    out, init = [], {}
    with open(path, encoding="utf-8", errors="replace") as f:
        for raw in f:
            try:
                ev = json.loads(raw)
            except json.JSONDecodeError:
                continue
            if ev.get("isSidechain") or ev.get("type") not in ("user", "assistant"):
                continue
            # user-role lines: your typed messages, tool results, and lines from elsewhere — of those,
            # only a capture you shared (a pushed meta line) is kept
            you = ev["type"] == "user" and typed_by_a_person(ev)
            msg, ts = ev.get("message") or {}, ev.get("timestamp", "")
            init.setdefault("sessionId", ev.get("sessionId"))
            if ev["type"] == "assistant" and msg.get("model") and not init.get("model"):
                init["model"] = msg["model"]
            content = msg.get("content")
            blocks = [{"type": "text", "text": content}] if isinstance(content, str) else content or []
            for b in blocks:
                if not isinstance(b, dict):
                    continue
                kind = b.get("type")
                if kind == "text" and ev["type"] == "user":
                    text = _PASTE_RE.sub("[pasted]", _REMINDER_RE.sub("", b.get("text", ""))).strip()
                    shared = None if you else SHARED_RE.search(text)
                    if shared:
                        out.append({"kind": "shared", "captureId": shared.group(1),
                                    "projectUid": shared.group(2), "ts": ts})
                    elif you and text:
                        out.append({"kind": "user", "text": text, "ts": ts})
                elif kind == "text" and b.get("text", "").strip():
                    out.append({"kind": "text", "text": b["text"].strip(), "ts": ts})
                elif kind == "tool_use":
                    server, name = _split_tool(b.get("name", "?"))
                    out.append({"kind": "call", "id": b.get("id"), "server": server, "name": name,
                                "input": b.get("input") or {}, "ts": ts})
                elif kind == "tool_result":
                    c = b.get("content")
                    imgs = [{"data": x["source"]["data"], "mediaType": x["source"].get("media_type", "")}
                            for x in (c if isinstance(c, list) else [])
                            if isinstance(x, dict) and x.get("type") == "image" and (x.get("source") or {}).get("data")]
                    out.append({"kind": "result", "id": b.get("tool_use_id"),
                                "text": trace_view._result_text(c),
                                "isError": bool(b.get("is_error")), "images": imgs, "ts": ts})
    return [{"kind": "init", **init}, *out]


def _in_window(ts: str, since: str | None, until: str | None) -> bool:
    if not ts or not (since or until):
        return True
    t = _dt.datetime.fromisoformat(ts.replace("Z", "+00:00"))
    return (not since or t >= _dt.datetime.fromisoformat(since.replace("Z", "+00:00"))) and \
           (not until or t <= _dt.datetime.fromisoformat(until.replace("Z", "+00:00")))


# ── events → exchanges ───────────────────────────────────────────────────────────────────────────

def _is_cecelia(call: dict) -> bool:
    return call["server"].startswith("cecelia")


def _call_text(call: dict) -> str:
    args = {k: v for k, v in call["input"].items() if k != "project_uid"}
    return f"{call['name']}({', '.join(f'{k}={run_record._short(v, 40)}' for k, v in args.items())})"


def exchanges(evs: list[dict], since: str | None = None, until: str | None = None) -> list[dict]:
    """The session cut at each message of yours (or capture you shared). Each exchange:
    {kind: ask|fix|shared, ts, you, captureId?, calls: [call + result], said: [text], other: n}."""
    results = {e["id"]: e for e in evs if e["kind"] == "result"}
    out: list[dict] = []
    cur = None
    for e in evs:
        if e["kind"] in ("user", "shared"):
            if not _in_window(e["ts"], since, until):
                cur = None
                continue
            cur = {"kind": "shared" if e["kind"] == "shared" else ("fix" if FIX_RE.search(e["text"]) else "ask"),
                   "ts": e["ts"], "you": e.get("text", ""), "captureId": e.get("captureId"),
                   "projectUid": e.get("projectUid"), "calls": [], "said": [], "other": 0}
            out.append(cur)
        elif cur is None:
            continue
        elif e["kind"] == "text":
            cur["said"].append(e["text"])
        elif e["kind"] == "call":
            if _is_cecelia(e):
                r = results.get(e["id"]) or {"text": "", "isError": False, "images": []}
                cur["calls"].append({**e, "result": r["text"], "isError": r["isError"], "images": r["images"]})
            else:
                cur["other"] += 1
    return out


def recorded(ex: dict) -> bool:
    """Whether an exchange becomes a section (Decision 4)."""
    return ex["kind"] in ("fix", "shared") or bool(ex["calls"])


def project_of(exs: list[dict]) -> tuple[str, dict]:
    """The project most Cecelia calls (and shared captures) name, and the count per project."""
    n = collections.Counter()
    for ex in exs:
        if ex.get("projectUid"):
            n[ex["projectUid"]] += 1
        for c in ex["calls"]:
            uid = c["input"].get("project_uid") or c["input"].get("projectUid")
            if uid:
                n[uid] += 1
    return (n.most_common(1)[0][0] if n else ""), dict(n)


def project_name(exs: list[dict], uid: str) -> str:
    """The project's name as a tool result gave it (`list_projects`, `get_project_info`, …), else ''."""
    pats = [re.compile(r'"uid":\s*"%s",\s*"name":\s*"([^"]+)"' % re.escape(uid)),
            re.compile(r'"projectUid":\s*"%s",\s*"projectName":\s*"([^"]+)"' % re.escape(uid))]
    for ex in exs:
        for c in ex["calls"]:
            for p in pats:
                m = p.search(c["result"])
                if m:
                    return m.group(1)
    return ""


def sections(exs: list[dict]) -> list[dict]:
    """The recorded exchanges with their ids; a fix names the section it corrects (the one before)."""
    out, prev = [], None
    for ex in exs:
        if not recorded(ex):
            continue
        s = {**ex, "id": f"s{len(out) + 1:02d}"}
        if ex["kind"] == "fix" and prev:
            s["corrects"] = prev
        out.append(s)
        prev = s["id"]
    return out


# ── the entry ────────────────────────────────────────────────────────────────────────────────────

def heading(s: dict) -> str:
    if s["kind"] == "shared":
        return f"### {s['id']} · you shared · {s['captureId']}"
    first = next((ln for ln in s["you"].splitlines() if ln.strip() and ln.strip() != "[pasted]"),
                 "").replace("·", "-")
    tag = f"fix · corrects {s['corrects']}" if s.get("corrects") else s["kind"]
    return f"### {s['id']} · {tag} · {run_record._short(first, 70)}"


def pictures(secs: list[dict]) -> dict:
    """{sectionId: [(kind, payload, caption)]} — `png` with base64, `capture` with a capture id, or
    `skipped` with the media type (the capture route keeps PNGs only)."""
    out: dict[str, list] = {}
    for s in secs:
        for c in s["calls"]:
            if c["name"] == "get_capture" and c["input"].get("capture_id"):
                out.setdefault(s["id"], []).append(("capture", c["input"]["capture_id"], _call_text(c)))
                continue
            for im in c["images"]:
                kind = "png" if im["mediaType"] in ("image/png", "") else "skipped"
                out.setdefault(s["id"], []).append(
                    (kind, im["data"] if kind == "png" else im["mediaType"], _call_text(c)))
    return out


def render(meta: dict, secs: list[dict], caps: dict) -> str:
    """entry.md. `caps` = {sectionId: [line]} — one line per picture, already worded."""
    lines = [f"**Session:** `{meta['sessionId']}` · {meta.get('model') or '?'} · "
             f"{meta['start']} → {meta['end']}",
             f"**Transcript:** `{meta['transcript']}`",
             f"**Exchanges:** {meta['exchanges']} · {len(secs)} recorded below · "
             f"{meta['cecelia']} Cecelia calls · {meta['fixes']} `#fix` · {meta['other']} other tool calls not shown"]
    if len(meta["projects"]) > 1:
        lines.append("**Also called into:** " + ", ".join(f"`{u}` ×{n}" for u, n in meta["projects"].items()
                                                         if u != meta["projectUid"]))
    lines += ["", "Written by `codrive_record.py` from the session transcript, not by Claude. One section per "
              "exchange that touched Cecelia. A `fix` section is where you corrected Claude; give each "
              "section a verdict.", "", "## Exchanges", ""]
    for s in secs:
        lines.append(heading(s))
        if s["you"]:
            lines += ["- **You:**", run_record._quote(run_record._short(s["you"], USER_MAX))]
        groups = (("Looked at", lambda c: c["name"] not in POINT_TOOLS | WRITE_TOOLS),
                  ("Pointed at", lambda c: c["name"] in POINT_TOOLS),
                  ("Wrote", lambda c: c["name"] in WRITE_TOOLS))
        ok = [c for c in s["calls"] if not c["isError"]]
        for label, keep in groups:
            got = [_call_text(c) for c in ok if keep(c)]
            if got:
                lines.append(f"- **{label}:** " + " · ".join(got))
        bad = [f"{_call_text(c)} → {run_record._short(c['result'], 240)}" for c in s["calls"] if c["isError"]]
        if bad:
            lines.append("- **Tried first:** " + " · ".join(bad))
        lines += [f"- **Picture:** {c}" for c in caps.get(s["id"], [])]
        if s["other"]:
            lines.append(f"- **Other tool calls:** {s['other']}")
        if s["said"]:
            lines += ["- **Claude said:**", run_record._quote("\n\n".join(run_record._short(t, SAID_MAX) for t in s["said"]))]
        lines.append("")
    return "\n".join(lines) + "\n"


# ── write ────────────────────────────────────────────────────────────────────────────────────────

def resolve(arg: str | None) -> pathlib.Path:
    """A transcript path, or a session id looked up under Claude Code's `projects/*/`; None is the
    session this runs under."""
    if arg is None:
        arg = calling_session()
        if arg is None:
            raise SystemExit("can't tell which Claude Code session this is — pass its session id "
                             "or its transcript path")
    p = pathlib.Path(arg).expanduser()
    if p.is_file():
        return p
    projects = claude_home() / "projects"
    hits = sorted(projects.glob(f"*/{arg}.jsonl"))
    if not hits:
        raise SystemExit(f"no transcript {arg!r} (a path, or a session id under {projects})")
    return hits[0]


def build(path: pathlib.Path, project: str | None = None, since: str | None = None,
          until: str | None = None) -> tuple[dict, list[dict]]:
    evs = events(str(path))
    exs = exchanges(evs, since, until)
    secs = sections(exs)
    uid, per = project_of(exs)
    uid = project or uid
    init = evs[0]
    stamps = [ex["ts"] for ex in exs if ex["ts"]]
    meta = {"sessionId": init.get("sessionId") or path.stem, "model": init.get("model"), "transcript": str(path),
            "projectUid": uid, "projectName": project_name(exs, uid), "projects": per,
            "start": min(stamps, default=""), "end": max(stamps, default=""), "exchanges": len(exs),
            "cecelia": sum(len(ex["calls"]) for ex in exs), "fixes": sum(ex["kind"] == "fix" for ex in exs),
            "other": sum(ex["other"] for ex in exs if not recorded(ex))}
    return meta, secs


def title_of(meta: dict) -> str:
    when = meta["start"][:16].replace("T", " ")
    return f"Co-drive session {when} — {meta['projectName'] or meta['projectUid']}"


def write(path: pathlib.Path, api: str | None, dry_run: pathlib.Path | None, project: str | None = None,
          since: str | None = None, until: str | None = None, records: pathlib.Path | None = None) -> dict:
    meta, secs = build(path, project, since, until)
    if not secs:
        raise SystemExit("nothing to record: no exchange in this window touched Cecelia")
    if not meta["projectUid"]:
        raise SystemExit("no project named by any Cecelia call — pass --project")
    caps: dict[str, list[str]] = {}
    attach = []
    if dry_run:
        dry_run.mkdir(parents=True, exist_ok=True)
    for sid, got in pictures(secs).items():
        for k, (kind, payload, caption) in enumerate(got, 1):
            if kind == "capture":
                line = f"`{payload}` — {caption}"
                if not dry_run:
                    attach.append(payload)
            elif kind == "skipped":
                line = f"not kept ({payload}) — {caption}"
            elif dry_run:
                name = f"{sid}-{k}.png"
                with write_atomic(dry_run / name, "wb") as f:
                    f.write(base64.b64decode(payload))
                line = f"`{name}` — {caption}"
            else:
                address = {"projectUid": meta["projectUid"], "section": sid, "session": meta["sessionId"]}
                cid = run_record.upload_capture(api, meta["projectUid"], payload, caption, address)
                attach.append(cid)
                line = f"`{cid}` — {caption}"
            caps.setdefault(sid, []).append(line)
    title = title_of(meta)
    content = render(meta, secs, caps)
    local = {**meta, "title": title,
             "sections": [{"id": s["id"], "kind": s["kind"], "ts": s["ts"], "you": s["you"],
                           "corrects": s.get("corrects"), "calls": [c["name"] for c in s["calls"]]}
                          for s in secs]}
    out = {"title": title, "sections": len(secs), "fixes": meta["fixes"], "projectUid": meta["projectUid"]}
    if dry_run:
        with write_atomic(dry_run / "entry.md") as f:
            f.write(f"# {title}\n\n{content}")
        write_json_atomic(dry_run / "record.json", local, indent=2)
        return {**out, "dryRun": str(dry_run)}
    r = run_record._post(api, "/api/blackboard/create", {"projectUid": meta["projectUid"], "title": title,
                                                         "content": content, "attachments": attach})
    records = records or state_dir() / "codrive"      # beside the effectiveness log (Decision 9)
    records.mkdir(parents=True, exist_ok=True)
    write_json_atomic(records / f"{meta['sessionId']}.json", {**local, "entryId": r.get("entryId")}, indent=2)
    return {**out, "entryId": r.get("entryId")}


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("transcript", nargs="?", default=None,
                    help="a Claude Code transcript (.jsonl) or its session id (default: the session this runs under)")
    ap.add_argument("--project", default=None, help="the project to record into (default: the one most calls name)")
    ap.add_argument("--since", default=None, help="ISO time; leave out exchanges before it")
    ap.add_argument("--until", default=None, help="ISO time; leave out exchanges after it")
    ap.add_argument("--api-url", default=default_base_url())
    ap.add_argument("--dry-run", default=None, help="write entry.md + pictures here; post nothing")
    a = ap.parse_args(argv)
    out = write(resolve(a.transcript), a.api_url, pathlib.Path(a.dry_run).expanduser() if a.dry_run else None,
                a.project, a.since, a.until)
    print(json.dumps(out))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
