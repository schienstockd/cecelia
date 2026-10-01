#!/usr/bin/env python3
"""CLAUDE.md eval — run records: one JSON per pass, rendered to markdown.

Design: docs/todo/CLAUDE_MD_EVAL_SUPERVISOR_PLAN.md → *Decisions* 2–3, *Run record (fields)*.

A record is what a fresh session acts on cold: the pass's scores attached to its traces, the
findings triaged from them, and the next actions. The JSON is the source; the markdown is
rendered from it and never edited. Records live in `~/.cecelia-effectiveness/eval-runs/`
(beside `events.jsonl`, so they follow `CECELIA_EFFECTIVENESS_LOG` in tests). A PR mirrors them
to `docs/ai-assist/eval-runs/`, and a superseded PR loses nothing.

`replay` builds a record from a pass already in the log. Every saved trace is rescored with
today's scorer, so `results.raw` is what the pass logged and `results.rescored` is what it
scores now. Findings, proposals and next actions come from an `--annotations` JSON; until the
supervisor exists (phase 3), a session writes that file by hand.

Usage:
    pixi run claude-md-eval-record replay --date 2026-09-30 --annotations notes.json [--mirror]
    pixi run claude-md-eval-record render 2026-09-30 [--mirror]
"""
from __future__ import annotations

import argparse
import hashlib
import importlib.util as _importlib_util
import json
import pathlib
import re
import sys
import typing as _t

_REPO = pathlib.Path(__file__).resolve().parents[2]
MIRROR_REL = pathlib.PurePosixPath("docs/ai-assist/eval-runs")   # in any checkout of the repo
_MIRROR_DIR = _REPO / MIRROR_REL

sys.path.insert(0, str(_REPO / "python"))
from cecelia.effectiveness import read_events  # noqa: E402
from cecelia.effectiveness.git_context import git_output  # noqa: E402
from cecelia.effectiveness.eval_staleness import eval_store  # noqa: E402
from cecelia.utils.atomic_io import write_atomic, write_json_atomic  # noqa: E402


def _load_sibling(name: str):
    spec = _importlib_util.spec_from_file_location(f"_ce_{name}", pathlib.Path(__file__).parent / f"{name}.py")
    mod = _importlib_util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


_run_prompt = _load_sibling("run_prompt")
_rollup = _load_sibling("rollup")

SCHEMA_VERSION = 1
FINDING_CLASSES = ("scorer_bug", "infra", "genuine", "decision")
FINDING_STATUSES = ("open", "resolved", "dropped")
RECURRENCE = ("recurring", "watch")
PROPOSAL_KINDS = ("setup", "scorer", "retire", "add")
VERDICTS = ("compliant", "noncompliant", "error")

_HOW_TO_USE = (
    "Point a session at this file. Do the open Next actions in order, then update each finding's "
    "status. Generated from `{json}` — edit the JSON, never this page.")


class RecordError(ValueError):
    pass


def store_root() -> pathlib.Path:
    return eval_store()


def sandbox_hash(settings: dict | None = None) -> str:
    """Short content hash of the eval agent's sandbox settings, so scores compare per setting."""
    settings = _run_prompt._SANDBOX_SETTINGS if settings is None else settings
    return hashlib.sha256(json.dumps(settings, sort_keys=True).encode()).hexdigest()[:12]


def prompt_set(ids: _t.Sequence[str], prompts_dir: pathlib.Path | None = None) -> dict:
    """The ids plus one hash over their prompt files: a scorer or wording change moves the hash."""
    prompts_dir = prompts_dir or _run_prompt._PROMPTS_DIR
    h = hashlib.sha256()
    for pid in sorted(ids):
        path = prompts_dir / f"{pid}.md"
        h.update(pid.encode() + b"\0" + (path.read_bytes() if path.is_file() else b"<missing>") + b"\0")
    return {"ids": sorted(ids), "hash": h.hexdigest()[:12]}


def _git(*args: str) -> str | None:
    return git_output(*args, cwd=str(_REPO))


def setup_size(ref: str) -> dict:
    """The setup the agents were given, measured at `ref` (Decision 15: growth over 10% is flagged)."""
    def lines(path: str) -> int | None:
        text = _git("show", f"{ref}:{path}")
        return None if text is None else len(text.splitlines())

    def count(tree: str, suffix: str = "") -> int:
        names = (_git("ls-tree", "--name-only", f"{ref}:{tree}") or "").split()
        return sum(1 for n in names if n.endswith(suffix))

    def claude_hooks() -> int:
        try:
            hooks = json.loads(_git("show", f"{ref}:.claude/settings.json") or "{}").get("hooks", {})
        except ValueError:
            return 0
        return sum(len(m.get("hooks", [])) for matchers in hooks.values() for m in matchers)

    return {"ref": ref, "claude_md_lines": lines("CLAUDE.md"),
            "frontend_claude_md_lines": lines("frontend/CLAUDE.md"),
            # git hooks + Claude Code hooks: both gate what an agent can do
            "hook_count": count(".githooks") + claude_hooks(),
            "inventory_doc_count": count("docs/inventory", ".md")}


def _trace_header(trace_dir: pathlib.Path) -> dict:
    """`claude_code_version` + `model` from the stream's init event; `{}` if it isn't there."""
    try:
        with (trace_dir / "stream.jsonl").open(encoding="utf-8") as fh:
            first = json.loads(fh.readline() or "{}")
    except (OSError, ValueError):
        return {}
    return {k: first[k] for k in ("claude_code_version", "model") if k in first}


def rescore(trace_dir: pathlib.Path, prompt_id: str) -> str | None:
    """Today's verdict on a saved trace, or None when the trace or prompt is gone.

    An errored run has nothing to rescore; it stays `error`.
    """
    prompt_path = _run_prompt._PROMPTS_DIR / f"{prompt_id}.md"
    try:
        meta_saved = json.loads((trace_dir / "meta.json").read_text(encoding="utf-8"))
        if meta_saved.get("error"):
            return "error"
        diff = (trace_dir / "diff.patch").read_text(encoding="utf-8")
        stream = (trace_dir / "stream.jsonl").read_text(encoding="utf-8")
        meta, _ = _run_prompt.parse_prompt(prompt_path)
    except (OSError, ValueError):
        return None
    verdict, _ = _run_prompt.score_all(diff, _run_prompt.parse_stream_json(stream), meta)
    return verdict


def _tally(verdicts: _t.Iterable[str | None]) -> dict:
    vs = list(verdicts)
    return {v: vs.count(v) for v in VERDICTS} | {"total": len(vs)}


def find_suite(events: _t.Sequence[dict], date: str) -> dict:
    """The day's pass: its last full-catalog suite row, else its last suite row."""
    day = [e for e in events if e.get("event") == "claude_md_eval_suite" and e["ts"].startswith(date)]
    if not day:
        raise RecordError(f"no claude_md_eval_suite row on {date}")
    full = [e for e in day if _rollup._is_full_pass(e)]
    return max(full or day, key=lambda e: e["ts"])


def build(events: _t.Sequence[dict], date: str, *, annotations: dict | None = None,
          ref: str | None = None, sandboxed: bool = False, suite: dict | None = None) -> dict:
    """Assemble the record for the pass on `date` (or for `suite`, when the caller knows its row).

    Pure apart from reading traces and git.
    """
    suite = suite or find_suite(events, date)
    runs = _rollup.pass_runs(events, suite)
    traces, header = [], {}
    for row in sorted(runs, key=lambda e: e["ts"]):
        p = row["payload"]
        trace_dir = pathlib.Path(p["trace_dir"]) if p.get("trace_dir") else None
        if trace_dir is not None and not header:
            header = _trace_header(trace_dir)
        traces.append({
            "trace": str(trace_dir) if trace_dir else None,
            "prompt_id": p["prompt_id"], "run_number": p.get("run_number"), "arm": p.get("arm", "with"),
            "cost_usd": p.get("cost_usd"), "turns": p.get("turns"), "error": p.get("error"),
            "scores": {"raw": p["verdict"],
                       "rescored": rescore(trace_dir, p["prompt_id"]) if trace_dir else None},
        })
    ids = suite["payload"].get("prompt_ids") or []
    per_prompt = {pid: {"raw": _tally(t["scores"]["raw"] for t in traces if t["prompt_id"] == pid),
                        "rescored": _tally(t["scores"]["rescored"] for t in traces if t["prompt_id"] == pid)}
                  for pid in ids}
    # Rescoring reads the prompts in this checkout, so that is the scorer version; `ref` is only
    # where setup size is measured (the commit the agents ran on, when the caller knows it).
    head = _git("rev-parse", "HEAD")
    size_ref = (_git("rev-parse", ref) or ref) if ref else head
    notes = annotations or {}
    findings = notes.get("findings", [])
    proposals = notes.get("proposals", [])
    record = {
        "schema_version": SCHEMA_VERSION,
        "kind": "pass",
        "date": date,
        "run": {
            "suite_ts": suite["ts"], "session": suite.get("session"), "branch": suite.get("branch"),
            # The log's `commit` is the CLAUDE.md blob, not the repo SHA (run_prompt.py header).
            # A replay can't recover the pinned SHA; the supervisor records it (Decision 5).
            "sha": notes.get("sha"), "claude_md_blob": suite.get("commit"),
            "full_catalog": _rollup._is_full_pass(suite), "arm": _rollup._suite_arm(suite),
            "runs_per_prompt": suite["payload"].get("runs_per_prompt"),
            "prompt_set": prompt_set(ids),
            "scored_at": head,
            "sandbox": sandbox_hash() if sandboxed else None,
            "claude_code_version": header.get("claude_code_version"), "model": header.get("model"),
            "cost_usd": suite["payload"].get("totals", {}).get("cost_usd"),
            "retries": notes.get("retries", []),
            # The supervisor's own judge spend, kept apart from the suite's (Decision 4).
            "supervisor": notes.get("supervisor"),
            "trace_root": str(_run_prompt.trace_root()),
        },
        "results": {"raw": _tally(t["scores"]["raw"] for t in traces),
                    "rescored": _tally(t["scores"]["rescored"] for t in traces),
                    "per_prompt": per_prompt, "candidates": notes.get("candidates", [])},
        "traces": traces,
        "findings": findings,
        "proposals": proposals,
        # Filled by `with_delta` against the previous record in the store.
        "delta": None,
        "tracking": {"setup_size": setup_size(size_ref or "HEAD")},
        "next_actions": notes.get("next_actions", []),
        # What the owner has to answer (Decision 3: a review queue, not a markdown edit).
        "queue": ([{"kind": "decision", "ref": f["id"]} for f in findings
                   if f.get("class") == "decision" and f.get("status") == "open"]
                  + [{"kind": "proposal", "ref": p["id"]} for p in proposals]),
    }
    return record


def failure_record(date: str, *, stage: str, error: str, sha: str | None = None) -> dict:
    """The minimal record a crashed run still leaves (Decision 13), so the gap is visible."""
    return {"schema_version": SCHEMA_VERSION, "kind": "failure", "date": date,
            "run": {"stage": stage, "error": error, "sha": sha}}


def pass_records(before: str | None = None) -> list[dict]:
    """Every valid pass record in the store, oldest first; only those dated before `before` if given.

    Failure records and anything that doesn't validate are skipped.
    """
    out = []
    for path in sorted(store_root().glob("*.json")):
        if before is not None and path.stem >= before:
            continue
        try:
            record = load(path)
        except RecordError:
            continue
        if record.get("kind", "pass") == "pass":
            out.append(record)
    return out


def latest_before(date: str) -> dict | None:
    """The newest valid pass record in the store dated before `date`; None if there isn't one."""
    records = pass_records(before=date)
    return records[-1] if records else None


_HYPOTHESIS_RE = re.compile(r"expect\s+`?([\w-]+)`?\s+to\b")


def delta(record: dict, previous: dict | None) -> dict | None:
    """What changed since `previous`: findings, score, versions, and whether each hypothesis held.

    Findings match on (slug, class). A score is only comparable when the prompt set, sandbox and
    CLAUDE.md are unchanged (Decision 1), so every change is named next to it.
    """
    if previous is None:
        return None
    key = lambda f: (f.get("slug"), f.get("class"))   # noqa: E731
    before = {key(f): f for f in previous.get("findings", []) if f.get("status") == "open"}
    now = {key(f): f for f in record.get("findings", [])}
    prun, run = previous["run"], record["run"]
    changed = [name for name, a, b in (
        ("prompt set", prun["prompt_set"]["hash"], run["prompt_set"]["hash"]),
        ("sandbox", prun.get("sandbox"), run.get("sandbox")),
        ("CLAUDE.md", prun.get("claude_md_blob"), run.get("claude_md_blob")),
        ("Claude Code", prun.get("claude_code_version"), run.get("claude_code_version"))) if a != b]
    per_prompt = record["results"]["per_prompt"]
    hypotheses = []
    for prop in previous.get("proposals", []):
        m = _HYPOTHESIS_RE.search(prop.get("hypothesis") or "")
        if not m:
            continue
        pid = m.group(1)
        tally = per_prompt.get(pid, {}).get("raw")
        held = None if not tally or not tally["total"] else tally["compliant"] == tally["total"]
        hypotheses.append({"proposal": prop["id"], "prompt": pid, "hypothesis": prop["hypothesis"],
                           "held": held, "score": _score(tally) if tally else None})
    return {
        "previous": previous["date"],
        "opened": [now[k]["id"] for k in now if k not in before],
        "still_open": [now[k]["id"] for k in now if k in before],
        "resolved": [f"{before[k]['id']} `{k[0]}` {k[1]}" for k in before if k not in now],
        "score": {"previous": _score(previous["results"]["raw"]), "now": _score(record["results"]["raw"])},
        "changed": changed,
        "hypotheses": hypotheses,
    }


def with_delta(record: dict) -> dict:
    """`record` with its delta against the newest earlier pass record in the store."""
    return {**record, "delta": delta(record, latest_before(record["date"]))}


_REQUIRED = {
    "": ("schema_version", "date", "run", "results", "traces", "findings", "proposals", "delta",
         "tracking", "next_actions", "queue"),
    "run": ("suite_ts", "sha", "claude_md_blob", "prompt_set", "sandbox", "cost_usd", "retries"),
    "results": ("raw", "rescored", "per_prompt", "candidates"),
    "finding": ("id", "slug", "class", "status", "recurrence", "evidence", "diagnosis", "proposed_fix"),
    "next_action": ("title", "files", "change", "verify"),
    "proposal": ("id", "kind", "summary", "sources"),
    "failure": ("schema_version", "date", "kind", "run"),
    "failure_run": ("stage", "error", "sha"),
}


def validate(record: dict) -> list[str]:
    """Every problem with `record`, as readable strings; `[]` when it's valid."""
    errs: list[str] = []

    def need(obj: dict, where: str, keys: _t.Iterable[str]) -> None:
        errs.extend(f"{where or 'record'}: missing `{k}`" for k in keys if k not in obj)

    if record.get("schema_version") != SCHEMA_VERSION:
        return [f"schema_version {record.get('schema_version')!r}, this code reads {SCHEMA_VERSION}"]
    if record.get("kind") == "failure":
        need(record, "", _REQUIRED["failure"])
        need(record.get("run", {}), "run", _REQUIRED["failure_run"])
        return errs
    need(record, "", _REQUIRED[""])
    need(record.get("run", {}), "run", _REQUIRED["run"])
    need(record.get("results", {}), "results", _REQUIRED["results"])
    ids = [f.get("id") for f in record.get("findings", [])]
    if len(ids) != len(set(ids)):
        errs.append("findings: duplicate ids")
    for f in record.get("findings", []):
        where = f"finding {f.get('id', '?')}"
        need(f, where, _REQUIRED["finding"])
        for key, allowed in (("class", FINDING_CLASSES), ("status", FINDING_STATUSES),
                             ("recurrence", RECURRENCE)):
            if key in f and f[key] not in allowed:
                errs.append(f"{where}: {key} {f[key]!r} not in {allowed}")
        for ev in f.get("evidence", []):
            if not ev.get("trace") or not ev.get("excerpt"):
                errs.append(f"{where}: evidence needs a `trace` and an `excerpt`")
    for p in record.get("proposals", []):
        where = f"proposal {p.get('id', '?')}"
        need(p, where, _REQUIRED["proposal"])
        if "kind" in p and p["kind"] not in PROPOSAL_KINDS:
            errs.append(f"{where}: kind {p['kind']!r} not in {PROPOSAL_KINDS}")
    for i, a in enumerate(record.get("next_actions", [])):
        need(a, f"next_action {i + 1}", _REQUIRED["next_action"])
    for t in record.get("traces", []):
        for stage, v in t.get("scores", {}).items():
            if v is not None and v not in VERDICTS:
                errs.append(f"trace {t.get('trace')}: {stage} verdict {v!r}")
    for item in record.get("queue", []):
        if item.get("ref") not in ids + [p.get("id") for p in record.get("proposals", [])]:
            errs.append(f"queue: {item.get('ref')!r} names no finding or proposal")
    return errs


def load(path: pathlib.Path) -> dict:
    """Read and validate one record; a missing file or another schema is a clear error, not a KeyError."""
    if not path.is_file():
        raise RecordError(f"no run record at {path}")
    try:
        record = json.loads(path.read_text(encoding="utf-8"))
    except ValueError as e:
        raise RecordError(f"{path} is not JSON: {e}") from e
    errs = validate(record)
    if errs:
        raise RecordError(f"{path}:\n  " + "\n  ".join(errs))
    return record


_ANNOTATION_KEYS = ("findings", "proposals", "next_actions", "candidates", "retries", "sha", "supervisor")


def carried_annotations(path: pathlib.Path) -> dict | None:
    """An existing record's annotations, so a `--force` rescore doesn't drop its findings."""
    if not path.is_file():
        return None
    record = load(path)
    if record.get("kind") == "failure":
        return None
    return {k: (record["run"] if k in ("retries", "sha", "supervisor") else record["results"] if k == "candidates"
                else record).get(k) for k in _ANNOTATION_KEYS}


def _score(t: dict) -> str:
    return f"{t.get('compliant', 0)}/{t.get('total', 0)}"


def _cell(text: _t.Any) -> str:
    return str(text if text is not None else "—").replace("|", "\\|").replace("\n", " ")


def render_markdown(record: dict) -> str:
    if record.get("kind") == "failure":
        run = record["run"]
        return (f"# CLAUDE.md eval run — {record['date']} — FAILED\n\n"
                f"The supervised pass stopped at **{run['stage']}** and scored nothing.\n\n"
                f"- Pinned SHA: {run['sha'] or 'not pinned yet'}\n"
                f"- Error: `{_cell(run['error'])}`\n\n"
                "Next action: read the cron log in `~/.cecelia-effectiveness/cron/`, fix the cause, "
                "and rerun `pixi run claude-md-eval-supervise`.\n")
    run, res = record["run"], record["results"]
    out = [f"# CLAUDE.md eval run — {record['date']}", "",
           _HOW_TO_USE.format(json=f"{record['date']}.json"), "",
           "## Run", "",
           "| | |", "|---|---|"]
    meta = [
        ("Pass", f"{run['suite_ts']} · {'full catalog' if run.get('full_catalog') else 'subset'} · "
                 f"N={run.get('runs_per_prompt')} · arm={run.get('arm')}"),
        ("Pinned SHA", run["sha"] or "not recorded (replayed from the log)"),
        ("CLAUDE.md blob", run["claude_md_blob"]),
        ("Prompt set", f"{len(run['prompt_set']['ids'])} prompts · hash `{run['prompt_set']['hash']}`"),
        ("Scored at", run.get("scored_at")),
        ("Sandbox", f"`{run['sandbox']}`" if run["sandbox"] else "none"),
        ("Claude Code", f"{run.get('claude_code_version')} · {run.get('model')}"),
        ("Cost", f"${run['cost_usd']:.2f}" if run["cost_usd"] is not None else None),
        ("Retries", len(run["retries"])),
        ("Supervisor", (f"{run['supervisor'].get('judge_calls', 0)} judge call(s) · "
                        f"${run['supervisor'].get('cost_usd', 0):.2f}"
                        + (f" · {run['supervisor']['skipped']} unjudged (budget)"
                           if run["supervisor"].get("skipped") else ""))
         if run.get("supervisor") else "not supervised (replayed)"),
        ("Traces", f"`{run.get('trace_root')}`"),
    ]
    out += [f"| {k} | {_cell(v)} |" for k, v in meta]
    out += ["", "## Results", "",
            f"**{_score(res['raw'])} as logged → {_score(res['rescored'])} rescored** with the scorer at "
            f"`{(run.get('scored_at') or '?')[:8]}`.", "",
            "| Prompt | Logged | Rescored |", "|---|---|---|"]
    out += [f"| `{pid}` | {_score(v['raw'])} | {_score(v['rescored'])} |"
            for pid, v in res["per_prompt"].items()]
    if res["candidates"]:
        out += ["", "Candidates (unscored): " + ", ".join(f"`{c}`" for c in res["candidates"])]

    out += ["", "## Findings", ""]
    if not record["findings"]:
        out.append("None.")
    for f in record["findings"]:
        out += [f"### {f['id']} · `{f['slug'] or '—'}` · {f['class']} · {f['status']} · {f['recurrence']}", "",
                f["diagnosis"], "", f"**Proposed fix:** {f['proposed_fix']}", ""]
        for ev in f["evidence"]:
            out += [f"- `{ev['trace']}`", "", "  ```", *(f"  {ln}" for ln in ev["excerpt"].splitlines()),
                    "  ```"]
        out.append("")

    out += ["## Proposals", ""]
    out += [f"- **{p['id']}** {p.get('kind', '')}: {p.get('summary', '')}"
            + (f" — sources: {', '.join(p['sources'])}" if p.get("sources") else "")
            + (f"  \n  Hypothesis: {p['hypothesis']}" if p.get("hypothesis") else "")
            for p in record["proposals"]] or ["None."]

    out += ["", "## Owner queue", ""]
    out += [f"- {q['kind']}: {q['ref']}" for q in record["queue"]] or ["Empty."]

    size = record["tracking"]["setup_size"]
    out += ["", "## Since last run", ""]
    d = record.get("delta")
    if not d:
        out.append("First record; nothing to compare.")
    else:
        note = (f" — **not comparable directly:** {', '.join(d['changed'])} changed" if d["changed"]
                else " — same prompt set, sandbox, CLAUDE.md and Claude Code")
        out += [f"Against {d['previous']}: {d['score']['previous']} → {d['score']['now']}{note}.", "",
                f"- Opened: {', '.join(d['opened']) or 'none'}",
                f"- Still open: {', '.join(d['still_open']) or 'none'}",
                f"- Resolved: {', '.join(d['resolved']) or 'none'}"]
        for h in d["hypotheses"]:
            verdict = {True: "held", False: "did not hold", None: "untested (prompt not run)"}[h["held"]]
            out.append(f"- {h['proposal']} ({h['hypothesis']}): **{verdict}**"
                       + (f", `{h['prompt']}` {h['score']}" if h["score"] else ""))

    out += ["", "## Setup size", "",
            f"At `{(size.get('ref') or '?')[:8]}`: CLAUDE.md {size.get('claude_md_lines')} lines, "
            f"`frontend/CLAUDE.md` {size.get('frontend_claude_md_lines')}, "
            f"{size.get('hook_count')} hook(s), {size.get('inventory_doc_count')} inventory docs."]

    out += ["", "## Next actions", ""]
    for i, a in enumerate(record["next_actions"], 1):
        out += [f"{i}. **{a['title']}**" + (f" (finding {a['finding']})" if a.get("finding") else ""),
                f"   - Files: {', '.join(f'`{x}`' for x in a['files'])}",
                f"   - Change: {a['change']}",
                f"   - Verify: `{a['verify']}`"]
    if not record["next_actions"]:
        out.append("None.")

    out += ["", "## Traces", "", "| Trace | Logged | Rescored | Cost |", "|---|---|---|---|"]
    out += [f"| `{pathlib.Path(t['trace']).name if t['trace'] else '—'}` | {t['scores']['raw']} | "
            f"{_cell(t['scores']['rescored'])} | {_cell(t['cost_usd'])} |" for t in record["traces"]]
    return "\n".join(out) + "\n"


def write(record: dict, *, mirror: bool = False, force: bool = False,
          mirror_dir: pathlib.Path | None = None) -> list[pathlib.Path]:
    """Validate, then write the record to the local store (and its JSON + markdown to the mirror)."""
    errs = validate(record)
    if errs:
        raise RecordError("record is invalid:\n  " + "\n  ".join(errs))
    dest = store_root() / f"{record['date']}.json"
    if dest.exists() and not force:
        raise RecordError(f"{dest} exists; pass --force to replace it")
    dest.parent.mkdir(parents=True, exist_ok=True)
    write_json_atomic(dest, record, indent=2, ensure_ascii=False)
    written = [dest]
    if mirror:
        mirror_dir = mirror_dir or _MIRROR_DIR
        mirror_dir.mkdir(parents=True, exist_ok=True)
        written.append(write_json_atomic(mirror_dir / dest.name, record, indent=2, ensure_ascii=False))
        md = mirror_dir / f"{record['date']}.md"
        with write_atomic(md, encoding="utf-8") as fh:
            fh.write(render_markdown(record))
        written.append(md)
    return written


def main(argv: list[str] | None = None) -> int:
    ap = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    sub = ap.add_subparsers(dest="cmd", required=True)
    rp = sub.add_parser("replay", help="build a record from a pass already in the effectiveness log")
    rp.add_argument("--date", required=True, help="pass date, YYYY-MM-DD (UTC)")
    rp.add_argument("--annotations", type=pathlib.Path,
                    help="JSON with findings / proposals / next_actions / sha / retries "
                         "(default: kept from the existing record)")
    rp.add_argument("--ref", help="commit the pass ran on, for setup size (default HEAD)")
    rp.add_argument("--sandboxed", action="store_true", help="the pass ran with today's _SANDBOX_SETTINGS")
    rn = sub.add_parser("render", help="re-render a stored record's markdown")
    rn.add_argument("date")
    for p in (rp, rn):
        p.add_argument("--mirror", action="store_true", help=f"also write to {_MIRROR_DIR.relative_to(_REPO)}/")
    rp.add_argument("--force", action="store_true", help="replace an existing record")
    args = ap.parse_args(argv)

    try:
        if args.cmd == "replay":
            if args.annotations:
                notes = json.loads(args.annotations.read_text(encoding="utf-8"))
            else:
                notes = carried_annotations(store_root() / f"{args.date}.json")
            record = with_delta(build(list(read_events()), args.date, annotations=notes, ref=args.ref,
                                      sandboxed=args.sandboxed))
            paths = write(record, mirror=args.mirror, force=args.force)
        else:
            record = load(store_root() / f"{args.date}.json")
            paths = write(record, mirror=args.mirror, force=True)
    except RecordError as e:
        print(f"claude-md-eval-record: {e}", file=sys.stderr)
        return 1
    if record.get("kind") == "failure":
        print(f"{record['date']}: failure record (stopped at {record['run']['stage']})")
    else:
        res = record["results"]
        print(f"{record['date']}: {_score(res['raw'])} logged → {_score(res['rescored'])} rescored, "
              f"{len(record['findings'])} finding(s)")
    for p in paths:
        print(f"  wrote {p}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
