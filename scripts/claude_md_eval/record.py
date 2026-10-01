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
import sys
import typing as _t

_REPO = pathlib.Path(__file__).resolve().parents[2]
_MIRROR_DIR = _REPO / "docs" / "ai-assist" / "eval-runs"

sys.path.insert(0, str(_REPO / "python"))
from cecelia.effectiveness import read_events  # noqa: E402
from cecelia.effectiveness.git_context import git_output  # noqa: E402
from cecelia.effectiveness.log import default_log_path  # noqa: E402
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
VERDICTS = ("compliant", "noncompliant", "error")

_HOW_TO_USE = (
    "Point a session at this file. Do the open Next actions in order, then update each finding's "
    "status. Generated from `{json}` — edit the JSON, never this page.")


class RecordError(ValueError):
    pass


def store_root() -> pathlib.Path:
    return default_log_path().parent / "eval-runs"


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
          ref: str | None = None, sandboxed: bool = False) -> dict:
    """Assemble the record for the pass on `date`. Pure apart from reading traces and git."""
    suite = find_suite(events, date)
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
            "trace_root": str(_run_prompt.trace_root()),
        },
        "results": {"raw": _tally(t["scores"]["raw"] for t in traces),
                    "rescored": _tally(t["scores"]["rescored"] for t in traces),
                    "per_prompt": per_prompt, "candidates": notes.get("candidates", [])},
        "traces": traces,
        "findings": findings,
        "proposals": proposals,
        # Phase 4 fills this from the previous record in the store.
        "delta": None,
        "tracking": {"setup_size": setup_size(size_ref or "HEAD")},
        "next_actions": notes.get("next_actions", []),
        # What the owner has to answer (Decision 3: a review queue, not a markdown edit).
        "queue": ([{"kind": "decision", "ref": f["id"]} for f in findings
                   if f.get("class") == "decision" and f.get("status") == "open"]
                  + [{"kind": "proposal", "ref": p["id"]} for p in proposals]),
    }
    return record


_REQUIRED = {
    "": ("schema_version", "date", "run", "results", "traces", "findings", "proposals", "delta",
         "tracking", "next_actions", "queue"),
    "run": ("suite_ts", "sha", "claude_md_blob", "prompt_set", "sandbox", "cost_usd", "retries"),
    "results": ("raw", "rescored", "per_prompt", "candidates"),
    "finding": ("id", "slug", "class", "status", "recurrence", "evidence", "diagnosis", "proposed_fix"),
    "next_action": ("title", "files", "change", "verify"),
}


def validate(record: dict) -> list[str]:
    """Every problem with `record`, as readable strings; `[]` when it's valid."""
    errs: list[str] = []

    def need(obj: dict, where: str, keys: _t.Iterable[str]) -> None:
        errs.extend(f"{where or 'record'}: missing `{k}`" for k in keys if k not in obj)

    if record.get("schema_version") != SCHEMA_VERSION:
        return [f"schema_version {record.get('schema_version')!r}, this code reads {SCHEMA_VERSION}"]
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


_ANNOTATION_KEYS = ("findings", "proposals", "next_actions", "candidates", "retries", "sha")


def carried_annotations(path: pathlib.Path) -> dict | None:
    """An existing record's annotations, so a `--force` rescore doesn't drop its findings."""
    if not path.is_file():
        return None
    record = load(path)
    return {k: (record["run"] if k in ("retries", "sha") else record["results"] if k == "candidates"
                else record)[k] for k in _ANNOTATION_KEYS}


def _score(t: dict) -> str:
    return f"{t.get('compliant', 0)}/{t.get('total', 0)}"


def _cell(text: _t.Any) -> str:
    return str(text if text is not None else "—").replace("|", "\\|").replace("\n", " ")


def render_markdown(record: dict) -> str:
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
            record = build(list(read_events()), args.date, annotations=notes, ref=args.ref,
                           sandboxed=args.sandboxed)
            paths = write(record, mirror=args.mirror, force=args.force)
        else:
            record = load(store_root() / f"{args.date}.json")
            paths = write(record, mirror=args.mirror, force=True)
    except RecordError as e:
        print(f"claude-md-eval-record: {e}", file=sys.stderr)
        return 1
    res = record["results"]
    print(f"{record['date']}: {_score(res['raw'])} logged → {_score(res['rescored'])} rescored, "
          f"{len(record['findings'])} finding(s)")
    for p in paths:
        print(f"  wrote {p}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
