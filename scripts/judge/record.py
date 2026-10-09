#!/usr/bin/env python3
"""Weekly judge — run records: one JSON per pass, rendered to markdown.

A record is what a fresh session acts on cold: the bugs nobody fixed, checked against the pinned
SHA; how often each rule was broken; and what to tighten. The JSON is the source; the markdown is
rendered from it and never edited. Records live in `~/.cecelia-effectiveness/judge-runs/` (beside
`events.jsonl`, so they follow `CECELIA_EFFECTIVENESS_LOG` in tests). The pass's PR mirrors them to
`docs/ai-assist/judge-runs/`.

Usage:
    pixi run judge-record 2026-10-06 [--mirror]     # re-render a stored record
"""
from __future__ import annotations

import argparse
import json
import pathlib
import sys
import typing as _t

_REPO = pathlib.Path(__file__).resolve().parents[2]
MIRROR_REL = pathlib.PurePosixPath("docs/ai-assist/judge-runs")   # in any checkout of the repo
_MIRROR_DIR = _REPO / MIRROR_REL

sys.path.insert(0, str(_REPO / "python"))
from cecelia.effectiveness.judge_staleness import backlog, judge_store  # noqa: E402
from cecelia.utils.atomic_io import write_atomic, write_json_atomic  # noqa: E402

SCHEMA_VERSION = 1
PROPOSAL_KINDS = ("tighten", "ratchet")
BUG_STATUSES = ("open", "unjudged", "unmerged", "parked", "gone", "dismissed", "wont_fix")

_HOW_TO_USE = (
    "Point a session at this file. To fix bugs, work the `open` ones under *Bugs*. To tighten a "
    "rule, take a proposal under *Rules*. Generated from `{json}`; edit the JSON, never this page.")
_BUGS_HOW_TO = (
    "Possible bugs in shipped code, from fanout findings nobody fixed, checked against the pinned SHA. "
    "To work them: for each `open` bug, read the code at `file:line` on `origin/main`, confirm it, "
    "and fix it on a normal branch (recital, PR), naming the bug's key in the commit message. "
    "A bug an agent verified says how: `fix` is live, `decide` waits for the owner's answer; an *Owner's "
    "answer* overrides the agent's recommendation. A `guard` bug can't happen yet: it is `parked`, off the "
    "work list, and goes back to verify when a commit touches its file (add the guard or test its *Live once* "
    "names to close it sooner). "
    "The next pass checks each one again and marks the fixed ones `gone`. "
    "`pixi run judge-review` walks these one at a time and can open a briefed fix session for each, "
    "or answer `wont_fix` for a bug that isn't worth fixing. "
    "A `stranded` bug is commits pushed to a PR's branch after it merged: land them in a new PR. "
    "An *agent run* bug is an error an autonomous run hit; with no `file:line`, start from the tool and the error. "
    "A *repeated agent error* is a 4xx with a reason that agents hit in several runs: the input was wrong each "
    "time, so the fix is the guidance (the tool's description, the MCP guidance, the error message), not the API. "
    "A *run review* is a section of an agent run's record a person marked bad with cause `guide` (the guide didn't "
    "say it) or `platform` (the agent couldn't see it): fix the guide or the surface the note names. "
    "*Waiting for the judge* lists candidates nobody has checked yet: not work until a pass judges them.")


def parked_why(v: dict) -> str:
    """A parked bug's `why`, from its `guard` verdict."""
    return (f"verified guard ({v.get('date') or '?'}): can't happen yet"
            + (f"; live once {v['trigger']}" if v.get("trigger") else ""))


def backlog_line(record: dict) -> str:
    """`N waiting for the judge (last pass M)`; the trend is the point, so it's said even at 0."""
    last = record["run"].get("backlog_last")
    return f"{backlog(record['bugs'])} waiting for the judge" + (f" (last pass {last})" if last is not None else "")
def verify_waiting(spend: dict) -> str | None:
    """`N waiting for verify (over the $X verify cap)`: open bugs verify's cap held back this pass,
    unverified and so unfiled; None when there are none."""
    v = spend.get("verify") or {}
    if not v.get("waiting"):
        return None
    return f"{v['waiting']} waiting for verify (over the " + (f"${v['cap_usd']:g} " if v.get("cap_usd") else "") + "verify cap)"


_RULES_HOW_TO = (
    "Reviewer findings in the last {days} days, mapped to the CLAUDE.md rule each one breaks. "
    "*Agent's code* is a mistake in the reviewed diff; *older code* was there before it. A proposal "
    "needs {n} different sessions: `tighten` (agents keep missing the rule: reword it, or make it a "
    "mechanical check) or `ratchet` (old code keeps the shape alive: a test that bans it).")


class RecordError(ValueError):
    pass


def store_root() -> pathlib.Path:
    return judge_store()


def owner_bug(bug: dict, date: str) -> bool:
    """An open bug the owner has to answer: verified `decide` on `date`."""
    v = bug.get("verify") or {}
    return bug.get("status") == "open" and v.get("verdict") == "decide" and v.get("date") == date


def newly_open(bug: dict, date: str) -> bool:
    """An `open` bug new to the work list: it became open on `date` (a carried `unjudged`/`unmerged`
    one judged live now counts). The PR body's "N new"."""
    return bug.get("status") == "open" and (bug.get("opened") or bug.get("first_seen")) == date


def build(date: str, *, ts: str, sha: str, bugs: _t.Sequence[dict], rules: _t.Sequence[dict],
          proposals: _t.Sequence[dict], spend: dict, agent_causes: _t.Sequence[dict] = ()) -> dict:
    """The record for the pass on `date`. `ts` is when the pass started: the next pass's sweep
    window opens there. `agent_causes`: run reviews' `agent` causes per guide (`run_reviews.py`)."""
    return {
        "schema_version": SCHEMA_VERSION, "kind": "pass", "date": date,
        "run": {"ts": ts, "sha": sha, "spend": spend},
        "bugs": list(bugs), "rules": list(rules), "proposals": list(proposals),
        # what the owner has to answer: only a bug an agent verified as `decide` this pass
        "queue": [{"kind": "bug", "ref": b["id"]} for b in bugs if owner_bug(b, date)],
        **({"agent_causes": list(agent_causes)} if agent_causes else {}),
    }


def failure_record(date: str, *, stage: str, error: str, sha: str | None = None) -> dict:
    """The minimal record a crashed pass still leaves, so the gap is visible."""
    return {"schema_version": SCHEMA_VERSION, "kind": "failure", "date": date,
            "run": {"stage": stage, "error": error, "sha": sha}}


_REQUIRED = {
    "": ("schema_version", "kind", "date", "run", "bugs", "rules", "proposals", "queue"),
    "run": ("ts", "sha", "spend"),
    "proposal": ("id", "kind", "rule", "summary", "sources"),
    "bug": ("id", "key", "status", "file", "line", "desc", "why", "first_seen"),
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
        need(record, "", ("date", "run"))
        need(record.get("run", {}), "run", _REQUIRED["failure_run"])
        return errs
    need(record, "", _REQUIRED[""])
    need(record.get("run", {}), "run", _REQUIRED["run"])
    for p in record.get("proposals", []):
        need(p, f"proposal {p.get('id', '?')}", _REQUIRED["proposal"])
        if p.get("kind") not in PROPOSAL_KINDS:
            errs.append(f"proposal {p.get('id', '?')}: kind {p.get('kind')!r} not in {PROPOSAL_KINDS}")
    ids = {b.get("id") for b in record.get("bugs", [])}
    for b in record.get("bugs", []):
        if b.get("kind") != "stranded":   # a stranded commit has a PR, not a file:line
            need(b, f"bug {b.get('id', '?')}", _REQUIRED["bug"])
        if b.get("status") not in BUG_STATUSES:
            errs.append(f"bug {b.get('id', '?')}: status {b.get('status')!r} not in {BUG_STATUSES}")
    for item in record.get("queue", []):
        if item.get("kind") != "bug" or item.get("ref") not in ids:
            errs.append(f"queue: {item!r} names no bug")
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


def pass_records(before: str | None = None) -> list[dict]:
    """Every valid pass record in the store, oldest first; only those dated before `before` if given.
    Failure records and anything that doesn't validate are skipped."""
    out = []
    for path in sorted(store_root().glob("*.json")):
        if before is not None and path.stem >= before:
            continue
        try:
            record = load(path)
        except RecordError:
            continue
        if record.get("kind") == "pass":
            out.append(record)
    return out


def spend_line(spend: dict) -> str:
    return (f"${spend.get('total_usd', 0):.2f} · bug sweep ${spend.get('sweep_usd', 0):.2f} · "
            f"verify ${spend.get('verify_usd', 0):.2f} · rules ${spend.get('rules_usd', 0):.2f}")


def _count(n: int) -> str:
    return f"{n / 1e6:.2f}M" if n >= 1e6 else f"{n / 1e3:.1f}k" if n >= 1e3 else str(n)


def tokens_line(spend: dict) -> str:
    """The pass's tokens, total then per step. Records from before token counting say so."""
    tok = spend.get("tokens")
    if not tok:
        return "not recorded"

    def one(t: dict) -> str:
        return (f"{_count(t.get('output', 0))} out · {_count(t.get('input', 0))} in "
                f"(+ cache read {_count(t.get('cache_read', 0))}, cache write {_count(t.get('cache_write', 0))})")
    steps = " · ".join(f"{k} {_count(sum(v.values()))}" for k, v in tok.items() if k != "total")
    return f"{one(tok.get('total') or {})} — {steps}"


def one_line(text: _t.Any, limit: int = 200) -> str:
    """An error message as one short line: whitespace collapsed, backticks dropped (it goes in code
    spans), cut at `limit`."""
    flat = " ".join(str(text).replace("`", "'").split())
    return flat if len(flat) <= limit else flat[:limit - 1] + "…"


def _cell(text: _t.Any) -> str:
    return str(text if text is not None else "—").replace("|", "\\|").replace("\n", " ")


def landed_hint(b: dict) -> str:
    """A judge or verify prompt's line per landed fix (`fix_landed`): a hint to check, given as data."""
    return "".join(f"(a commit naming this key landed: {c['subject']} ({c['commit'][:8]}))\n"
                   for c in b.get("fix_landed", []))


def _landed(b: dict) -> str:
    """`fix_landed` as markdown: `abcd1234` #1430 per commit."""
    return ", ".join(f"`{c['commit'][:8]}`" + (f" #{c['pr']}" if c.get("pr") else "") for c in b.get("fix_landed", []))


#: A landed fix is confirmed once the bug is one of these.
_CONFIRMED = ("gone", "dismissed")


def landed_counts(bugs: _t.Sequence[dict]) -> tuple[int, int]:
    """(bugs a fix landed for since the last pass, of those confirmed fixed: the judge said `gone` or
    verify dismissed it)."""
    landed = [b for b in bugs if b.get("fix_landed")]
    return len(landed), sum(b["status"] in _CONFIRMED for b in landed)


def bug_location(b: dict) -> str:
    """Where a bug is, as plain text: `file:line`, the PR of stranded commits, or the tool an agent
    run's error came from when there was no stacktrace."""
    if b.get("kind") == "stranded":
        return f"PR #{b.get('pr')}"
    if b.get("review"):
        return f"run review · {b.get('cause') or '?'} · {b.get('section') or '?'}"
    if not b.get("file"):
        return f"{'repeated agent error' if b.get('repeat') else 'agent run'} · {b.get('tool') or '?'}"
    return f"{b['file']}:{b['line']}"


def _bug_where(b: dict) -> str:
    if b.get("kind") == "stranded":
        return f"stranded · PR #{b.get('pr')} `{b.get('branch')}`"
    return f"`{bug_location(b)}`"


def _render_bugs(bugs: _t.Sequence[dict]) -> list[str]:
    if not bugs:
        return ["None."]
    counts = {s: sum(b["status"] == s for b in bugs) for s in BUG_STATUSES}
    out = [_BUGS_HOW_TO, "",
           " · ".join(f"{n} {s.replace('_', ' ')}" for s, n in counts.items() if n), ""]
    landed, confirmed = landed_counts(bugs)
    if landed:
        out += [f"A fix landed for {landed} bug(s) since the last pass (a commit names the bug's key): "
                f"{confirmed} confirmed fixed, {landed - confirmed} awaiting re-check.", ""]
    for b in (b for b in bugs if b["status"] not in ("unjudged", "parked") and not b.get("muted")):
        v = b.get("verify") or {}
        out += [f"### {b['id']} · {b['status']}{' · ' + v['verdict'] if v else ''} · {_bug_where(b)} · `{b['key']}`", "",
                f"**Check:** {b['why']}", ""]
        if b.get("fix_landed"):
            out += [f"**Fix landed:** {_landed(b)} — "
                    + ("confirmed fixed" if b["status"] in _CONFIRMED else "awaiting re-check"), ""]
        if b.get("owner_answer"):
            out += [f"**Owner's answer** (follow this, not the recommendation): {b['owner_answer']}", ""]
        if v:
            out += [f"**Verified** ({v['verdict']}, {v.get('date')}): {v.get('effect', '')}",
                    *([f"- Question: {v['question']}"] if v.get("question") else []),
                    *([f"- Recommendation: {v['recommendation']}"] if v.get("recommendation") else []),
                    *([f"- Live once: {v['trigger']}"] if v.get("trigger") else []),
                    f"- Evidence: {v.get('evidence', '')}", ""]
        if b.get("kind") == "stranded":
            out += [f"**Commits:** {', '.join(f'`{c}`' for c in b.get('commits', []))}", ""]
            continue
        if b.get("review"):
            out += [f"**Run review** (cause `{b.get('cause')}`, run {b.get('run') or '?'}, entry `{b.get('entry')}` in "
                    f"`{b.get('project')}`" + (f", guide `{b['guide']}`" if b.get("guide") else "")
                    + f", first seen {b['first_seen']}): {b['desc']}", ""]
        elif b.get("kind") == "agent_run":
            out += [f"**{'Repeated error' if b.get('repeat') else 'Error'}** (`{b.get('tool') or '?'}`, hit in {b.get('runs') or 1} run(s), first seen "
                    f"{b['first_seen']}, last {(b.get('last_seen') or '?')[:10]}): {b['desc']}", ""]
        else:
            out += [f"**Finding** ({b.get('marker') or '?'}, branch `{b.get('branch') or '?'}`, "
                    f"first seen {b['first_seen']}): {b['desc']}", ""]
        out += [f"**Also raised** (`{a['key']}`, branch `{a.get('branch') or '?'}`): {a['desc']}"
                for a in b.get("also", [])]
        out += [""] if b.get("also") else []
    muted = [b for b in bugs if b.get("muted")]
    if muted:
        out += ["### Closed agent-run errors", "", "Carried so a run that hits one again doesn't raise it as new; "
                "a dismissed repeated error re-opens once its run count doubles.", ""]
        out += [f"- {b['id']} · {b['status'].replace('_', ' ')} · {_bug_where(b)} · `{b['key']}` — "
                f"hit in {b.get('runs') or 1} run(s), last {(b.get('last_seen') or '?')[:10]}" for b in muted]
        out.append("")
    parked = [b for b in bugs if b["status"] == "parked"]
    if parked:
        out += ["### Parked", "", "Verified `guard`: the flaw is there, but nothing reachable triggers it. "
                "Each goes back to verify when a commit touches its file or names its key, a few a pass; "
                "one marked *re-check due* waits for the next slot.", ""]
        out += [f"- {b['id']} · {_bug_where(b)} · `{b['key']}` — {(b.get('verify') or {}).get('effect') or b['desc']}"
                + (f" Live once: {b['verify']['trigger']}" if (b.get("verify") or {}).get("trigger") else "")
                + (f" · *re-check due:* {b['recheck']}" if b.get("recheck") else "")
                for b in parked]
        out.append("")
    waiting = [b for b in bugs if b["status"] == "unjudged"]
    if waiting:
        out += ["### Waiting for the judge", ""]
        out += [f"- {b['id']} · {_bug_where(b)} · `{b['key']}` — {b['why']}"
                + (f" · fix landed {_landed(b)}" if b.get("fix_landed") else "") for b in waiting]
        out.append("")
    return out


def _render_rules(record: dict) -> list[str]:
    rules = record.get("rules", [])
    failed = (record["run"].get("failed") or {}).get("rules")
    if failed:
        return [f"The rule-mapping judge failed this pass (`{one_line(failed)}`): nothing was tallied, "
                "so no proposals. The next pass maps the same window."]
    if not rules:
        return ["No reviewer findings mapped to a rule."]
    window, need = record["run"].get("rules_window_days", "?"), record["run"].get("min_sessions", "?")
    out = [_RULES_HOW_TO.format(days=window, n=need), "",
           "| Rule | Findings | Sessions | Agent's code | Older code |", "|---|---|---|---|---|"]
    out += [f"| {_cell(r['rule'])} | {r['findings']} | {r['sessions']} | {r['agent_made']} | {r['legacy']} |"
            for r in rules]
    out.append("")
    for p in record.get("proposals", []):
        out += [f"- **{p['id']} · {p['kind']}** — {p['summary']}",
                f"  - Sources: {', '.join(f'`{s}`' for s in p['sources'])}"]
    if not record.get("proposals"):
        out.append(f"No rule reached {need} sessions.")
    return out


_AGENT_CAUSES_HOW_TO = (
    "Run review sections a person marked bad with cause `agent`: the guide and tools were enough. They stay "
    "on the record; a guide whose notes span 2+ runs may share one gap — compare the notes, and if they "
    "describe the same mistake, the guide needs the step.")


def _render_agent_causes(record: dict) -> list[str]:
    out = [_AGENT_CAUSES_HOW_TO, ""]
    for g in record.get("agent_causes", []):
        name = f"`{g['guide']}`" if g.get("guide") else "no guide named"
        out += [f"### {name} — {g['runs']} run(s){' · possible guide gap' if g.get('gap') else ''}", ""]
        out += [f"- run {n.get('run') or n['entry']} · {n['section']}"
                + (f" · {n['heading']}" if n.get("heading") else "") + f": {n['note']}" for n in g["notes"]]
        out.append("")
    return out


def render_markdown(record: dict) -> str:
    if record.get("kind") == "failure":
        run = record["run"]
        return (f"# Weekly judge — {record['date']} — FAILED\n\n"
                f"The pass stopped at **{run['stage']}**.\n\n"
                f"- Pinned SHA: {run['sha'] or 'not pinned yet'}\n"
                f"- Error: `{_cell(run['error'])}`\n\n"
                "Next action: read the cron log in `~/.cecelia-effectiveness/cron/`, fix the cause, "
                "and rerun `pixi run judge-weekly`.\n")
    run = record["run"]
    out = [f"# Weekly judge — {record['date']}", "",
           _HOW_TO_USE.format(json=f"{record['date']}.json"), "",
           "| | |", "|---|---|",
           f"| Pinned SHA | `{run['sha']}` |",
           f"| Tokens | {tokens_line(run['spend'])} |",
           f"| Spend (list price) | {spend_line(run['spend'])} |",
           f"| Owner queue | {len(record['queue'])} (`pixi run judge-review`) |",
           f"| Backlog | {backlog_line(record)} |",
           *([f"| Verify | {verify_waiting(run['spend'])} |"] if verify_waiting(run["spend"]) else []),
           *(f"| Failed | {step}: `{_cell(one_line(why))}` |" for step, why in (run.get("failed") or {}).items()),
           "",
           "## Bugs", "",
           *([f"The bug sweep's judge failed, so nothing was re-checked: open bugs stay open (marked "
              "*still open*), new findings wait for the next pass, and none can be marked fixed.", ""]
             if (run.get("failed") or {}).get("sweep") else []),
           *_render_bugs(record["bugs"]), "",
           "## Rules", "", *_render_rules(record)]
    if record.get("agent_causes"):
        out += ["", "## Agent causes per guide", "", *_render_agent_causes(record)]
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
    with write_atomic(dest.with_suffix(".md"), encoding="utf-8") as fh:   # the record to read; the JSON is the source
        fh.write(render_markdown(record))
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
    ap.add_argument("date")
    ap.add_argument("--mirror", action="store_true", help=f"also write to {MIRROR_REL}/")
    args = ap.parse_args(argv)
    try:
        paths = write(load(store_root() / f"{args.date}.json"), mirror=args.mirror, force=True)
    except RecordError as e:
        print(f"judge-record: {e}", file=sys.stderr)
        return 1
    for p in paths:
        print(f"  wrote {p}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
