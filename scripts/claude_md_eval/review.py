#!/usr/bin/env python3
"""CLAUDE.md eval — the owner's review queue, in the terminal.

Design: docs/todo/CLAUDE_MD_EVAL_SUPERVISOR_PLAN.md → Decision 15 and phase 6.

Walks the open items of a run record (owner decisions, proposals, spot checks, the loop review)
one at a time, one key per answer. Every answer is an append-only event in
`~/.cecelia-effectiveness/review.jsonl`; an undo is a correcting event, never an edit. Nothing here
touches the repo or the record: the supervisor applies the events at the start of the next pass.

Usage:
    pixi run recital-review                # the newest record
    pixi run recital-review --date 2026-09-30
"""
from __future__ import annotations

import argparse
import datetime as _dt
import importlib.util as _importlib_util
import json
import os
import pathlib
import random
import sys
import typing as _t
import uuid

_REPO = pathlib.Path(__file__).resolve().parents[2]

sys.path.insert(0, str(_REPO / "python"))
from cecelia.effectiveness import read_events  # noqa: E402
from cecelia.effectiveness.palette import BLUISH_GREEN, GREY, ORANGE, SKY_BLUE, YELLOW  # noqa: E402


def _load_sibling(name: str):
    spec = _importlib_util.spec_from_file_location(f"_ce_{name}", pathlib.Path(__file__).parent / f"{name}.py")
    mod = _importlib_util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


_record = _load_sibling("record")

REVIEW_SCHEMA_VERSION = 1

#: queue item kind → (event name, {key: value}). The key is what the owner presses.
ANSWERS: dict[str, tuple[str, dict[str, str]]] = {
    "decision": ("finding_status", {"r": "resolved", "d": "dropped", "o": "open"}),
    "proposal": ("proposal_decision", {"a": "accept", "r": "reject", "d": "defer"}),
    "spot_check": ("spot_check_label", {"r": "real", "f": "false", "u": "unclear"}),
    "loop_review": ("loop_review_decision", {"c": "continue", "t": "retune", "s": "stop"}),
}
REVIEW_EVENTS = tuple(e for e, _ in ANSWERS.values())
_CONTROL = {"n": "skip", "z": "undo", "q": "quit"}


def review_path() -> pathlib.Path:
    """`review.jsonl` beside the record store and the effectiveness log."""
    return _record.store_root().parent / "review.jsonl"


def append_review(event: str, record_date: str, ref: str, value: str | None, *,
                  note: str | None = None, corrects: str | None = None,
                  path: pathlib.Path | None = None) -> dict:
    """Append one owner event. `value=None` with `corrects` is an undo."""
    if event not in REVIEW_EVENTS:
        raise ValueError(f"unknown review event {event!r}")
    row = {"schema_version": REVIEW_SCHEMA_VERSION, "id": uuid.uuid4().hex[:12],
           "ts": _dt.datetime.now(_dt.timezone.utc).replace(microsecond=0).isoformat().replace("+00:00", "Z"),
           "event": event, "record": record_date, "ref": ref, "value": value}
    if note:
        row["note"] = note
    if corrects:
        row["corrects"] = corrects
    path = path or review_path()
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("a", encoding="utf-8") as fh:
        fh.write(json.dumps(row, ensure_ascii=False) + "\n")
        fh.flush()
        os.fsync(fh.fileno())
    return row


def read_reviews(path: pathlib.Path | None = None) -> list[dict]:
    """Every owner event, oldest first. Torn lines are skipped by `read_events`; rows of another
    schema or event are skipped here."""
    return [row for row in read_events(path or review_path())
            if row.get("schema_version") == REVIEW_SCHEMA_VERSION and row.get("event") in REVIEW_EVENTS]


def applied_pass_records(before: str | None = None) -> list[dict]:
    """`record.pass_records` with the owner's answers folded in: the view every reader of the
    history wants (delta, recurrence, curation)."""
    reviews = read_reviews()
    return [apply_reviews(r, reviews) for r in _record.pass_records(before=before)]


def current(reviews: _t.Iterable[dict]) -> dict[tuple[str, str, str], dict]:
    """The latest event per (record, event, ref): later events, undos included, win."""
    out: dict[tuple[str, str, str], dict] = {}
    for r in reviews:
        out[(r["record"], r["event"], r["ref"])] = r
    return out


def apply_reviews(record: dict, reviews: _t.Iterable[dict]) -> dict:
    """`record` with the owner's answers folded in: finding status, proposal decisions, spot-check
    labels, loop decision. What the supervisor does to the previous record at the start of a pass."""
    now = current(reviews)
    date = record["date"]

    def answer(event: str, ref: str) -> dict | None:
        row = now.get((date, event, ref))
        return row if row and row.get("value") is not None else None

    record = json.loads(json.dumps(record))   # deep copy; the caller's record is left alone
    for f in record.get("findings", []):
        row = answer("finding_status", f["id"])
        if row and row["value"] in _record.FINDING_STATUSES:
            f["status"] = row["value"]
            if row.get("note"):
                f["owner_note"] = row["note"]
    for p in record.get("proposals", []):
        row = answer("proposal_decision", p["id"])
        if row:
            p["decision"] = row["value"]
    tracking = record.setdefault("tracking", {})
    labels = {ref: row["value"] for (d, e, ref), row in now.items()
              if d == date and e == "spot_check_label" and row.get("value") is not None}
    if labels:
        tracking["spot_check_labels"] = labels
    row = answer("loop_review_decision", "loop")
    if row and tracking.get("loop_review"):
        tracking["loop_review"]["decision"] = row["value"]
    return record


def pending(record: dict, reviews: _t.Iterable[dict]) -> list[dict]:
    """The record's queue items with no live answer yet, in queue order."""
    now = current(reviews)
    out = []
    for item in record.get("queue", []):
        event = ANSWERS[item["kind"]][0]
        row = now.get((record["date"], event, item["ref"]))
        if row is None or row.get("value") is None:
            out.append(item)
    return out


# ── what the supervisor adds to a record ────────────────────────────────────────────────────────

SPOT_EVERY, SPOT_MIN, SPOT_MAX = 4, 3, 5
LOOP_EVERY = 8


def spot_check_sample(findings: _t.Sequence[dict], *, run_number: int, seed: str) -> list[str]:
    """On every 4th supervised run, 3–5 findings for the owner to label (never the supervisor).

    Seeded by the record date, so a rerun of the same pass samples the same findings.
    """
    if run_number % SPOT_EVERY or not findings:
        return []
    rng = random.Random(seed)
    ids = [f["id"] for f in findings]
    return sorted(rng.sample(ids, min(len(ids), rng.randint(SPOT_MIN, SPOT_MAX))))


def loop_review(record: dict, history: _t.Sequence[dict], *, run_number: int,
                reviews: _t.Iterable[dict]) -> dict | None:
    """On every 8th supervised run, the numbers for the owner's continue / retune / stop call."""
    if run_number % LOOP_EVERY:
        return None
    records = [*history, record]
    answers = current(reviews)
    accepted = sum(1 for (_, e, _), row in answers.items() if e == "proposal_decision" and row.get("value") == "accept")
    labels = [row["value"] for (_, e, _), row in answers.items()
              if e == "spot_check_label" and row.get("value") in ("real", "false")]
    raw = lambda r: r["results"]["raw"]   # noqa: E731
    return {
        "runs": run_number,
        "scores": [f"{r['date']} {raw(r)['compliant']}/{raw(r)['total']}" for r in records],
        "proposals": sum(len(r.get("proposals", [])) for r in history),
        "accepted": accepted,
        "false_positive_rate": f"{labels.count('false')}/{len(labels)}" if labels else "n/a",
        "cost_usd": round(sum((r["run"].get("cost_usd") or 0) + ((r["run"].get("supervisor") or {}).get("cost_usd") or 0)
                              for r in records), 2),
    }


# ── the terminal queue ─────────────────────────────────────────────────────────────────────────

def _col(code: str, text: str, use_colour: bool) -> str:
    return f"{code}{text}\033[0m" if use_colour else text


def describe(record: dict, item: dict) -> list[str]:
    """What the owner needs to answer this one item, drawn from the record."""
    findings = {f["id"]: f for f in record.get("findings", [])}
    proposals = {p["id"]: p for p in record.get("proposals", [])}
    if item["kind"] in ("decision", "spot_check"):
        f = findings[item["ref"]]
        lines = [f"{f['id']} · {f.get('slug') or '—'} · {f['class']} · {f.get('recurrence', '')}",
                 f.get("title") or "", f["diagnosis"], f"Proposed fix: {f['proposed_fix']}"]
        if f.get("evidence"):
            ev = f["evidence"][0]
            lines += [f"Evidence: {ev['trace']}", *("  " + ln for ln in ev["excerpt"].splitlines()[:6])]
        if item["kind"] == "spot_check":
            lines.append("Is this finding real? Label it; the supervisor never does.")
        return [ln for ln in lines if ln]
    if item["kind"] == "proposal":
        p = proposals[item["ref"]]
        return [ln for ln in (f"{p['id']} · {p['kind']}", p["summary"],
                              f"Sources: {', '.join(p.get('sources', [])) or '—'}",
                              f"Hypothesis: {p['hypothesis']}" if p.get("hypothesis") else "") if ln]
    loop = record.get("tracking", {}).get("loop_review", {})
    return [f"Loop review after {loop.get('runs')} supervised runs:",
            f"  scores: {' → '.join(loop.get('scores', []))}",
            f"  proposals accepted: {loop.get('accepted', 0)} of {loop.get('proposals', 0)}",
            f"  spot-check false positives: {loop.get('false_positive_rate', 'n/a')}",
            f"  cost: ${loop.get('cost_usd', 0):.2f}",
            "Continue, retune, or stop the loop?"]


def run_queue(record: dict, *, read: _t.Callable[[str], str] = input, out: _t.TextIO = sys.stdout,
              path: pathlib.Path | None = None, use_colour: bool = True) -> int:
    """Walk the pending items; returns how many answers were recorded (net of undos)."""
    reviews = read_reviews(path)
    items = pending(record, reviews)
    total = len(record.get("queue", []))
    if not items:
        print(_col(BLUISH_GREEN, f"Nothing to review for {record['date']}: all {total} item(s) answered.",
                   use_colour), file=out)
        return 0
    done: list[tuple[dict, dict]] = []   # (item, event) answered this session, for undo
    i = 0
    while i < len(items):
        item = items[i]
        event, keys = ANSWERS[item["kind"]]
        answered = total - len(items) + len(done)
        print("", file=out)
        print(_col(SKY_BLUE, f"[{answered + 1}/{total}] {item['kind'].replace('_', ' ')} {item['ref']}",
                   use_colour), file=out)
        for ln in describe(record, item):
            print(f"  {ln}", file=out)
        opts = "  ".join(f"[{k}] {v}" for k, v in {**keys, **_CONTROL}.items())
        try:
            key = read(_col(GREY, opts + " > ", use_colour)).strip().lower()[:1]
        except EOFError:
            key = "q"
        if key == "q":
            break
        if key == "n":
            i += 1
            continue
        if key == "z":
            if not done:
                print(_col(YELLOW, "  nothing to undo in this session", use_colour), file=out)
                continue
            prev_item, prev = done.pop()
            append_review(prev["event"], prev["record"], prev["ref"], None, corrects=prev["id"], path=path)
            print(_col(ORANGE, f"  undid {prev['event']} {prev['ref']} = {prev['value']}", use_colour), file=out)
            i = items.index(prev_item)   # ask that one again
            continue
        if key not in keys:
            print(_col(YELLOW, f"  press one of: {', '.join([*keys, *_CONTROL])}", use_colour), file=out)
            continue
        note = None
        if item["kind"] == "decision" and keys[key] == "resolved":
            note = read("  your answer, for the record (enter to skip) > ").strip() or None
        row = append_review(event, record["date"], item["ref"], keys[key], note=note, path=path)
        done.append((item, row))
        print(_col(BLUISH_GREEN, f"  {item['ref']} = {keys[key]}", use_colour), file=out)
        i += 1
    left = len(pending(record, read_reviews(path)))
    print(_col(GREY, f"\n{total - left}/{total} answered for {record['date']}"
                     + (f"; {left} left" if left else ""), use_colour), file=out)
    return len(done)


def main(argv: list[str] | None = None) -> int:
    ap = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    ap.add_argument("--date", help="record to review (default: the newest pass record)")
    args = ap.parse_args(argv)
    try:
        if args.date:
            record = apply_reviews(_record.load(_record.store_root() / f"{args.date}.json"), read_reviews())
        else:
            records = applied_pass_records()
            if not records:
                print(f"recital-review: no run records in {_record.store_root()}", file=sys.stderr)
                return 1
            record = records[-1]
    except _record.RecordError as e:
        print(f"recital-review: {e}", file=sys.stderr)
        return 1
    if record.get("kind") == "failure":
        print(f"recital-review: {record['date']} is a failure record; nothing to review", file=sys.stderr)
        return 1
    run_queue(record, use_colour=sys.stdout.isatty())
    return 0


if __name__ == "__main__":
    sys.exit(main())
